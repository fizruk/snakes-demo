{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Snakes.Model.Universe where

import Control.Monad
import Control.Monad.Random
import Data.Binary
import Data.List (inits, tails)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import GHC.Generics
import Graphics.Gloss

import Snakes.Config
import Snakes.Model.Effect
import Snakes.Model.Item
import Snakes.Model.Snake

import Network.WebSockets

-- | Player's or bot's name.
type PlayerName = String

-- | The universe of the The Game of Snakes.
data Universe = Universe
  { uSnakes     :: Map PlayerName Snake     -- ^ Player's 'Snake'.
  , uItems      :: [Item]                   -- ^ Active items.
  , uEffects    :: Map PlayerName [Effect]  -- ^ Active bonus effects.
  , uDeadLinks  :: [DeadLink]               -- ^ Dead links on the field, fading away.
  , uColors     :: [Color]                  -- ^ Available colors for new players.
  } deriving (Generic)

instance Binary Universe

-- | An empty universe.
emptyUniverse :: Universe
emptyUniverse = Universe
  { uSnakes     = Map.empty
  , uItems      = []
  , uEffects    = Map.empty
  , uDeadLinks  = []
  , uColors     = playerColors }

-- | Spawn a new player in the 'Universe'.
spawnPlayer :: MonadRandom m => PlayerName -> Universe -> m Universe
spawnPlayer = respawnSnake

-- | Remove player from the 'Universe'.
kickPlayer :: PlayerName -> Universe -> Universe
kickPlayer name u@Universe{..} = u
  { uSnakes  = Map.delete name uSnakes
  , uEffects = Map.delete name uEffects
  , uColors  = uColors ++ map snakeColor (maybeToList (Map.lookup name uSnakes)) }

-- | Update universe for each frame.
updateUniverse :: MonadRandom m => Float -> Universe -> m Universe
updateUniverse dt
    = updateUniverseObjects dt
  >=> (return . checkItemCollision)
  >=> checkSnakeCollision

-- * Helpers

-- | Update every object in the universe.
updateUniverseObjects :: MonadRandom m => Float -> Universe -> m Universe
updateUniverseObjects dt u@Universe{..} = do
  items <- randomItems
  return u
    { uSnakes     = Map.map (moveSnake dt) uSnakes
    , uItems      = newItems <> take (itemActiveTotal - length newItems) items
    , uEffects    = Map.map (mapMaybe (updateEffect dt)) uEffects
    , uDeadLinks  = mapMaybe (updateDeadLink dt) uDeadLinks }
  where
    newItems = mapMaybe (updateItem dt) uItems

-- | Check if a 'Snake' eats an 'Item'.
-- If yes — apply its effect.
checkItemCollision :: Universe -> Universe
checkItemCollision u@Universe{..} = foldr (.) id effects u { uItems = leftovers }
  where
    eItems = map eaters uItems
    eaters item = (item, listToMaybe (Map.keys (Map.filter (collidesWithItem item) uSnakes)))
    effect (item, name) = applyEffect (itemEffect item) name

    effects   = map effect (mapMaybe sequenceA eItems)
    leftovers = map fst (filter (isNothing . snd) eItems)

-- | Check if a 'Snake' collides with an 'Item'.
collidesWithItem :: Item -> Snake -> Bool
collidesWithItem Item{..} Snake{..}
  = (itemLocation, effectItemSize itemEffect) `collides` (head snakeLinks, snakeLinkSize)

-- | Check if any 'Snake's collide with other 'Snake's or themselves.
checkSnakeCollision :: MonadRandom m => Universe -> m Universe
checkSnakeCollision u@Universe{..} = respawnSnakes dead u
  where
    phantoms  = Map.keys (Map.filter (any ((== EffectPhantom) . effectType)) uEffects)
    snakes    = Map.toList (Map.filterWithKey (\k _ -> k `notElem` phantoms) uSnakes)
    dead      = map (fst . fst) (filter namedSnakesCollision (splits snakes))

    namedSnakesCollision (s, ss) = snakesCollision (map snd ss) (snd s)
    splits xs = zip xs (zipWith (++) (inits xs) (tail (tails xs)))

-- | Respawn 'Snake's of listed players and leave 'DeadLink's where dead bodies are (if any).
respawnSnakes :: MonadRandom m => [PlayerName] -> Universe -> m Universe
respawnSnakes names u = foldM (flip respawnSnake) u names

-- | Respawn 'Snake' of a player and leave 'DeadLink's where dead body is (if any).
respawnSnake :: MonadRandom m => PlayerName -> Universe -> m Universe
respawnSnake name u@Universe{..} = do
  spawn <- randomSpawn
  let newSnake = spawnSnake spawn col
  return u
    { uSnakes     = Map.insert name newSnake uSnakes
    , uEffects    = Map.unionWith (<>) newEffects uEffects
    , uDeadLinks  = newDeadLinks <> uDeadLinks
    , uColors     = cols }
  where
    oldSnake = Map.lookup name uSnakes
    col:cols = maybeToList (snakeColor <$> oldSnake) ++ uColors
    newDeadLinks = foldMap destroySnake oldSnake
    newEffects
      | null newDeadLinks = Map.empty
      | otherwise         = Map.singleton name respawnEffects

-- | Check snake collision with itself or other snakes.
snakesCollision :: [Snake] -> Snake -> Bool
snakesCollision snakes snake
  = selfCollision snake { snakeLinks = concatMap snakeLinks (snake : snakes) }

-- | Check if snake collides with itself.
-- Collision with second link does not count since it is attached to the head.
selfCollision :: Snake -> Bool
selfCollision Snake{..}
  = any (collides (head snakeLinks, snakeLinkSize)) (map (, snakeLinkSize) (drop 2 snakeLinks))

-- | Check collision for two objects.
-- Collision counts when objects' are at least halfway into each other.
collides :: (Point, Float) -> (Point, Float) -> Bool
collides ((x, y), a) ((u, v), b) = ((a + b) / 2)^2 > (x - u)^2 + (y - v)^2

-- | Apply item effect.
applyEffect :: EffectType -> PlayerName -> Universe -> Universe
applyEffect EffectFood name u@Universe{..}
  = u { uSnakes = Map.adjust feedSnake name uSnakes }
applyEffect EffectReverse _ u@Universe{..}
  = u { uSnakes = Map.map reverseSnake uSnakes }
applyEffect ty name u@Universe{..}
  = u { uEffects = Map.insertWith (<>) name [mkEffect ty] uEffects }

-- ** Random generators

-- | Generate infinite list of random points within the game field.
randomPoints :: MonadRandom m => m [Point]
randomPoints = do
  xs <- getRandomRs (-w/2, w/2)
  ys <- getRandomRs (-h/2, h/2)
  return (zip xs ys)
  where
    (w, h) = fieldSize - fieldMargin

-- | Generate an infinite list of random 'Item's.
randomItems :: MonadRandom m => m [Item]
randomItems = zipWith mkItem
  <$> randomPoints    -- location
  <*> getRandoms      -- effect type

-- | Generate a single random 'Spawn'.
randomSpawn :: MonadRandom m => m Spawn
randomSpawn = (,)
  <$> fmap head randomPoints  -- location
  <*> fmap head randomPoints  -- direction

-- =====================================
-- WebSockersData instances are needed
-- to send/receive Haskell structures
-- over websockets
-- =====================================

instance WebSocketsData Universe where
  fromLazyByteString = decode
  toLazyByteString   = encode


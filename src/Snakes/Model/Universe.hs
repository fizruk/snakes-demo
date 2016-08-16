{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Snakes.Model.Universe where

import Control.Applicative
import Control.Monad
import Data.Traversable
import Data.List (find, inits, tails)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import System.Random

import Snakes.Config
import Snakes.Model.Item
import Snakes.Model.Snake

-- | Player's or bot's name.
type PlayerName = String

-- | The universe of the The Game of Snakes.
data Universe = Universe
  { uSnakes     :: Map PlayerName Snake -- ^ Player's 'Snake'.
  , uItems      :: [Item]               -- ^ Infinite item source, only the first item is active.
  , uEffects    :: [Effect]             -- ^ Active bonus effects.
  , uDeadLinks  :: [DeadLink]           -- ^ Dead links on the field, fading away.
  , uSpawns     :: [(Point, Vector)]    -- ^ Infinite list of spawn locations and directions.
  , uColors     :: [Color]              -- ^ Available colors for new players.
  }

-- | An active effect.
data Effect = Effect
  { effectType    :: ItemEffect       -- ^ Effect type.
  , effectTimeout :: Float            -- ^ Time left.
  , effectPlayer  :: Maybe PlayerName -- ^ Player under effect.
  }

-- | Update effect's timer.
updateEffect :: Float -> Effect -> Maybe Effect
updateEffect dt e@Effect{..}
  | effectTimeout > dt = Just e { effectTimeout = effectTimeout - dt }
  | otherwise = Nothing

randomPoints :: Float -> Float -> IO [Point]
randomPoints w h = do
  xs <- randomRs (-w/2, w/2) <$> newStdGen
  ys <- randomRs (-h/2, h/2) <$> newStdGen
  return (zip xs ys)

-- | Generate a random 'Universe'.
randomUniverse :: IO Universe
randomUniverse = do
  itemLocs  <- randomPoints (w - foodSize)  (h - foodSize)
  itemEffs  <- randoms <$> newStdGen
  dirs      <- randomPoints 1 1
  let spawnLocs = iterate (rotateV phi) (w/5, h/5)
  return emptyUniverse
    { uItems  = zipWith mkItem itemLocs itemEffs
    , uSpawns = zip spawnLocs dirs }
  where
    (w, h) = fieldSize
    phi = (1 + sqrt(5)) / 2

-- | An empty universe.
emptyUniverse :: Universe
emptyUniverse = Universe
  { uSnakes     = Map.empty
  , uItems      = []
  , uEffects    = []
  , uDeadLinks  = []
  , uSpawns     = []
  , uColors     = cycle playerColors }

addPlayer :: PlayerName -> Universe -> Universe
addPlayer name u@Universe{..} = u
  { uSnakes = Map.insert name (initSnake (head uSpawns) (head uColors)) uSnakes
  , uSpawns = tail uSpawns
  , uColors = tail uColors }

-- | Update universe for each frame.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt
  = checkSnakeCollision
  . checkItemCollision
  . updateUniverseObjects dt

-- | Update every object in the universe.
updateUniverseObjects :: Float -> Universe -> Universe
updateUniverseObjects dt u@Universe{..} = u
  { uSnakes     = Map.map (moveSnake dt) uSnakes
  , uItems      = newItems
  , uEffects    = mapMaybe (updateEffect dt) uEffects
  , uDeadLinks  = mapMaybe (updateDeadLink dt) uDeadLinks }
  where
    newItems = case updateItem dt (head uItems) of
      Nothing -> tail uItems
      Just b  -> b : tail uItems

checkItemCollision :: Universe -> Universe
checkItemCollision u@Universe{..} = case fed of
  Nothing -> u
  Just (name, _) -> applyItemEffect (itemEffect item) name u { uItems = tail uItems }
  where
    item = head uItems
    fed = find (collidesWithItem item . snd) (Map.toList uSnakes)

collidesWithItem :: Item -> Snake -> Bool
collidesWithItem Item{..} Snake{..}
  = (itemLocation, bonusSize) `collides` (head snakeLinks, snakeLinkSize)   -- FIXME: size should depend on effect

-- | Check if any snakes collide.
checkSnakeCollision :: Universe -> Universe
checkSnakeCollision u@Universe{..}
  = respawnSnakes dead (destroySnakes dead u)
  where
    phantoms  = mapMaybe effectPlayer (filter ((== ItemBonusPhantom) . effectType) uEffects)
    snakes    = Map.toList (Map.filterWithKey (\k v -> k `notElem` phantoms) uSnakes)
    dead      = map (fst . fst) (filter namedSnakesCollision (splits snakes))

    namedSnakesCollision (s, ss) = snakesCollision (map snd ss) (snd s)
    splits xs = zip xs (zipWith (++) (inits xs) (tail (tails xs)))

destroySnakes :: [PlayerName] -> Universe -> Universe
destroySnakes names u@Universe{..} = u
  { uDeadLinks = newDeadLinks <> uDeadLinks }
  where
    snakes = map snd (filter ((`elem` names) . fst) (Map.toList uSnakes))
    newDeadLinks = concatMap destroySnake snakes

respawnSnakes :: [PlayerName] -> Universe -> Universe
respawnSnakes names u@Universe{..} = u
  { uSnakes = newSnakes <> uSnakes
  , uSpawns = drop (length newSnakes) uSpawns
  , uEffects = newEffects <> uEffects }
  where
    respawn name spawn = (name, initSnake spawn (snakeColor (uSnakes Map.! name)))
    newSnakes = Map.fromList (zipWith respawn names uSpawns)
    newEffects = map (Effect ItemBonusPhantom effectPhantomDuration . Just) names

-- | Check snake collision with itself or other snakes.
snakesCollision :: [Snake] -> Snake -> Bool
snakesCollision snakes snake
  = selfCollision snake { snakeLinks = snakeLinks snake ++ otherLinks }
  where
    otherLinks = concatMap snakeLinks snakes

-- | Check if snake collides with itself.
selfCollision :: Snake -> Bool
selfCollision Snake{..}
  = any (collides (head snakeLinks, snakeLinkSize)) (map (, snakeLinkSize) (drop 2 snakeLinks))

-- | Check collision for two objects.
-- Collision counts when objects' are at least halfway into each other.
collides :: (Point, Float) -> (Point, Float) -> Bool
collides ((x, y), a) ((u, v), b) = ((a + b) / 2)^2 > (x - u)^2 + (y - v)^2

-- | Apply item effect.
applyItemEffect :: ItemEffect -> PlayerName -> Universe -> Universe
applyItemEffect ItemFood name u@Universe{..}
  = u { uSnakes = Map.adjust feedSnake name uSnakes }
applyItemEffect ItemBonusReverse _ u@Universe{..}
  = u { uSnakes = Map.map reverseSnake uSnakes }
applyItemEffect ItemBonusPhantom name u@Universe{..}
  = u { uEffects = Effect ItemBonusPhantom effectPhantomDuration (Just name) : uEffects }


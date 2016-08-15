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
import Snakes.Model.Bonus
import Snakes.Model.Food
import Snakes.Model.Snake

-- | Player's or bot's name.
type PlayerName = String

-- | The universe of the The Game of Snakes.
data Universe = Universe
  { uSnakes     :: Map PlayerName Snake -- ^ Player's 'Snake'.
  , uFood       :: [Food]               -- ^ Infinite food source, only first food item is active.
  , uBonuses    :: [Bonus]              -- ^ Infinite bonus source, only first is active.
  , uEffects    :: [Effect]             -- ^ Active bonus effects.
  , uDeadLinks  :: [DeadLink]           -- ^ Dead links on the field, fading away.
  , uSpawns     :: [(Point, Vector)]    -- ^ Infinite list of spawn locations and directions.
  }

-- | An active effect.
data Effect = Effect
  { effectType    :: BonusEffect      -- ^ Effect type.
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
randomUniverse :: GameConfig -> IO Universe
randomUniverse cfg@GameConfig{..} = do
  foodLocs  <- randomPoints (w - foodSize)  (h - foodSize)
  bonusLocs <- randomPoints (w - bonusSize) (h - bonusSize)
  effects   <- randoms <$> newStdGen
  dirs      <- randomPoints 1 1
  let spawns = zip (iterate (rotateV phi) (w/5, h/5)) dirs
  return (initUniverse foodLocs (zip bonusLocs effects) spawns cfg)
  where
    (w, h) = fieldSize
    phi = (1 + sqrt(5)) / 2

-- | Initial universe.
initUniverse :: [Point] -> [(Point, BonusEffect)] -> [(Point, Vector)] -> GameConfig -> Universe
initUniverse ps bs spawns = Universe
  <$> pure Map.empty
  <*> traverse mkFood ps
  <*> traverse (uncurry mkBonus) bs
  <*> pure []
  <*> pure []
  <*> pure spawns

addPlayer :: PlayerName -> Universe -> GameConfig -> Universe
addPlayer name u@Universe{..} cfg = u
  { uSnakes = Map.insert name (initSnake (head uSpawns) cfg) uSnakes
  , uSpawns = tail uSpawns }

-- | Update universe for each frame.
updateUniverse :: Float -> Universe -> GameConfig -> Universe
updateUniverse dt
    = updateUniverseObjects dt
  >=> checkFoodCollision
  >=> checkBonusCollision
  >=> checkSnakeCollision

-- | Update every object in the universe.
updateUniverseObjects :: Float -> Universe -> GameConfig -> Universe
updateUniverseObjects dt u@Universe{..} cfg = u
  { uSnakes = Map.map (flip (moveSnake dt) cfg) uSnakes
  , uFood  = newFood
  , uBonuses = newBonuses
  , uEffects = mapMaybe (updateEffect dt) uEffects
  , uDeadLinks = mapMaybe (flip (updateDeadLink dt) cfg) uDeadLinks }
  where
    newBonuses = case updateBonus dt (head uBonuses) of
      Nothing -> tail uBonuses
      Just b  -> b : tail uBonuses
    newFood = case updateFood dt (head uFood) of
      Nothing -> tail uFood
      Just f  -> f : tail uFood

-- | Check if snake eats food.
checkFoodCollision :: Universe -> GameConfig -> Universe
checkFoodCollision u@Universe{..} cfg = case fed of
  Nothing -> u
  Just (name, _) -> u
    { uFood = tail uFood
    , uSnakes = Map.adjust feedSnake name uSnakes }
  where
    collidesWithFood snake = foodCollision snake (head uFood) cfg
    fed = find (collidesWithFood . snd) (Map.toList uSnakes)

foodCollision :: Snake -> Food -> GameConfig -> Bool
foodCollision Snake{..} Food{..} GameConfig{..}
  = (head snakeLinks, linkSize) `collides` (foodLocation, foodSize)

bonusCollision :: Snake -> Bonus -> GameConfig -> Bool
bonusCollision Snake{..} Bonus{..} GameConfig{..}
  = (head snakeLinks, linkSize) `collides` (bonusLocation, bonusSize)

checkBonusCollision :: Universe -> GameConfig -> Universe
checkBonusCollision u@Universe{..} cfg = case fed of
  Nothing -> u
  Just (name, _) -> applyBonusEffect (bonusEffect bonus) name u { uBonuses = tail uBonuses } cfg
  where
    bonus = head uBonuses
    collidesWithBonus snake = bonusCollision snake bonus cfg
    fed = find (collidesWithBonus . snd) (Map.toList uSnakes)

-- | Check if any snakes collide.
checkSnakeCollision :: Universe -> GameConfig -> Universe
checkSnakeCollision u@Universe{..} cfg@GameConfig{..}
  = u { uSnakes    = newSnakes <> uSnakes
      , uDeadLinks = uDeadLinks <> newDeadLinks
      , uSpawns    = drop (length newSnakes) uSpawns }
  where
    phantoms  = mapMaybe effectPlayer (filter ((== BonusPhantom) . effectType) uEffects)
    snakes    = Map.toList (Map.filterWithKey (\k v -> k `notElem` phantoms) uSnakes)
    dead      = map (fst . fst) (filter namedSnakesCollision (splits snakes))
    newSnakes = Map.fromList (zip dead (map (flip initSnake cfg) uSpawns))
    newDeadLinks = concatMap (flip destroySnake cfg . snd) (filter ((`elem` dead) . fst) snakes)

    namedSnakesCollision (s, ss) = snakesCollision (map snd ss) (snd s) cfg
    splits xs = zip xs (zipWith (++) (inits xs) (tail (tails xs)))

-- | Check snake collision with itself or other snakes.
snakesCollision :: [Snake] -> Snake -> GameConfig -> Bool
snakesCollision snakes snake cfg
  = selfCollision snake { snakeLinks = snakeLinks snake ++ otherLinks } cfg
  where
    otherLinks = concatMap snakeLinks snakes

-- | Check if snake collides with itself.
selfCollision :: Snake -> GameConfig -> Bool
selfCollision Snake{..} GameConfig{..}
  = any (collides (head snakeLinks, linkSize)) (map (, linkSize) (drop 2 snakeLinks))

-- | Check collision for two objects.
-- Collision counts when objects' are at least halfway into each other.
collides :: (Point, Float) -> (Point, Float) -> Bool
collides ((x, y), a) ((u, v), b) = ((a + b) / 2)^2 > (x - u)^2 + (y - v)^2

-- | Apply bonus effect.
applyBonusEffect :: BonusEffect -> PlayerName -> Universe -> GameConfig -> Universe
applyBonusEffect BonusReverse _ u@Universe{..} _
  = u { uSnakes = Map.map reverseSnake uSnakes }
applyBonusEffect BonusPhantom name u@Universe{..} GameConfig{..}
  = u { uEffects = Effect BonusPhantom bonusPhantomDuration (Just name) : uEffects }


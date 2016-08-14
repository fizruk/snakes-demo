{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Snakes.Model.Universe where

import Control.Applicative
import Control.Monad
import Data.Traversable
import Data.Maybe (mapMaybe)
import Graphics.Gloss

import Snakes.Config
import Snakes.Model.Snake
import Snakes.Model.Food

-- | The universe of the The Game of Snakes.
data Universe = Universe
  { uSnake      :: Snake      -- ^ Player's 'Snake'.
  , uFood       :: [Food]     -- ^ Infinite food source, only first food item is active.
  , uDeadLinks  :: [DeadLink] -- ^ Dead links on the field, fading away.
  }

-- | Initial universe.
initUniverse :: [Point] -> GameConfig -> Universe
initUniverse ps = Universe
  <$> initSnake
  <*> traverse mkFood ps
  <*> pure []

-- | Update universe for each frame.
updateUniverse :: Float -> Universe -> GameConfig -> Universe
updateUniverse dt
    = updateUniverseObjects dt
  >=> checkFoodCollision
  >=> checkSnakeCollision

-- | Update every object in the universe.
updateUniverseObjects :: Float -> Universe -> GameConfig -> Universe
updateUniverseObjects dt u@Universe{..} cfg = u
  { uSnake = moveSnake dt uSnake cfg
  , uFood  = newFood
  , uDeadLinks = mapMaybe (flip (updateDeadLink dt) cfg) uDeadLinks }
  where
    newFood = case updateFood dt (head uFood) of
      Nothing -> tail uFood
      Just f  -> f : tail uFood

-- | Check if snake eats food.
checkFoodCollision :: Universe -> GameConfig -> Universe
checkFoodCollision u@Universe{..} GameConfig{..}
  | (head (snakeLinks uSnake), linkSize) `collides` (foodLocation (head uFood), foodSize)
    = u { uFood = tail uFood
        , uSnake = feedSnake uSnake }
  | otherwise = u

-- | Check if snake collides with itself.
checkSnakeCollision :: Universe -> GameConfig -> Universe
checkSnakeCollision u@Universe{..} cfg@GameConfig{..}
  | any (collides (head (snakeLinks uSnake), linkSize)) (map (, foodSize) (drop 2 (snakeLinks uSnake)))
    = u { uSnake = initSnake cfg
        , uDeadLinks = destroySnake uSnake cfg }
  | otherwise = u

-- | Check collision for two objects.
-- Collision counts when objects' are at least halfway into each other.
collides :: (Point, Float) -> (Point, Float) -> Bool
collides ((x, y), a) ((u, v), b) = ((a + b) / 2)^2 > (x - u)^2 + (y - v)^2


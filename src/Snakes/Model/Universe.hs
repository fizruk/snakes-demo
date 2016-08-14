{-# LANGUAGE RecordWildCards #-}
module Snakes.Model.Universe where

import Control.Applicative
import Data.Traversable
import Graphics.Gloss

import Snakes.Config
import Snakes.Model.Snake
import Snakes.Model.Food

-- | The universe of the The Game of Snakes.
data Universe = Universe
  { uSnake  :: Snake    -- ^ Player's 'Snake'.
  , uFood   :: [Food]   -- ^ Infinite food source, only first food item is active.
  }

initUniverse :: [Point] -> GameConfig -> Universe
initUniverse ps = Universe
  <$> initSnake
  <*> traverse mkFood ps

updateUniverse :: Float -> Universe -> GameConfig -> Universe
updateUniverse dt u@Universe{..} cfg = u
  { uSnake = moveSnake dt uSnake cfg
  , uFood  = newFood }
  where
    newFood = case updateFood dt (head uFood) of
      Nothing -> tail uFood
      Just f  -> f : tail uFood

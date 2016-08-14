{-# LANGUAGE RecordWildCards #-}
module Snakes.Model.Food where

import Graphics.Gloss
import Snakes.Config

type Duration = Float

-- | Regular food item.
data Food = Food
  { foodLocation  :: Point    -- ^ Where is the food.
  , foodTimeout   :: Duration -- ^ How long until location change.
  }

mkFood :: Point -> GameConfig -> Food
mkFood loc GameConfig{..} = Food
  { foodLocation = loc
  , foodTimeout  = defaultFoodTimeout
  }

updateFood :: Float -> Food -> Maybe Food
updateFood dt food
  | foodTimeout food > dt = Just food { foodTimeout = foodTimeout food - dt }
  | otherwise = Nothing

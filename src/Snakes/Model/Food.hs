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

-- | Make a food item at a given location.
mkFood :: Point -> Food
mkFood loc = Food
  { foodLocation = loc
  , foodTimeout  = foodDuration
  }

-- | Update food item's timer.
updateFood :: Float -> Food -> Maybe Food
updateFood dt food
  | foodTimeout food > dt = Just food { foodTimeout = foodTimeout food - dt }
  | otherwise = Nothing

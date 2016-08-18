{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Snakes.Model.Item where

import Data.Binary
import GHC.Generics
import Graphics.Gloss
import Snakes.Model.Effect

-- | A food or bonus item.
data Item = Item
  { itemLocation :: Point       -- ^ Where is the item.
  , itemTimeout  :: Float       -- ^ How long until item disappears (in seconds).
  , itemEffect   :: EffectType  -- ^ What this item does if eaten by a snake.
  } deriving (Generic)

instance Binary Item

-- | Create an 'Item' at a given location.
mkItem :: Point -> EffectType -> Item
mkItem loc eff = Item
  { itemLocation = loc
  , itemTimeout  = effectItemDuration eff
  , itemEffect   = eff
  }

-- | Update item's inner timer.
-- If time's up, return 'Nothing'.
updateItem :: Float -> Item -> Maybe Item
updateItem dt item@Item{..}
  | itemTimeout > dt = Just item { itemTimeout = itemTimeout - dt }
  | otherwise = Nothing


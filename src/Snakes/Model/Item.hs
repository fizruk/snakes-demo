{-# LANGUAGE RecordWildCards #-}
module Snakes.Model.Item where

import Graphics.Gloss
import System.Random
import Snakes.Config

-- | A food or bonus item.
data Item = Item
  { itemLocation :: Point       -- ^ Where is the item.
  , itemTimeout  :: Float       -- ^ How long until item disappears.
  , itemEffect   :: ItemEffect  -- ^ What this item does if eaten by a snake.
  }

-- | What an item does.
data ItemEffect
  = ItemFood          -- ^ Just some food. Makes a snake a bit longer.
  | ItemBonusReverse  -- ^ Reverse all snakes in the game.
  | ItemBonusPhantom  -- ^ Temporarily make snake a phantom, allowing crosses with other snakes and self-crosses.
  deriving (Eq, Enum, Bounded)

instance Random ItemEffect where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

-- | Duration of an item based on its effect.
itemDuration :: ItemEffect -> Float
itemDuration ItemFood = 10
itemDuration _        = 5

itemColor :: ItemEffect -> Color
itemColor ItemFood         = white
itemColor ItemBonusReverse = magenta
itemColor ItemBonusPhantom = cyan

itemSize :: ItemEffect -> Float
itemSize ItemFood = 20
itemSize _        = 10

-- | Create an 'Item' at a given location.
mkItem :: Point -> ItemEffect -> Item
mkItem loc eff = Item
  { itemLocation = loc
  , itemTimeout  = itemDuration eff
  , itemEffect   = eff
  }

-- | Update item's inner timer.
-- If time's up, return 'Nothing'.
updateItem :: Float -> Item -> Maybe Item
updateItem dt item@Item{..}
  | itemTimeout > dt = Just item { itemTimeout = itemTimeout - dt }
  | otherwise = Nothing


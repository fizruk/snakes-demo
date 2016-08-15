{-# LANGUAGE RecordWildCards #-}
module Snakes.Model.Bonus where

import Graphics.Gloss
import System.Random
import Snakes.Config

-- | A bonus item.
data Bonus = Bonus
  { bonusLocation :: Point        -- ^ Where is the bonus.
  , bonusTimeout  :: Float        -- ^ How long until bonus disappears.
  , bonusEffect   :: BonusEffect  -- ^ What this bonus does.
  }

-- | What a bonus does.
data BonusEffect
  = BonusReverse  -- ^ Reverse all snakes in the game.
  | BonusPhantom  -- ^ Temporarily make snake a phantom, allowing crosses with other snakes and self-crosses.
  deriving (Eq, Enum, Bounded)

instance Random BonusEffect where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

-- | Make a bonus item at a given location with given effect.
mkBonus :: Point -> BonusEffect -> GameConfig -> Bonus
mkBonus loc effect GameConfig{..} = Bonus
  { bonusLocation = loc
  , bonusTimeout  = defaultBonusTimeout
  , bonusEffect   = effect
  }

-- | Update bonus item's timer.
updateBonus :: Float -> Bonus -> Maybe Bonus
updateBonus dt bonus@Bonus{..}
  | bonusTimeout > dt = Just bonus { bonusTimeout = bonusTimeout - dt }
  | otherwise = Nothing


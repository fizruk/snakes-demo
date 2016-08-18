{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Snakes.Model.Effect where

import Data.Binary
import GHC.Generics
import Graphics.Gloss
import System.Random

-- | Effect type.
data EffectType
  = EffectFood      -- ^ Just some food. Makes a snake a bit longer.
  | EffectReverse   -- ^ Reverse all snakes in the game.
  | EffectPhantom   -- ^ Temporarily make snake a phantom, allowing crosses with other snakes and self-crosses.
  deriving (Eq, Enum, Bounded, Generic)

instance Binary EffectType

instance Random EffectType where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

-- | Item size for an effect.
effectItemSize :: EffectType -> Float
effectItemSize EffectFood = 15
effectItemSize _          = 7

-- | Duration of an item based on its effect (in seconds).
effectItemDuration :: EffectType -> Float
effectItemDuration EffectFood = 10
effectItemDuration _          = 5

-- | Duration of an effect when activated (in seconds).
effectDuration :: EffectType -> Float
effectDuration EffectPhantom = 10
effectDuration _             = 0

-- | Color of an effect.
effectColor :: EffectType -> Color
effectColor EffectFood    = white
effectColor EffectReverse = magenta
effectColor EffectPhantom = cyan

-- | An active effect.
data Effect = Effect
  { effectType    :: EffectType       -- ^ Effect type.
  , effectTimeout :: Float            -- ^ Time left (in seconds).
  } deriving (Generic)

instance Binary Effect

-- | Create an effect with default duration.
mkEffect :: EffectType -> Effect
mkEffect ty = Effect ty (effectDuration ty)

-- | Update effect's timer.
updateEffect :: Float -> Effect -> Maybe Effect
updateEffect dt e@Effect{..}
  | effectTimeout > dt = Just e { effectTimeout = effectTimeout - dt }
  | otherwise = Nothing

-- | Effects on respawn (i.e. after death).
respawnEffects :: [Effect]
respawnEffects = [ Effect EffectPhantom 5 ]

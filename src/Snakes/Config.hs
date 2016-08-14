module Snakes.Config where

import Graphics.Gloss

-- | Global configuration for the Game of Snakes.
data GameConfig = GameConfig
  { linkSize      :: Float  -- ^ Radius of a single link.
  , linkDistance  :: Float  -- ^ Distance between adjusent link centers.
  , snakeSpeed    :: Float  -- ^ Default snake speed.
  , snakeColor    :: Color  -- ^ Default snake color.
  , maxTurnAngle  :: Float  -- ^ Maximum snake's turn angle velocity in radians per second.
  , initialDir    :: Vector -- ^ Initial direction of the snake.
  , initialLen    :: Int    -- ^ Initial snake's length.
  , fieldSize     :: (Float, Float) -- ^ Size of game's field.
  , defaultFoodTimeout :: Float -- ^ Default food regeneration interval.
  , foodSize      :: Float      -- ^ Radius of a food item.
  , itemRotationRate  :: Float  -- ^ Rotation rate for food and bonus items.
  , deadLinkDuration  :: Float  -- ^ For how long dead links appear on the field.
  , deadLinkSpeed     :: Float  -- ^ How fast dead links float around.
  , deadLinkColor     :: Color  -- ^ Color of dead links.
  , defaultBonusTimeout :: Float  -- ^ Default time before bonus disappears.
  , bonusSize         :: Float  -- ^ Radius of a bonus item.
  , bonusPhantomDuration :: Float -- ^ Duration of a phantom bonus effect.
  }

-- | Default game configuration.
defaultGameConfig :: GameConfig
defaultGameConfig = GameConfig
  { linkSize      = 10
  , linkDistance  = 11
  , snakeSpeed    = 160
  , snakeColor    = dark red
  , maxTurnAngle  = 5
  , initialDir    = (1, 2)
  , initialLen    = 3
  , fieldSize     = (700, 700)
  , defaultFoodTimeout = 10
  , foodSize      = 20
  , itemRotationRate = 50
  , deadLinkDuration = 2
  , deadLinkSpeed    = 30
  , deadLinkColor    = greyN 0.3
  , defaultBonusTimeout = 5
  , bonusSize = 10
  , bonusPhantomDuration = 10
  }

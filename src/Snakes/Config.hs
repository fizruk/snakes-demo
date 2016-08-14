module Snakes.Config where

import Graphics.Gloss

-- | Global configuration for the Game of Snakes.
data GameConfig = GameConfig
  { linkSize      :: Float  -- ^ Radius of a single link.
  , linkDistance  :: Float  -- ^ Distance between adjusent link centers.
  , snakeSpeed    :: Float  -- ^ Default snake speed.
  , snakeColor    :: Color  -- ^ Default snake color.
  , maxTurnAngle  :: Float  -- ^ Maximum snake's turn angle velocity in radians per second.
  }

-- | Default game configuration.
defaultGameConfig :: GameConfig
defaultGameConfig = GameConfig
  { linkSize      = 10
  , linkDistance  = 8
  , snakeSpeed    = 140
  , snakeColor    = dark red
  , maxTurnAngle  = 4
  }

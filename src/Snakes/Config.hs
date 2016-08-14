module Snakes.Config where

import Graphics.Gloss

-- | Global configuration for the Game of Snakes.
data GameConfig = GameConfig
  { linkSize      :: Float  -- ^ Radius of a single link.
  , linkDistance  :: Float  -- ^ Distance between adjusent link centers.
  , snakeSpeed    :: Float  -- ^ Default snake speed.
  , snakeColor    :: Color  -- ^ Default snake color.
  }

-- | Default game configuration.
defaultGameConfig :: GameConfig
defaultGameConfig = GameConfig
  { linkSize      = 15
  , linkDistance  = 12
  , snakeSpeed    = 50
  , snakeColor    = dark red
  }

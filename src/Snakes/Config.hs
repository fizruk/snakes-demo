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
  }

-- | Default game configuration.
defaultGameConfig :: GameConfig
defaultGameConfig = GameConfig
  { linkSize      = 7
  , linkDistance  = 7.5
  , snakeSpeed    = 140
  , snakeColor    = dark red
  , maxTurnAngle  = 6
  , initialDir    = (1, 2)
  , initialLen    = 5
  , fieldSize     = (700, 700)
  }

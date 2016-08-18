module Snakes.Config where

import Graphics.Gloss

-- * General

fieldSize     :: (Float, Float) -- ^ Size of game's field.
fieldMargin   :: (Float, Float) -- ^ Margin of game's field. Food and bonus items can't pop up near field's border.
playerColors  :: [Color]        -- ^ Available snake colors.

-- * Snakes

snakeLinkSize :: Float  -- ^ Radius of a single link.
snakeLinkGap  :: Float  -- ^ Distance between adjusent link centers.
snakeSpeed    :: Float  -- ^ Default snake speed.
snakeTurnRate :: Float  -- ^ Maximum snake's turn angle velocity in radians per second.
snakeStartLen :: Int    -- ^ Initial snake's length.

-- ** Dead snake remains
deadLinkDuration  :: Float  -- ^ For how long dead links appear on the field.
deadLinkSpeed     :: Float  -- ^ How fast dead links float around.
deadLinkColor     :: Color  -- ^ Color of dead links.

-- * Food and bonuses

itemTurnRate    :: Float  -- ^ Rotation rate for food and bonus items.
itemActiveTotal :: Int    -- ^ Number of simultaneously active items.

-- NOTE: Values are groupped below for a clearer view.

fieldSize = (700, 700)
fieldMargin = (50, 50)
playerColors = map dark [red, green, cyan, orange, magenta, yellow]

snakeLinkSize = 7
snakeLinkGap  = 7.5
snakeSpeed    = 150
snakeTurnRate = 8
snakeStartLen = 5

deadLinkDuration = 2
deadLinkSpeed    = 75
deadLinkColor    = greyN 0.3

itemTurnRate    = 2
itemActiveTotal = 7

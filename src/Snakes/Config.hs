module Snakes.Config where

import Graphics.Gloss

-- * General

fieldSize     :: (Float, Float) -- ^ Size of game's field.
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

itemTurnRate  :: Float  -- ^ Rotation rate for food and bonus items.

foodDuration  :: Float  -- ^ Default food regeneration interval.
foodSize      :: Float  -- ^ Radius of a food item.

bonusDuration :: Float  -- ^ Default time before bonus disappears.
bonusSize     :: Float  -- ^ Radius of a bonus item.

-- * Effects

effectPhantomDuration :: Float -- ^ Duration of a phantom bonus effect.

-- NOTE: Values are groupped below for a clearer view.

fieldSize = (700, 700)
playerColors = map dark [white, red, green, cyan, orange, magenta, yellow]

snakeLinkSize = 10
snakeLinkGap  = 11
snakeSpeed    = 160
snakeTurnRate = 5
snakeStartLen = 3

deadLinkDuration = 2
deadLinkSpeed    = 30
deadLinkColor    = greyN 0.3

foodDuration  = 10
foodSize      = 20

bonusDuration = 5
bonusSize     = 10

itemTurnRate  = 2

effectPhantomDuration = 10


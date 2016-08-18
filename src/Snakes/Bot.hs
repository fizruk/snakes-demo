{-# LANGUAGE RecordWildCards #-}
module Snakes.Bot where

import Data.List (minimumBy)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord (comparing)
import Graphics.Gloss.Data.Vector
import Snakes.Control
import Snakes.Model

-- | An artificial intelligence for the Game of Snakes.
type Bot = PlayerName -> Universe -> Maybe SnakeAction

-- | Simple bot just eats whatever there is to eat and ignores any obsticles.
simpleBot :: Bot
simpleBot _name Universe{..} = fmap (SnakeRedirect . itemLocation) (listToMaybe uItems)

-- | This bot goes to the closes item.
lazyBot :: Bot
lazyBot name Universe{..} =
  fmap SnakeRedirect (minimumBy (comparing dist) (Nothing : map (Just . itemLocation) uItems))
  where
    botLoc = head (snakeLinks (uSnakes Map.! name))
    dist (Just point) = magV (botLoc - point)
    dist Nothing      = 1000

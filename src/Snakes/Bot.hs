{-# LANGUAGE RecordWildCards #-}
module Snakes.Bot where

import Data.Maybe
import Snakes.Control
import Snakes.Model

-- | An artificial intelligence for the Game of Snakes.
type Bot = Universe -> Maybe SnakeAction

-- | Simple bot just eats whatever there is to eat and ignores any obsticles.
simpleBot :: Bot
simpleBot Universe{..} = fmap (SnakeRedirect . itemLocation) (listToMaybe uItems)


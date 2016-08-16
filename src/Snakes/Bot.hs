{-# LANGUAGE RecordWildCards #-}
module Snakes.Bot where

import Data.Maybe
import Snakes.Control
import Snakes.Model

-- | An artificial intelligence for the Game of Snakes.
type Bot = PlayerName -> Universe -> Maybe SnakeAction

-- | Simple bot just eats whatever there is to eat and ignores any obsticles.
simpleBot :: Bot
simpleBot _name Universe{..} = fmap (SnakeRedirect . itemLocation) (listToMaybe uItems)


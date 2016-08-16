{-# LANGUAGE RecordWildCards #-}
module Snakes.Bot where

import Snakes.Control
import Snakes.Model

type Bot = Universe -> Maybe Action

simpleBot :: Bot
simpleBot Universe{..} = Just (RedirectSnake (itemLocation (head uItems)))


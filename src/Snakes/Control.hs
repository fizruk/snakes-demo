{-# LANGUAGE RecordWildCards #-}
module Snakes.Control where

import qualified Data.Map as Map
import Graphics.Gloss
import Snakes.Config
import Snakes.Model

-- | Possible user actions.
data Action
  = RedirectSnake Point   -- ^ Point snake's head in a different direction.

handleAction :: Action -> Snake -> GameConfig -> Snake
handleAction (RedirectSnake pos) snake _ = snake { snakeTarget = Just pos }

handlePlayerAction :: PlayerName -> Action -> Universe -> GameConfig -> Universe
handlePlayerAction name action u@Universe{..} cfg = u { uSnakes = Map.adjust (flip (handleAction action) cfg) name uSnakes }

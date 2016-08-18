{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Snakes.Control where

import Data.Binary
import qualified Data.Map as Map
import GHC.Generics
import Graphics.Gloss
import Network.WebSockets
import Snakes.Model

-- | Possible user actions.
data SnakeAction
  = SnakeRedirect Point   -- ^ Point snake's head in a different direction.
  deriving (Generic)

instance Binary SnakeAction

-- | Apply 'SnakeAction' to a 'Snake'.
applySnakeAction :: SnakeAction -> Snake -> Snake
applySnakeAction (SnakeRedirect pos) snake = snake { snakeTarget = Just pos }

-- | Apply 'SnakeAction' to player's 'Snake'.
handleSnakeAction :: PlayerName -> SnakeAction -> Universe -> Universe
handleSnakeAction name action u@Universe{..}
  = u { uSnakes = Map.adjust (applySnakeAction action) name uSnakes }

-- =====================================
-- WebSockersData instances are needed
-- to send/receive Haskell structures
-- over websockets
-- =====================================

instance WebSocketsData SnakeAction where
  fromLazyByteString = decode
  toLazyByteString   = encode


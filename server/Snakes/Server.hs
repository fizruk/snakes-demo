{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Snakes.Server where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (catch)
import Control.Monad (forever)
import Control.Monad.Random (evalRand, newStdGen)
import Data.Map (Map)
import qualified Data.Map as Map

import Network.HTTP.Types (status400)
import Network.Wai (responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
import Servant

import Snakes

-- | Server config.
data Config = Config
  { configUniverse  :: TVar Universe                -- ^ The current state of the universe.
  , configClients   :: TVar (Map PlayerName Client) -- ^ All connected clients by a unique name.
  , configNames     :: TVar [PlayerName]            -- ^ Source of new names.
  }

-- | A client is represented by its websocket 'Connection'.
type Client = Connection

-- | Default server config with empty universe and no clients.
mkDefaultConfig :: IO Config
mkDefaultConfig = do
  cfg <- atomically $ Config
          <$> newTVar emptyUniverse
          <*> newTVar Map.empty
          <*> newTVar (map show [1..])
  spawnBot "Bot 1" simpleBot  (configUniverse cfg)
  spawnBot "Bot 2" lazyBot    (configUniverse cfg)
  spawnBot "Bot 3" lazyBot    (configUniverse cfg)
  return cfg

-- | An API for the Game of Snakes server.
type SnakesAPI = "connect" :> Raw

-- | The Game of Snakes server 'Application'.
server :: Config -> Server SnakesAPI
server config = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
        conn <- acceptRequest pending_conn
        name <- addClient conn config
        putStrLn $ name ++ " joined!"
        handleActions name conn config

    -- this application will be used for non-websocket requests
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

-- | Add a new client to the server state.
-- This will update 'configClients' and add
-- a new player to the 'configUniverse'.
addClient :: Client -> Config -> IO PlayerName
addClient client Config{..} = do
  g <- newStdGen
  atomically $ do
    name:names <- readTVar configNames
    writeTVar configNames names
    modifyTVar configClients (Map.insert name client)
    modifyTVar configUniverse (flip evalRand g . spawnPlayer name)
    return name

-- | An infinite loop, receiving data from the 'Client'
-- and handling its actions via 'handlePlayerAction'.
handleActions :: PlayerName -> Connection -> Config -> IO ()
handleActions name conn cfg@Config{..} = forever $ do
  action <- receiveData conn
  atomically $ do
    modifyTVar configUniverse (handleSnakeAction name action)

-- | Periodically update the 'Universe' and send updates to all the clients.
periodicUpdates :: Int -> Config -> IO ()
periodicUpdates ms cfg@Config{..} = forever $ do
  threadDelay ms -- wait ms milliseconds
  g <- newStdGen
  universe <- atomically $ do
    universe <- (flip evalRand g . updateUniverse secs) <$> readTVar configUniverse
    writeTVar configUniverse universe
    return universe
  broadcastUpdate universe cfg
  where
    -- FIXME: (ms / 10^6) is not the actual time that has passed since the previous update
    -- we should use getCurrentTime to more accurately keep track of time deltas
    secs = fromIntegral ms / 1000000

-- | Send every 'Client' updated 'Universe' concurrently.
broadcastUpdate :: Universe -> Config -> IO ()
broadcastUpdate universe cfg@Config{..} = do
  clients <- readTVarIO configClients
  mapM_ (forkIO . sendUpdate) (Map.toList clients)
  where
    sendUpdate (name, conn) = sendBinaryData conn universe `catch` handleClosedConnection name

    handleClosedConnection :: PlayerName -> ConnectionException -> IO ()
    handleClosedConnection name _ = do
      putStrLn (name ++ " disconnected.")
      atomically $ do
        modifyTVar configClients  (Map.delete name)
        modifyTVar configUniverse (kickPlayer name)

-- | Add a bot to the 'Universe'.
spawnBot :: PlayerName -> Bot -> TVar Universe -> IO ()
spawnBot name bot w = do
  g <- newStdGen
  atomically $ modifyTVar w (flip evalRand g . spawnPlayer name)
  forkIO $ forever $ do
    threadDelay 1000
    atomically $ do
      u <- readTVar w
      case bot name u of
        Just action -> writeTVar w (handleSnakeAction name action u)
        Nothing -> return ()
  return ()


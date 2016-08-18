module Main where

import Control.Concurrent (forkIO)
import Network.Wai.Handler.Warp (run)

import Snakes.Server

main :: IO ()
main = do
  config <- mkDefaultConfig
  forkIO $ periodicUpdates 10000 config   -- update Universe every 10 milliseconds
  run 8000 $ server config

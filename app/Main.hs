module Main where

import Graphics.Gloss.Interface.Pure.Game
import Snakes

main :: IO ()
main = play display bgColor fps initialWorld renderWorld handleWorld updateWorld
  where
    display = InWindow "The Game of Snakes" (500, 500) (200, 200)
    bgColor = black
    fps     = 60

    initialWorld = mkSnake 3 (1, 1) defaultGameConfig
    renderWorld  = flip renderSnake defaultGameConfig
    handleWorld _ w = w
    updateWorld dt = flip (moveSnake dt) defaultGameConfig

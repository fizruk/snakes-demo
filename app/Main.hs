module Main where

import Snakes
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play display bgColor fps initialWorld renderWorld handleWorld updateWorld
  where
    display = InWindow "The Game of Snakes" (500, 500) (200, 200)
    bgColor = black
    fps     = 30

    initialWorld = ()
    renderWorld   w = blank
    handleWorld _ w = w
    updateWorld _ w = w

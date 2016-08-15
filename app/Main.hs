{-# LANGUAGE RecordWildCards #-}
module Main where

import Graphics.Gloss.Interface.Pure.Game
import Snakes

main :: IO ()
main = do
  initialWorld <- randomUniverse cfg
  play display bgColor fps initialWorld renderWorld handleWorld updateWorld
  where
    display = InWindow "The Game of Snakes" (w, h) (100, 100)
    bgColor = black
    fps     = 60

    renderWorld  = flip renderUniverse cfg
    updateWorld dt = flip (updateUniverse dt) cfg

    handleWorld (EventMotion mouse) = flip (handleUniverse (RedirectSnake mouse)) cfg
    handleWorld _ = id

    cfg@GameConfig{..} = defaultGameConfig
    (fieldWidth, fieldHeight) = fieldSize
    (w, h) = (floor fieldWidth, floor fieldHeight)

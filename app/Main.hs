{-# LANGUAGE RecordWildCards #-}
module Main where

import Graphics.Gloss.Interface.Pure.Game
import Snakes

main :: IO ()
main = play display bgColor fps initialWorld renderWorld handleWorld updateWorld
  where
    display = InWindow "The Game of Snakes" (w, h) (100, 100)
    bgColor = black
    fps     = 60

    initialWorld = initSnake cfg
    renderWorld  = flip renderSnake cfg
    updateWorld dt = flip (moveSnake dt) cfg

    handleWorld (EventMotion mouse) = flip (handleAction (RedirectSnake mouse)) cfg
    handleWorld _ = id

    cfg@GameConfig{..} = defaultGameConfig
    (fieldWidth, fieldHeight) = fieldSize
    (w, h) = (floor fieldWidth, floor fieldHeight)

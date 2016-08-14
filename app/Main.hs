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

    initialWorld = initUniverse
      (cycle [(100, 200), (-50, 0), (300, -300)])
      (cycle [((-200, -30), BonusReverse), ((100, -200), BonusReverse)])
      cfg
    renderWorld  = flip renderUniverse cfg
    updateWorld dt = flip (updateUniverse dt) cfg

    handleWorld (EventMotion mouse) = flip (handleUniverse (RedirectSnake mouse)) cfg
    handleWorld _ = id

    cfg@GameConfig{..} = defaultGameConfig
    (fieldWidth, fieldHeight) = fieldSize
    (w, h) = (floor fieldWidth, floor fieldHeight)

{-# LANGUAGE RecordWildCards #-}
module Snakes.Bot where

import Snakes.Control
import Snakes.Model

type Bot = Universe -> Maybe Action

simpleBot :: Bot
simpleBot Universe{..} = Just (RedirectSnake (foodLocation (head uFood)))

phantomBot :: Bot
phantomBot Universe{..}
  | bonusEffect (head uBonuses) == BonusPhantom = Just (RedirectSnake (bonusLocation (head uBonuses)))
  | otherwise = Just (RedirectSnake (foodLocation (head uFood)))

bonusBot :: Bot
bonusBot Universe{..} = Just (RedirectSnake (bonusLocation (head uBonuses)))

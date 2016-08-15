{-# LANGUAGE RecordWildCards #-}
module Snakes.Render where

import Control.Arrow (first)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Snakes.Config
import Snakes.Model

renderUniverse :: Universe -> GameConfig -> Picture
renderUniverse Universe{..} cfg
    = foldMap (flip renderDeadLink cfg) uDeadLinks
   <> renderFood (head uFood) cfg
   <> renderBonus (head uBonuses) cfg
   <> foldMap (flip (uncurry renderSnake) cfg) snakesWithEffects
  where
    getEffects name = map effectType (filter ((== Just name) . effectPlayer) uEffects)
    snakesWithEffects = map (first getEffects) (Map.toList uSnakes)

renderSnake :: [BonusEffect] -> Snake -> GameConfig -> Picture
renderSnake effects snake cfg@GameConfig{..}
  = foldMap (flip renderLink cfg) (snakeLinks snake)
  & color c
  where
    c | BonusPhantom `elem` effects = withAlpha 0.5 snakeColor
      | otherwise = snakeColor

renderLink :: Link -> GameConfig -> Picture
renderLink (x, y) GameConfig{..}
  = thickCircle r r
  & translate x y
  where
    r = linkSize * 2/3

renderDeadLink :: DeadLink -> GameConfig -> Picture
renderDeadLink DeadLink{..} cfg@GameConfig{..}
  = renderLink linkLocation cfg
  & color (withAlpha (linkTimeout / deadLinkDuration) deadLinkColor)

renderFood :: Food -> GameConfig -> Picture
renderFood Food{..} cfg@GameConfig{..}
  = (timeRing <> item)
  & translate x y
  & color white
  where
    (x, y) = foodLocation
    timeRing = renderTimeout (foodTimeout / defaultFoodTimeout) foodSize
    item
      = renderItem foodSize 6
      & rotate (itemRotationRate * foodTimeout)

bonusColor :: BonusEffect -> Color
bonusColor BonusReverse = magenta
bonusColor BonusPhantom = cyan

renderBonus :: Bonus -> GameConfig -> Picture
renderBonus Bonus{..} GameConfig{..}
  = (item <> ring)
  & translate x y
  & color (bonusColor bonusEffect)
  where
    (x, y) = bonusLocation
    ring = renderTimeout (bonusTimeout / defaultBonusTimeout) bonusSize
    item
      = renderItem bonusSize 5
      & rotate (itemRotationRate * bonusTimeout)

renderItem :: Float -> Int -> Picture
renderItem size n
  = polygon (take n (iterate (rotateV (2 * pi / fromIntegral n)) (0, r)))
  where
    r = 0.5 * size

renderTimeout :: Float -> Float -> Picture
renderTimeout x size
  = arc 0 (360 * x) size
  & rotate (-90)

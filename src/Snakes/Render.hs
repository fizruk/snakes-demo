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

renderUniverse :: Universe -> Picture
renderUniverse Universe{..}
    = foldMap renderDeadLink uDeadLinks
   <> renderFood (head uFood)
   <> renderBonus (head uBonuses)
   <> foldMap (uncurry renderSnake) snakesWithEffects
  where
    getEffects name = map effectType (filter ((== Just name) . effectPlayer) uEffects)
    snakesWithEffects = map (first getEffects) (Map.toList uSnakes)

renderSnake :: [BonusEffect] -> Snake -> Picture
renderSnake effects Snake{..}
  = foldMap renderLink snakeLinks
  & color c
  where
    c | BonusPhantom `elem` effects = withAlpha 0.5 snakeColor
      | otherwise = snakeColor

renderLink :: Link -> Picture
renderLink (x, y)
  = thickCircle r r
  & translate x y
  where
    r = snakeLinkSize * 2/3

renderDeadLink :: DeadLink -> Picture
renderDeadLink DeadLink{..}
  = renderLink linkLocation
  & color (withAlpha (linkTimeout / deadLinkDuration) deadLinkColor)

renderFood :: Food -> Picture
renderFood Food{..}
  = (timeRing <> item)
  & translate x y
  & color white
  where
    (x, y) = foodLocation
    timeRing = renderTimeout (foodTimeout / foodDuration) foodSize
    item
      = renderItem foodSize 6
      & rotate (degrees itemTurnRate * foodTimeout)

bonusColor :: BonusEffect -> Color
bonusColor BonusReverse = magenta
bonusColor BonusPhantom = cyan

renderBonus :: Bonus -> Picture
renderBonus Bonus{..}
  = (item <> ring)
  & translate x y
  & color (bonusColor bonusEffect)
  where
    (x, y) = bonusLocation
    ring = renderTimeout (bonusTimeout / bonusDuration) bonusSize
    item
      = renderItem bonusSize 5
      & rotate (degrees itemTurnRate * bonusTimeout)

renderItem :: Float -> Int -> Picture
renderItem size n
  = polygon (take n (iterate (rotateV (2 * pi / fromIntegral n)) (0, r)))
  where
    r = 0.5 * size

renderTimeout :: Float -> Float -> Picture
renderTimeout x size
  = arc 0 (360 * x) size
  & rotate (-90)

-- | Convert radians to degrees.
degrees :: Float -> Float
degrees x = 180 * x / pi


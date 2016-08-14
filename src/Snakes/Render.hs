module Snakes.Render where

import Data.Function ((&))
import Graphics.Gloss
import Snakes.Config
import Snakes.Model

renderSnake :: Snake -> GameConfig -> Picture
renderSnake snake cfg = foldMap (flip renderLink cfg) (snakeLinks snake)

renderLink :: Link -> GameConfig -> Picture
renderLink (x, y) cfg = thickCircle r r
  & translate x y
  & color (snakeColor cfg)
  where
    r = linkSize cfg * 2/3

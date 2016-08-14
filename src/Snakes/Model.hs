module Snakes.Model where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point
import Snakes.Config

-- | A snake is represented by a list of 'Link's and a direction of snake's head.
data Snake = Snake
  { snakeLinks  :: [Link] -- ^ A list of links. First link is head, last link is tail.
  , snakeDir    :: Vector -- ^ Snake's direction is a normalized vector.
  }

-- | A single link of a 'Snake'.
type Link = Point

-- | @mkSnake n dir@ creates a straight 'Snake' with @n@ links,
-- poiting in @dir@ direction and with head at @(0, 0)@.
mkSnake :: Int -> Vector -> GameConfig -> Snake
mkSnake n dir cfg = Snake
  { snakeLinks = take n (iterate (subtract step) (0, 0))
  , snakeDir   = d
  }
  where
    step = mulSV (linkDistance cfg) d
    d = normalizeV dir

-- | Move snake naturally given time delta.
moveSnake :: Float -> Snake -> GameConfig -> Snake
moveSnake dt snake cfg = snake
  { snakeLinks = moveLinks (mulSV (dt * snakeSpeed cfg) (snakeDir snake)) (snakeLinks snake) cfg }

-- | Move head link by a given vector and then move other links.
moveLinks :: Vector -> [Link] -> GameConfig -> [Link]
moveLinks _ [] _ = []
moveLinks dir (l:ls) cfg = moveLinks' (l + dir) ls
  where
    moveLinks' pos [] = [pos]
    moveLinks' pos (n:ns) = pos : moveLinks' (moveLinkTo pos n cfg) ns

-- | Move a single link closer to a given point.
moveLinkTo :: Point -> Link -> GameConfig -> Link
moveLinkTo pos link cfg = pos - mulSV (linkDistance cfg) (normalizeV (pos - link))


module Snakes.Model where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point
import Snakes.Config

-- | A snake is represented by a list of 'Link's and a direction of snake's head.
data Snake = Snake
  { snakeLinks  :: [Link]       -- ^ A list of links. First link is head, last link is tail.
  , snakeDir    :: Vector       -- ^ Snake's direction is a normalized vector.
  , snakeTarget :: Maybe Point  -- ^ Snake's target position.
  }

-- | A single link of a 'Snake'.
type Link = Point

-- | @mkSnake n dir@ creates a straight 'Snake' with @n@ links,
-- poiting in @dir@ direction and with head at @(0, 0)@.
mkSnake :: Int -> Vector -> GameConfig -> Snake
mkSnake n dir cfg = Snake
  { snakeLinks  = take n (iterate (subtract step) (0, 0))
  , snakeDir    = d
  , snakeTarget = Nothing
  }
  where
    step = mulSV (linkDistance cfg) d
    d = normalizeV dir

-- | Move snake naturally given time delta.
moveSnake :: Float -> Snake -> GameConfig -> Snake
moveSnake dt snake cfg = snake' { snakeLinks = links' }
  where
    snake' = adjustDir dt snake cfg
    links' = moveLinks (mulSV (dt * snakeSpeed cfg) (snakeDir snake')) (snakeLinks snake') cfg

-- | Adjust snake's direction to follow the target
adjustDir :: Float -> Snake -> GameConfig -> Snake
adjustDir dt snake cfg = case snakeTarget snake of
  Just target ->
    let dir         = snakeDir snake
        targetDir   = target - head (snakeLinks snake)
        targetAngle = angleVV dir targetDir
        angle       = min (abs targetAngle) (dt * maxTurnAngle cfg)
    in snake { snakeDir = rotateV (angle * angleDir dir targetDir) dir }
  Nothing -> snake

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

-- | Compute an angular direction between two vectors.
angleDir :: Vector -> Vector -> Float
angleDir (x, y) (u, v) = signum (x * v - y * u)


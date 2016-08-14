{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
module Snakes.Model.Snake where

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

-- | Create a 'Snake' in the initial location.
initSnake :: GameConfig -> Snake
initSnake GameConfig{..} = mkSnake initialLen initialDir linkDistance

-- | @mkSnake n dir dist@ creates a straight 'Snake' with @n@ links,
-- poiting in @dir@ direction and with head at @(0, 0)@.
-- Initial length distance is @dist@.
mkSnake :: Int -> Vector -> Float -> Snake
mkSnake n dir dist = Snake
  { snakeLinks  = take n (iterate (subtract step) (0, 0))
  , snakeDir    = d
  , snakeTarget = Nothing }
  where
    step = mulSV dist d
    d = normalizeV dir

-- | Feed a snake effectively making it one link longer.
feedSnake :: Snake -> Snake
feedSnake snake@Snake{..} = snake
  { snakeLinks = snakeLinks ++ [ last snakeLinks ] }

-- | Move snake naturally given time delta.
moveSnake :: Float -> Snake -> GameConfig -> Snake
moveSnake dt snake@Snake{..} cfg@GameConfig{..} = snake
  { snakeLinks = newLinks
  , snakeDir = newDir }
  where
    newLinks = moveLinks (mulSV (dt * snakeSpeed) newDir) snakeLinks cfg
    newDir = adjustDir dt snake cfg

-- | Adjust snake's direction to follow its target.
adjustDir :: Float -> Snake -> GameConfig -> Vector
adjustDir dt snake@Snake{..} GameConfig{..} =
  case snakeTarget of
    Nothing     -> snakeDir
    Just target ->
      let
        targetDir   = target - head snakeLinks
        targetAngle = angleVV snakeDir targetDir
        angle       = min (abs targetAngle) (dt * maxTurnAngle)
        newDir      = rotateV (angle * angleDir snakeDir targetDir) snakeDir
      in newDir

-- | Move head link by a given vector and then move other links.
moveLinks :: Vector -> [Link] -> GameConfig -> [Link]
moveLinks _ [] _ = []
moveLinks dir (l:ls) cfg = moveLinks' (l + dir) ls
  where
    moveLinks' pos [] = [pos]
    moveLinks' pos (n:ns) = pos : moveLinks' (moveLinkTo pos n cfg) ns

-- | Move a single link closer to a given point.
-- Leave link be if already close enough.
moveLinkTo :: Point -> Link -> GameConfig -> Link
moveLinkTo pos link GameConfig{..}
  | magV (pos - link) < linkDistance = link
  | otherwise = pos - mulSV linkDistance (normalizeV (pos - link))

-- | Compute an angular direction between two vectors.
angleDir :: Vector -> Vector -> Float
angleDir (x, y) (u, v) = signum (x * v - y * u)

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Snakes.Model.Snake where

import Data.Binary
import GHC.Generics
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Snakes.Config

-- | A snake is represented by a list of 'Link's and a direction of snake's head.
data Snake = Snake
  { snakeLinks  :: [Link]       -- ^ A list of links. First link is head, last link is tail.
  , snakeDir    :: Vector       -- ^ Snake's direction is a normalized vector.
  , snakeTarget :: Maybe Point  -- ^ Snake's target position.
  , snakeColor  :: Color        -- ^ Color of the snake.
  } deriving (Generic)

instance Binary Color where
  put = put . rgbaOfColor
  get = uncurry4 makeColor <$> get
    where
      uncurry4 f (a, b, c, d) = f a b c d

instance Binary Snake

-- | A location and direction for a newly spawned 'Snake'.
type Spawn = (Point, Vector)

-- | A single link of a 'Snake'.
type Link = Point

-- | A dead snake's link, slowly fading away.
data DeadLink = DeadLink
  { linkLocation  :: Point    -- ^ Current location of the link.
  , linkTimeout   :: Float    -- ^ Time until complete fade out (in seconds).
  , linkDir       :: Vector   -- ^ Direction of flow.
  } deriving (Generic)

instance Binary DeadLink

-- | Spawn a fresh 'Snake'.
spawnSnake :: Spawn -> Color -> Snake
spawnSnake (loc, dir) c = Snake
  { snakeLinks  = take snakeStartLen (iterate (subtract step) loc)
  , snakeDir    = d
  , snakeTarget = Nothing
  , snakeColor  = c }
  where
    d = normalizeV dir
    step = mulSV snakeLinkGap d

-- | Move 'Snake'.
-- First adjust its direction to follow the target.
-- Then move head along that direction and drag all other links naturally.
moveSnake :: Float -> Snake -> Snake
moveSnake dt snake@Snake{..} = snake
  { snakeLinks = newLinks
  , snakeDir = newDir }
  where
    newDir = maybe snakeDir followTarget snakeTarget
    newLinks = moveLinks (mulSV (dt * snakeSpeed) newDir) snakeLinks

    -- | Adjust direction to follow target.
    followTarget :: Point -> Vector
    followTarget target = rotateV (sign * angle) snakeDir
      where
        targetDir   = target - head snakeLinks
        targetAngle = angleVV snakeDir targetDir
        sign        = angleDir snakeDir targetDir
        angle       = min targetAngle (dt * snakeTurnRate)

    -- | Compute an angular direction between two vectors.
    angleDir :: Vector -> Vector -> Float
    angleDir (x, y) (u, v) = signum (x * v - y * u)

    -- | Move head link by a given vector and then move other links.
    moveLinks :: Vector -> [Link] -> [Link]
    moveLinks dir ls = (head ls + dir) : zipWith moveLinkTo ls (tail ls)

    -- | Move a single link closer to a given point.
    -- Don't move if already close enough (i.e. within 'snakeLinkGap').
    moveLinkTo :: Point -> Link -> Link
    moveLinkTo pos link
      | magV (pos - link) < snakeLinkGap = link
      | otherwise = pos - mulSV snakeLinkGap (normalizeV (pos - link))

-- | Feed a snake effectively making it one link longer.
feedSnake :: Snake -> Snake
feedSnake snake@Snake{..} = snake
  { snakeLinks = snakeLinks ++ [ last snakeLinks ] }

-- | Destroy a 'Snake', leaving some floating dead links.
destroySnake :: Snake -> [DeadLink]
destroySnake Snake{..} = map mkDeadLink snakeLinks
  where
    mkDeadLink loc = DeadLink
      { linkLocation = loc
      , linkTimeout  = deadLinkDuration
      , linkDir      = normalizeV (loc - head snakeLinks) }

-- | Move 'DeadLink' around and update its timer.
-- Return 'Nothing' if time's up.
updateDeadLink :: Float -> DeadLink -> Maybe DeadLink
updateDeadLink dt link@DeadLink{..}
  | linkTimeout <= dt = Nothing
  | otherwise         = Just link
      { linkLocation = linkLocation + mulSV (dt * deadLinkSpeed) linkDir
      , linkTimeout  = linkTimeout - dt }

-- | Make snake's tail its head and change direction to where tail is pointed.
reverseSnake :: Snake -> Snake
reverseSnake snake@Snake{..} = snake
  { snakeLinks  = newLinks
  , snakeDir    = normalizeV (a - b) }
  where
    newLinks = reverse snakeLinks
    a:b:_ = newLinks


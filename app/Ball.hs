module Ball where

import Graphics.Gloss

import Types

bvel :: Float
bvel = 50

ball :: Ball
ball = Ball
   { ballPos = (0, 0)
   , ballDiam = 14
   , ballVel = (0, 0)
   , ballColor = white
   , ballShape = circleSolid 14
   }

renderBall :: Ball -> Picture
renderBall ball = uncurry translate (ballPos ball)
                  $ color (ballColor ball)
                  $ ballShape ball

setBallPos :: Ball -> Position -> Ball
setBallPos ball (x',y') = ball { ballPos = (x', y') }

setBallVel :: Ball -> Position -> Ball
setBallVel ball (vx',vy') = ball { ballVel = (vx', vy') }

setBallyVel :: Ball -> Float -> Ball
setBallyVel ball@(Ball _ _ (vx, _) _ _) vel = ball { ballVel = (vx, vel) }

setBallxVel :: Ball -> Float -> Ball
setBallxVel ball@(Ball _ _ (_, vy) _ _) vel = ball { ballVel = (vel, vy) }

resetBall :: Ball -> Ball
resetBall ball = ball { ballPos = (0, 0), ballVel = (0, 0) }

moveBall :: Float -> Game -> (Float, Game)
moveBall delta game@(Game ball@(Ball (x, y) _ (vx, vy) _ _) _ _ _ _ _ _)
   = (delta , game { gameBall = setBallPos ball (x', y') })
   where
      x' = x + vx * delta
      y' = y + vy * delta

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
setBallPos ball@(Ball (x, y) diam vel col shp) (x',y') =
   ball { ballPos = (x', y') , ballDiam = diam
        , ballVel = vel, ballColor = col
        , ballShape = shp }

setBallVel :: Ball -> Position -> Ball
setBallVel ball@(Ball pos diam (vx, vy) col shp) (vx',vy') =
   ball { ballPos = pos , ballDiam = diam
        , ballVel = (vx', vy'), ballColor = col
        , ballShape = shp }

setBallyVel :: Ball -> Float -> Ball
setBallyVel ball@(Ball pos diam (vx, vy) col shp) vel =
   ball { ballPos = pos , ballDiam = diam
        , ballVel = (vx, vel), ballColor = col
        , ballShape = shp }

setBallxVel :: Ball -> Float -> Ball
setBallxVel ball@(Ball pos diam (vx, vy) col shp) vel =
   ball { ballPos = pos , ballDiam = diam
        , ballVel = (vel, vy), ballColor = col
        , ballShape = shp }

resetBall :: Ball -> Ball
resetBall ball@(Ball _ diam vel col shp) =
   ball { ballPos = (0, 0), ballDiam = diam
        , ballVel = (0, 0), ballColor = col
        , ballShape = shp }

moveBall :: Float -> Game -> (Float, Game)
moveBall delta game@(Game ball@(Ball (x, y) _ (vx, vy) _ _) _ _ _ _ _ _) =
   (delta ,game { gameBall = setBallPos ball (x', y') }) where
      x' = x + vx * delta
      y' = y + vy * delta


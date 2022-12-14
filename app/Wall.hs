module Wall where

import Graphics.Gloss

import Types
import Config
import Ball

topWall :: Wall
topWall = Wall
   { wallPos = (0, (fromIntegral height)/2)
   , wallHeight = fromIntegral height
   , wallWidth = fromIntegral width
   , wallColor = white
   , wallShape = rectangleSolid (fromIntegral width) 10
   }

bottomWall :: Wall
bottomWall = Wall
   { wallPos = (0, -(fromIntegral height)/2)
   , wallHeight = fromIntegral height
   , wallWidth = fromIntegral width
   , wallColor = white
   , wallShape = rectangleSolid (fromIntegral width) 10
   }

wallCollided :: Position -> Diameter -> Bool 
wallCollided (_, y) diam = topCollided || bottomCollided where
   topCollided    = y - (diam + 5) <= (- fromIntegral height) / 2 
   bottomCollided = y + (diam + 5) >=  fromIntegral height / 2

wallBounce :: Game -> Game
wallBounce game@(Game ball@(Ball pos diam (vx, vy) _ _) _ _ _ _ _ _) =
   game { gameBall = setBallVel ball (vx', vy') } where
      vx' | wallCollided pos diam = vx - 4
          | otherwise = vx
      vy' | wallCollided pos diam = (vy) * (-1)
          | otherwise = vy

renderWalls :: [Wall] -> Picture
renderWalls walls =
   pictures [ translate (0) (snd $ wallPos wall)
            $ color (wallColor wall)
            $ wallShape wall | wall <- walls ]

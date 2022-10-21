{-# LANGUAGE TemplateHaskell #-}

module Main where

import Config
import DataModels

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Control.Lens
import Control.Lens.Operators

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



setPlayerPos :: Player -> Position -> Player
setPlayerPos player@(Player (x, y) hei wid col shp) (x', y') =
   player { playerPos = (x', y'), playerHeight = hei
          , playerWidth = wid, playerColor = col
          , playerShape = shp}



renderPlayers :: [Player] -> Picture
renderPlayers players =
   pictures [ translate (fst $ playerPos player) (snd $ playerPos player)
            $ color (playerColor player)
            $ playerShape player | player <- players ]



renderWalls :: [Wall] -> Picture
renderWalls walls =
   pictures [ translate (0) (snd $ wallPos wall)
            $ color (wallColor wall)
            $ wallShape wall | wall <- walls ]



-- | Functions
render :: Game -> Picture
render game@(Game ball topWall bottomWall playerOne playerTwo) =
   pictures [ renderWalls [topWall, bottomWall]
            , renderPlayers [playerOne, playerTwo]
            , renderBall ball ]



moveBall :: Float -> Game -> Game
moveBall delta game@(Game ball@(Ball (x, y) _ (vx, vy) _ _) _ _ _ _) =
   game { gameBall = setBallPos ball (x', y') } where
      x' = x + vx * delta
      y' = y + vy * delta



paddleCollided :: Ball -> Player -> Player -> (Bool, Bool)
paddleCollided (Ball (bx, by) _ _ _ _)
               (Player (p1x, p1y) p1h p1w _ _)
               (Player (p2x, p2y) p2h p2w _ _)
               = (lPC || rPC, lUP || rUP) where
   lPC = bx < p1x + p1w && by < p1y + (p1h / 1.5) && by > p1y - (p1h / 1.5)
   rPC = bx >= p2x - p2w && by < p2y + (p2h / 1.5) && by > p2y - (p2h / 1.5)
   lUP | lPC && by > p1y = True
       | otherwise = False
   rUP | rPC && by > p2y = True
       | otherwise = False




paddleBounce :: Game -> Game
paddleBounce game@(Game ball@(Ball pos diam (vx, vy) _ _) _ _ p1 p2) =
   game { gameBall = setBallVel ball (vx', vy') } where
   collision = paddleCollided ball p1 p2
   collided = fst collision
   upperPart = snd collision
   vx' | collided = vx * (-1) * 1.025
       | otherwise = vx
   vy' | collided && upperPart && vy < 0 = vy * (-1)
       | collided && not upperPart && vy > 0 = vy * (-1)
       | otherwise = vy



wallCollided :: Position -> Diameter -> Bool 
wallCollided (_, y) diam = topCollided || bottomCollided where
   topCollided    = y - (diam + 5) <= (- fromIntegral width) / 2 
   bottomCollided = y + (diam + 5) >=  fromIntegral width / 2



wallBounce :: Game -> Game
wallBounce game@(Game ball@(Ball pos diam (vx, vy) _ _) _ _ _ _) =
   game { gameBall = setBallVel ball (vx', vy') } where
      vx' | wallCollided pos diam = vx - 4
          | otherwise = vx
      vy' | wallCollided pos diam = (vy) * (-1)
          | otherwise = vy



handleInput :: Event -> Game -> Game
handleInput (EventKey (Char 's') _ _ _)
            game@(Game ball@(Ball (x, y) _ _ _ _) _ _ _ _) =
            game { gameBall = setBallVel (setBallPos ball (0, 0)) (-150, 50) }

handleInput (EventKey (SpecialKey KeyUp) _ _ _)
            game@(Game _ _ _ p@(Player (x, y) hei _ _ _) _) =
            game { player1 = setPlayerPos p (x, y') } where
            y' | y + (hei / 2) <= 190 = y + 10
               | otherwise = y

handleInput (EventKey (SpecialKey KeyDown) _ _ _)
            game@(Game _ _ _ p@(Player (x, y) hei _ _ _) _) =
            game { player1 = setPlayerPos p (x, y') } where
            y' | y - (hei / 2) >= -190 = y - 10
               | otherwise = y

handleInput (EventKey (SpecialKey KeyLeft) _ _ _)
            game@(Game _ _ _ _ p@(Player (x, y) hei _ _ _)) =
            game { player2 = setPlayerPos p (x, y') } where
            y' | y + (hei / 2) <= 190 = y + 10
               | otherwise = y

handleInput (EventKey (SpecialKey KeyRight) _ _ _)
            game@(Game _ _ _ _ p@(Player (x, y) hei _ _ _)) =
            game { player2 = setPlayerPos p (x, y') } where
            y' | y - (hei / 2) >= -190 = y - 10
               | otherwise = y

handleInput _ game = game



main :: IO ()
main = play window background fps initialState render handleInput update where
   update :: Float -> Game -> Game
   update delta = paddleBounce . wallBounce . moveBall delta

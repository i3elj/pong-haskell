module Player where

import Graphics.Gloss

import Types
import Config
import Ball

playerOne :: Player
playerOne = Player
   { playerPos = (-190,0)
   , playerHeight = 50
   , playerWidth = 10
   , playerColor = white
   , playerVel = 1
   , playerShape = rectangleSolid 10 50
   , playerMov = (IsStill, None)
   }

playerTwo :: Player
playerTwo = Player
   { playerPos = (190,0)
   , playerHeight = 50
   , playerWidth = 10
   , playerColor = white
   , playerVel = 1
   , playerShape = rectangleSolid 10 50
   , playerMov = (IsStill, None)
   }

setPlayerPos :: Player -> Position -> Player
setPlayerPos player@(Player _ hei wid  vel col shp mov _) (x', y') =
   player { playerPos = (x', y'), playerHeight = hei
          , playerWidth = wid, playerVel = vel
          , playerColor = col , playerShape = shp
          , playerMov = mov }

setPlayerState :: Player -> (Movement, Direction) -> Player
setPlayerState player@(Player pos hei wid vel col shp _ _) (mov', dir') =
   player { playerPos = pos, playerHeight = hei
          , playerWidth = wid, playerVel = vel
          , playerColor = col , playerShape = shp
          , playerMov = (mov', dir') }

movePlayerOne :: Float -> Game -> Game
movePlayerOne delta game@(Game ball _ _
                    p@(Player (x, y) hei _ vel _ _ (mov, dir) _) _ _)
   = game { player1 = setPlayerPos p (x, y') } where
         y' | mov == IsMoving && dir == GoingUp && y + (hei / 2) <= 190
               = y + vel * delta * abs (fst (ballVel ball))
            | mov == IsMoving && dir == GoingDown && y - (hei / 2) >= -190
               = y - vel * delta * abs (fst (ballVel ball))
            | otherwise = y

movePlayerTwo :: Float -> Game -> Game
movePlayerTwo delta game@(Game ball _ _ _
                    p@(Player (x, y) hei _ vel _ _ (mov, dir) _) _)
   = game { player2 = setPlayerPos p (x, y') } where
         y' | mov == IsMoving && dir == GoingUp && y + (hei / 2) <= 190
               = y + vel * delta * abs (fst (ballVel ball))
            | mov == IsMoving && dir == GoingDown && y - (hei / 2) >= -190
               = y - vel * delta * abs (fst (ballVel ball))
            | otherwise = y

movePlayers :: (Float, Game)-> Game
movePlayers (delta, game@(Game _ _ _ p1 p2 _))
   | mov1 == IsMoving = movePlayerOne delta game
   | mov2 == IsMoving = movePlayerTwo delta game
   | otherwise = game
   where (mov1, _) = playerMov p1
         (mov2, _) = playerMov p2

renderPlayers :: [Player] -> Picture
renderPlayers players =
   pictures [ translate (fst $ playerPos player) (snd $ playerPos player)
            $ color (playerColor player)
            $ playerShape player | player <- players ]

paddleCollided :: Ball -> Player -> Player -> (Bool, Bool)
paddleCollided (Ball (bx, by) _ _ _ _)
               (Player (p1x, p1y) p1h p1w _ _ _ _ _)
               (Player (p2x, p2y) p2h p2w _ _ _ _ _)
               = (lPC || rPC, lUP || rUP) where
   lPC = bx < p1x + p1w && by < p1y + (p1h / 1.5) && by > p1y - (p1h / 1.5) && bx > p1x
   rPC = bx >= p2x - p2w && by < p2y + (p2h / 1.5) && by > p2y - (p2h / 1.5) && bx < p2x
   lUP | lPC && by > p1y = True
       | otherwise = False
   rUP | rPC && by > p2y = True
       | otherwise = False

paddleBounce :: Game -> Game
paddleBounce game@(Game ball@(Ball pos diam (vx, vy) _ _) _ _ p1 p2 _) =
   game { gameBall = setBallVel ball (vx', vy') } where
   collision = paddleCollided ball p1 p2
   collided = fst collision
   upperPart = snd collision
   vx' | collided = vx * (-1) * 1.025
       | otherwise = vx
   vy' | collided && upperPart && vy < 0 = vy * (-1)
       | collided && not upperPart && vy > 0 = vy * (-1)
       | otherwise = vy

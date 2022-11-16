module Player where

import Graphics.Gloss

import Types
import Config
import Ball

playerOne :: Player
playerOne = Player
   { playerPos = (-(fromIntegral width / 2) + 40,0)
   , playerHeight = 80
   , playerWidth = 12
   , playerColor = white
   , playerVel = 1
   , playerShape = rectangleSolid 12 80
   , playerMov = (IsStill, None)
   , playerRotation = 0
   }

playerTwo :: Player
playerTwo = Player
   { playerPos = ((fromIntegral width/2) - 40,0)
   , playerHeight = 80
   , playerWidth = 12
   , playerColor = white
   , playerVel = 1
   , playerShape = rectangleSolid 12 80
   , playerMov = (IsStill, None)
   , playerRotation = 0
   }

setPlayerPos :: Player -> Position -> Player
setPlayerPos player@(Player pos hei wid  vel col shp mov deg) (x', y')
   = player { playerPos   = (x', y') , playerHeight   = hei
            , playerWidth = wid      , playerVel      = vel
            , playerColor = col      , playerShape    = shp
            , playerMov   = mov      , playerRotation = deg }

setPlayerState :: Player -> (Movement, Direction) -> Player
setPlayerState player@(Player pos hei wid vel col shp mov deg) (mov', dir')
   = player { playerPos   = pos          , playerHeight   = hei
            , playerWidth = wid          , playerVel      = vel
            , playerColor = col          , playerShape    = shp
            , playerMov   = (mov', dir') , playerRotation = deg }

rotatePlayerOne :: Player -> Float -> Player
rotatePlayerOne p@(Player pos hei wid vel col shp mov deg) deg'
   | deg <= 45 = p { playerPos   = pos , playerHeight   = hei
                   , playerWidth = wid , playerVel      = vel
                   , playerColor = col , playerShape    = shp
                   , playerMov   = mov , playerRotation = deg' }
   | otherwise = p { playerPos   = pos , playerHeight   = hei
                   , playerWidth = wid , playerVel      = vel
                   , playerColor = col , playerShape    = shp
                   , playerMov   = mov , playerRotation = deg }

rotatePlayerTwo :: Player -> Float -> Player
rotatePlayerTwo p@(Player pos hei wid vel col shp mov deg) deg'
   | deg <= -45 = p { playerPos   = pos , playerHeight   = hei
                    , playerWidth = wid , playerVel      = vel
                    , playerColor = col , playerShape    = shp
                    , playerMov   = mov , playerRotation = deg' }
   | otherwise = p { playerPos   = pos , playerHeight   = hei
                   , playerWidth = wid , playerVel      = vel
                   , playerColor = col , playerShape    = shp
                   , playerMov   = mov , playerRotation = deg }

movePlayerOne :: Float -> Game -> Game
movePlayerOne delta game@(Game ball _ _
                    p@(Player (x, y) hei _ vel _ _ (mov, dir) _) _ _ _)
   = game { player1 = setPlayerPos p (x, y') } where
         y' | mov == IsMoving && dir == GoingUp && y + (hei / 2) <= (fromIntegral height) / 2
               = y + vel * delta * abs (fst (ballVel ball))
            | mov == IsMoving && dir == GoingDown && y - (hei / 2) >= -(fromIntegral height) / 2
               = y - vel * delta * abs (fst (ballVel ball))
            | otherwise = y

movePlayerTwo :: Float -> Game -> Game
movePlayerTwo delta game@(Game ball _ _ _
                    p@(Player (x, y) hei _ vel _ _ (mov, dir) _) _ _)
   = game { player2 = setPlayerPos p (x, y') } where
         y' | mov == IsMoving && dir == GoingUp && y + (hei / 2) <= (fromIntegral height) / 2
               = y + vel * delta * abs (fst (ballVel ball))
            | mov == IsMoving && dir == GoingDown && y - (hei / 2) >= -(fromIntegral height) / 2
               = y - vel * delta * abs (fst (ballVel ball))
            | otherwise = y

resetPlayer :: Player -> Int -> Player
resetPlayer p@(Player pos hei wid vel col shp mov deg) i
   | i == 1 = p { playerPos = (-(fromIntegral width / 2) + 40,0)
                , playerHeight   = hei, playerWidth = wid
                , playerVel      = vel, playerColor = col
                , playerShape    = shp, playerMov   = mov
                , playerRotation = deg }
   | i == 2 = p { playerPos = ((fromIntegral width/2) - 40,0)
                , playerHeight   = hei, playerWidth = wid
                , playerVel      = vel, playerColor = col
                , playerShape    = shp, playerMov   = mov
                , playerRotation = deg }

movePlayers :: (Float, Game) -> Game
movePlayers (delta, game@(Game _ _ _ p1 p2 _ _))
   | mov1 == IsMoving = movePlayerOne delta game
   | mov2 == IsMoving = movePlayerTwo delta game
   | otherwise = game
   where (mov1, _) = playerMov p1
         (mov2, _) = playerMov p2

renderPlayers :: [Player] -> Picture
renderPlayers players =
   pictures [ translate (fst $ playerPos player) (snd $ playerPos player)
            $ color (playerColor player)
            $ rotate (playerRotation player)
            $ playerShape player | player <- players ]

paddleCollided :: Ball -> Player -> Player -> (Bool, Bool, Float)
paddleCollided (Ball (bx, by) _ _ _ _)
               (Player (p1x, p1y) p1h p1w _ _ _ _ _)
               (Player (p2x, p2y) p2h p2w _ _ _ _ _)
   = (collided, upOrDown, distance) where

      collided = lPC || rPC
      lPC = bx < p1x + p1w && by < p1y + (p1h / 1.5)
            && by > p1y - (p1h / 1.5) && bx > p1x
      rPC = bx >= p2x - p2w && by < p2y + (p2h / 1.5)
            && by > p2y - (p2h / 1.5) && bx < p2x

      upOrDown = lUP || rUP
      lUP | lPC && by > p1y = True
          | otherwise = False
      rUP | rPC && by > p2y = True
          | otherwise = False
      
      distance | rPC = by - p2y
               | lPC = by - p1y
               | otherwise = 0

paddleBounce :: Game -> Game
paddleBounce game@(Game ball@(Ball pos diam (vx, vy) _ _) _ _ p1 p2 _ _)
   = game { gameBall = setBallVel ball (vx', vy') }
   where
      collisionEvent = paddleCollided ball p1 p2
      cld = first collisionEvent
      up = second collisionEvent
      down = not up
      dis = third collisionEvent

      vx' | cld = vx * (-1) * 1.025
          | otherwise = vx

      vy' | cld = (dis*1.5) + vy
          | otherwise = vy

      first  (a,_,_) = a
      second (_,a,_) = a
      third  (_,_,a) = a

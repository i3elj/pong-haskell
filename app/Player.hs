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
setPlayerPos player@(Player pos hei wid  vel col shp mov) (x', y') =
   player { playerPos = (x', y'), playerHeight = hei
          , playerWidth = wid, playerVel = vel
          , playerColor = col , playerShape = shp
          , playerMov = mov }

setPlayerState :: Player -> (Movement, Direction) -> Player
setPlayerState player@(Player pos hei wid vel col shp mov) (mov', dir') =
   player { playerPos = pos, playerHeight = hei
          , playerWidth = wid, playerVel = vel
          , playerColor = col , playerShape = shp
          , playerMov = (mov', dir') }

movePlayerOne :: Float -> Game -> Game
movePlayerOne delta game@(Game ball _ _
                    p@(Player (x, y) hei _ vel _ _ (mov, dir)) _ _ _)
   = game { player1 = setPlayerPos p (x, y') } where
         y' | mov == IsMoving && dir == GoingUp && y + (hei / 2) <= 190
               = y + vel * delta * abs (fst (ballVel ball))
            | mov == IsMoving && dir == GoingDown && y - (hei / 2) >= -190
               = y - vel * delta * abs (fst (ballVel ball))
            | otherwise = y

movePlayerTwo :: Float -> Game -> Game
movePlayerTwo delta game@(Game ball _ _ _
                    p@(Player (x, y) hei _ vel _ _ (mov, dir)) _ _)
   = game { player2 = setPlayerPos p (x, y') } where
         y' | mov == IsMoving && dir == GoingUp && y + (hei / 2) <= 190
               = y + vel * delta * abs (fst (ballVel ball))
            | mov == IsMoving && dir == GoingDown && y - (hei / 2) >= -190
               = y - vel * delta * abs (fst (ballVel ball))
            | otherwise = y

resetPlayer :: Player -> Int -> Player
resetPlayer p@(Player pos hei wid vel col shp mov) i
   | i == 1 = p { playerPos = (-190, 0)
                , playerHeight = hei
                , playerWidth = wid
                , playerVel = vel
                , playerColor = col
                , playerShape = shp
                , playerMov = mov
                }
   | i == 2 = p { playerPos = (190, 0)
                , playerHeight = hei
                , playerWidth = wid
                , playerVel = vel
                , playerColor = col
                , playerShape = shp
                , playerMov = mov
                }

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
            $ playerShape player | player <- players ]

-- paddleCollided :: Ball -> Player -> Player -> (Bool, Bool)
-- paddleCollided (Ball (bx, by) _ _ _ _)
--                (Player (p1x, p1y) p1h p1w _ _ _ _)
--                (Player (p2x, p2y) p2h p2w _ _ _ _)
--                = (lPC || rPC, lUP || rUP) where
--    lPC = bx < p1x + p1w && by < p1y + (p1h / 1.5) && by > p1y - (p1h / 1.5) && bx > p1x
--    rPC = bx >= p2x - p2w && by < p2y + (p2h / 1.5) && by > p2y - (p2h / 1.5) && bx < p2x
--    lUP | lPC && by > p1y = True
--        | otherwise = False
--    rUP | rPC && by > p2y = True
--        | otherwise = False

paddleCollided :: Ball -> Player -> Player -> (Bool, ((Bool, Bool),(Bool, Bool)), Bool)
paddleCollided (Ball (bx, by) _ _ _ _)
               (Player (p1x, p1y) p1h p1w _ _ _ _)
               (Player (p2x, p2y) p2h p2w _ _ _ _)
   = (collided, ((level1, level2), (level3, level4)), level0) where
      hei = p1h / 5

      collided = lPC || rPC
      -- Left Paddle Collided?
      lPC = bx < p1x + p1w && by < p1y + (p1h / 1.5)
         && by > p1y - (p1h / 1.5) && bx > p1x

      -- Right Paddle Collided?
      rPC = bx >= p2x - p2w && by < p2y + (p2h / 1.5)
         && by > p2y - (p2h / 1.5) && bx < p2x

      level0 = lv0 || rv0
      -- Left Paddle level 0
      lv0 | lPC && by < p1y + (hei/2)
            && by > p1y - (hei/2) = True
          | otherwise = False
      -- Right Paddle level 0
      rv0 | lPC && by < p2y + (hei/2)
            && by > p2y - (hei/2) = True
          | otherwise = False

      level1 = lv1 || rv1
      -- Left Paddle level 1
      lv1 | lPC && by < p1y + hei + (hei/2)
            && by > p1y + (hei/2) = True
          | otherwise = False
      -- Right Paddle level 1
      rv1 | lPC && by < p2y + hei + (hei/2)
            && by > p2y + (hei/2) = True
          | otherwise = False

      level2 = lv2 || rv2
      -- Left Paddle level 2
      lv2 | lPC && by < p1y + (hei/2) + (hei*2)
            && by > p1y + (hei/2) + hei = True
          | otherwise = False
      -- Right Paddle level 2
      rv2 | lPC && by < p2y + (hei/2) + (hei*2)
            && by > p2y + (hei/2) + hei = True
          | otherwise = False

      level3 = lv3 || rv3
      -- Left Paddle level 3
      lv3 | lPC && by < p1y - (hei/2)
            && by > p1y - (hei/2) - hei = True
          | otherwise = False
      -- Right Paddle level 3
      rv3 | lPC && by < p2y - (hei/2)
            && by > p2y - (hei/2) - hei = True
          | otherwise = False

      level4 = lv4 || rv4
      -- Left Paddle level 4
      lv4 | lPC && by < p1y - (hei/2) - hei
            && by > p1y - (hei*2) - (hei/2) = True
          | otherwise = False
      -- Right Paddle level 4
      rv4 | lPC && by < p2y - (hei/2)
            && by > p2y - (hei*2) - (hei/2) = True
          | otherwise = False

paddleBounce :: Game -> Game
paddleBounce game@(Game ball@(Ball pos diam (vx, vy) _ _) _ _ p1 p2 _ _) =
   game { gameBall = setBallVel ball (vx', vy') } where
   collision = paddleCollided ball p1 p2
   collided = first collision
   lv0 = third collision
   lv1 = fst $ fst $ second collision
   lv2 = snd $ fst $ second collision
   lv3 = fst $ snd $ second collision
   lv4 = snd $ snd $ second collision

   vx' | collided = vx * (-1) * 1.025
       | otherwise = vx

   vy' | collided && lv3 && vy == 0 = 50 * (-0.5)
       | collided && lv4 && vy == 0 = 50 * (-1.5)
       | collided && lv1 && vy == 0 = 50 * 0.5
       | collided && lv2 && vy == 0 = 50 * 1.5

       | collided && lv2 && vy < 0 = vy * (-1.5)
       | collided && lv2 && vy > 0 = vy * 1.5

       | collided && lv1 && vy < 0 = vy * (-0.5)
       | collided && lv1 && vy > 0 = vy * 0.5

       | collided && lv3 && vy > 0 = vy * (-0.5)
       | collided && lv3 && vy < 0 = vy * 0.5

       | collided && lv4 && vy > 0 = vy * (-1.5)
       | collided && lv4 && vy < 0 = vy * 1.5

       | collided && lv0 = 0
       | otherwise = vy

   third  (_,_,a) = a
   second (_,a,_) = a
   first  (a,_,_) = a

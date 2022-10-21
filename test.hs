{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Control.Lens
import Control.Lens.Operators

type Radius = Float
type Position = (Float, Float)
type Height = Float
type Width = Float
type Shape = (Float, Float)

width :: Int
width = 400

data Ball = Ball 
   { ballPos :: Position
   , ballVel :: Position
   , ballRadius :: Radius
   } deriving (Show)

makeLenses ''Ball

data Player = Player 
   { playerPos :: (Float, Float)
   , playerHeight :: Float
   , playerWidth :: Float
   , playerShape :: (Int, Int)
   } deriving Show

makeLenses ''Player

data Game = Game 
   { ball :: Ball 
   , player1 :: Player
   , player2 :: Player
   } deriving Show

makeLenses ''Game

myBall :: Ball
myBall = Ball
   { ballPos = (0, 0)
   , ballVel = (-220, 100)
   , ballRadius = 10
   }

playerOne :: Player
playerOne = Player
   { playerPos = (0,0)
   , playerHeight = 0
   , playerWidth = 0
   , playerShape = (0, 0)
  }

playerTwo :: Player
playerTwo = Player 
   { playerPos = (10,20)
   , playerHeight = 100
   , playerWidth = 50
   , playerShape = (0,0)
   }

myGame :: Game
myGame = Game
   { ball = myBall
   , player1 = playerOne
   , player2 = playerTwo
   }

-- -- | Detect a collision with one of the side walls.
-- wallCollided :: Position -> Radius -> Bool 
-- wallCollided (_, y) radius = topCollided || bottomCollided where
--    topCollided    = y - (radius + 5) <= (- fromIntegral width) / 2 
--    bottomCollided = y + (radius + 5) >=  fromIntegral width / 2

-- wallBounce :: Game -> Game
-- wallBounce game = game & (ball . ballVel) .~ (vx, vy') where
--    pos = game ^. (ball . ballPos)
--    rad = game ^. (ball . ballRadius)
--    (vx, vy) = game ^. (ball . ballVel)
--    vy' | wallCollided pos rad = vy * (-1)
--        | otherwise = vy

setyVel :: Ball -> Float -> Position
setyVel ball vel = set _2 vel (ballVel ball)

-- test :: Game -> Game
test game = game ^. (ball . ballVel) .~ (0,1)

-- ballVel $ ball game
-- .~ (vx, vy) where
--    (vx, vy) = game ^. (ball . ballVel)

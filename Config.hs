module Config where

import Graphics.Gloss

import DataModels

-- | Configurations
fps :: Int
fps = 60

width, height, offset :: Int
width  = 400
height = 400
offset = 100

window :: Display
window = InWindow "pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Objects
ball :: Ball
ball = Ball
   { ballPos = (0, 0)
   , ballDiam = 10
   , ballVel = (-150, 50)
   , ballColor = white
   , ballShape = circleSolid 10
   }



topWall :: Wall
topWall = Wall
   { wallPos = (0, 200)
   , wallHeight = 10
   , wallWidth = 400
   , wallColor = white
   , wallShape = rectangleSolid 400 10
   }



bottomWall :: Wall
bottomWall = Wall
   { wallPos = (0, -200)
   , wallHeight = 400
   , wallWidth = 10
   , wallColor = white
   , wallShape = rectangleSolid 400 10
   }



playerOne :: Player
playerOne = Player
   { playerPos = (-190,0)
   , playerHeight = 50
   , playerWidth = 10
   , playerColor = white
   , playerShape = rectangleSolid 10 50
   }



playerTwo :: Player
playerTwo = Player
   { playerPos = (190,0)
   , playerHeight = 50
   , playerWidth = 10
   , playerColor = white
   , playerShape = rectangleSolid 10 50
   }



initialState :: Game
initialState = Game
   { gameBall = ball
   , wall1 = topWall
   , wall2 = bottomWall
   , player1 = playerOne 
   , player2 = playerTwo
   }

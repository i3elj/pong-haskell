type Radius = Float
type Position = (Float, Float)
type Height = Float
type Width = Float
type Shape = (Float, Float)


data Ball = Ball { ballLoc :: Position -- ^ Pong ball (x, y) location
                 , ballVel :: Position -- ^ Pong ball (x, y) velocity
                 , ballShape :: Radius
                 } deriving (Show)

data Player = Player { playerPosition :: (Float, Float) -- ^ Player (x, y) location
                     , playerHeight :: Float -- ^ Player height
                     , playerWidth :: Float -- ^ Player width
                     }


data Game = Game { ball :: Ball 
                 , player1 :: Player
                 , player2 :: Player
                 } deriving Show

myBall :: Ball
myBall = Ball { ballLoc = (0, 0)
              , ballVel = (-220, 100)
              , ballShape = 10
              }

playerOne :: Player
playerOne = Player { playerLoc = (0,0)
                   , playerHeight = 0
                   , playerWidth = 0
                   , shape = (playerHeight, playerWidth)
                   -- wall y = translate 0 y $ color (dark red) $ rectangleSolid 400 10
                   }

playerTwo :: Player
playerTwo = Player { playerLoc = (10,20)
                   , playerHeight = 100
                   , playerWidth = 50
                   , shape = (playerHeight, playerWidth)
                   -- wall y = translate 0 y $ color (dark red) $ rectangleSolid 400 10
                   }

myGame :: Game
myGame = Game { ball = myBall
              , player1 = playerOne
              , player2 = playerTwo
              }

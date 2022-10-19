module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-- | Configurations
fps :: Int
fps = 60

width, height, offset :: Int
width  = 400
height = 400
offset = 100

window :: Display
window = InWindow "dev" (width, height) (offset, offset)

background :: Color
background = white

-- | Data Models
type Radius = Float
type Position = (Float, Float)
type Height = Float
type Width = Float

data Ball = Ball
   { ballPos :: Position -- ^ Pong ball (x, y) location
   , ballRadius :: Float -- ^ Pong ball radius
   , ballVel :: Position -- ^ Pong ball (x, y) velocity
   , ballColor :: Color
   , ballShape :: Picture
   } deriving Show

data Wall = Wall
   { wallPos :: Position
   , wallHeight :: Height
   , wallWidth :: Width
   , wallColor :: Color
   , wallShape :: Picture
   } deriving Show

data Player = Player
   { playerPos :: Position -- ^ Player (x, y) location
   , playerHeight :: Height -- ^ Player height
   , playerWidth :: Width -- ^ Player width
   , playerColor :: Color
   , playerShape :: Picture
   } deriving Show

data Game = Game
   { gameBall :: Ball
   , walls :: [Wall]
   , players :: [Player]
   } deriving Show

-- | Objects
ball :: Ball
ball = Ball
   { ballPos = (0, 0)
   , ballRadius = 10
   , ballVel = (50, 45)
   , ballColor = dark red
   , ballShape = circle 10
   }
   
topWall :: Wall
topWall = Wall
   { wallPos = (0, 200)
   , wallHeight = 10
   , wallWidth = 400
   , wallColor = dark red
   , wallShape = rectangleSolid 400 10
   }

bottomWall :: Wall
bottomWall = Wall
   { wallPos = (0, -200)
   , wallHeight = 10
   , wallWidth = 400
   , wallColor = dark red
   , wallShape = rectangleSolid 400 10
   }

playerOne :: Player
playerOne = Player
   { playerPos = (-190,0)
   , playerHeight = 50
   , playerWidth = 10
   , playerColor = dark red
   , playerShape = rectangleSolid 10 50
   }

playerTwo :: Player
playerTwo = Player
   { playerPos = (190,0)
   , playerHeight = 50
   , playerWidth = 10
   , playerColor = dark red
   , playerShape = rectangleSolid 10 50
   }

-- | The starting state for the game of Pong.
initialState :: Game
initialState = Game
   { gameBall = ball
   , walls = [ topWall, bottomWall ]
   , players = [ playerOne, playerTwo ]
   }

renderBall :: Ball -> Picture
renderBall ball = uncurry translate (ballPos ball)
                  $ color (ballColor ball)
                  $ ballShape ball

renderPlayers :: [Player] -> Picture
renderPlayers players =
   pictures [ translate (fst $ playerPos player) (0)
            $ color (playerColor player)
            $ playerShape player | player <- players ]

renderWalls :: [Wall] -> Picture
renderWalls walls =
   pictures [ translate (0) (snd $ wallPos wall)
            $ color (wallColor wall)
            $ wallShape wall | wall <- walls ]

-- | Functions
render :: Game -> Picture
render game = pictures [ renderWalls (walls game)
                       , renderPlayers (players game)
                       , renderBall (gameBall game) ]

moveBall :: Float -> Game -> Game
moveBall delta game = game {
      gameBall = Ball {
         ballPos    = (x', y'),
         ballRadius = (ballRadius $ gameBall game),
         ballVel    = (ballVel $ gameBall game),
         ballColor  = (ballColor $ gameBall game),
         ballShape  = (ballShape $ gameBall game)
      }
   } where
   (x, y) = ballPos $ gameBall game
   (vx, vy) = ballVel $ gameBall game
   x' = x + vx * delta -- ^ new x velocity
   y' = y + vy * delta -- ^ new y velocity


-- | Detect a collision with a paddle.
paddleCollided :: Position -> Radius -> Bool
paddleCollided (x, y) radius = leftPaddleCollided || rightPaddleCollided
   where
   leftPaddleCollided  = x - (radius * 2) <= (- fromIntegral width) / 2
   rightPaddleCollided = x + (radius * 2) >= fromIntegral width / 2

paddleBounce :: Game -> Game
paddleBounce game = game {
      gameBall = Ball {
         ballPos    = (ballPos $ gameBall game),
         ballRadius = (ballRadius $ gameBall game),
         ballVel    = (vx', vy),
         ballColor  = (ballColor $ gameBall game),
         ballShape  = (ballShape $ gameBall game)
      }
   } where
   radius = ballRadius $ gameBall game
   (vx, vy) = ballVel $ gameBall game
   vx' | paddleCollided (ballPos $ gameBall game) radius = vx * (-1)
       | otherwise = vx

-- | Detect a collision with one of the side walls.
wallCollided :: Position -> Radius -> Bool 
wallCollided (_, y) radius = topCollided || bottomCollided where
   topCollided    = y - (radius + 5) <= (- fromIntegral width) / 2 
   bottomCollided = y + (radius + 5) >=  fromIntegral width / 2


wallBounce :: Game -> Game
wallBounce game = game {
      gameBall = Ball {
         ballPos = (ballPos $ gameBall game),
         ballRadius = (ballRadius $ gameBall game),
         ballVel = (vx, vy'),
         ballColor = (ballColor $ gameBall game),
         ballShape = (ballShape $ gameBall game)
      }
   } where
   pos = ballPos $ gameBall game
   radius = ballRadius $ gameBall game
   (vx, vy) = ballVel $ gameBall game
   vy' | wallCollided pos radius = vy * (-1)
       | otherwise = vy

main :: IO ()
main = simulate window background fps initialState render update where
   update :: ViewPort -> Float -> Game -> Game
   update _ delta = wallBounce . paddleBounce . moveBall delta

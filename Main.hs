module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-- Configurations
fps :: Int
fps = 45

width, height, offset :: Int
width  = 400
height = 400
offset = 100

window :: Display
window = InWindow "dev" (width, height) (offset, offset)

background :: Color
background = white

-- Data Models
-- | Data describing the state of the pong game. 
type Radius = Float
type Position = (Float, Float)
type Height = Float

data PongGame = Game { ballLoc :: Position -- ^ Pong ball (x, y) location
                     , ballVel :: Position -- ^ Pong ball (x, y) velocity
                     , player1 :: Height   -- ^ Left Player height
                     , player2 :: Height   -- ^ Right Player height
                     } deriving (Show)

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game { ballLoc = (0, 0)
                    , ballVel = (-220, 100)
                    , player1 = 40
                    , player2 = -80
                    }

-- Functions
moveBall :: Float -> PongGame  -> PongGame
moveBall delta game = game { ballLoc = (x', y')} where
    (x, y) = ballLoc game
    (vx, vy) = ballVel game
    -- new  values
    x' = x + vx * delta
    y' = y + vy * delta


render :: PongGame -> Picture
render game = pictures [ ball, walls, players ] where
    ball     = uncurry translate (ballLoc game) $ color (dark red) $ circle 10
    wall y   = translate 0 y                    $ color (dark red) $ rectangleSolid 400 10
    player x = translate x 0                    $ color (dark red) $ rectangleSolid 10  50
    players  = pictures [player 180, player (-180)]
    walls    = pictures [wall 200, wall (-200)]

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleCollision :: Position -> Radius -> Bool
paddleCollision (x, y) radius = leftPaddleCollided || rightPaddleCollided where
    leftPaddleCollided  = x - (radius * 2) <= (- fromIntegral width) / 2
    rightPaddleCollided = x + (radius * 2) >= fromIntegral width / 2

paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) } where
    radius = 15
    (vx, vy) = ballVel game
    vx' | paddleCollision (ballLoc game) radius = vx * (-1)
        | otherwise = vx

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollided || bottomCollided where
    topCollided    = y - radius <= (- fromIntegral width) / 2 
    bottomCollided = y + radius >=  fromIntegral width / 2

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') } where
    radius = 15
    (vx, vy) = ballVel game
    vy' | wallCollision (ballLoc game) radius = vy * (-1)
        | otherwise = vy

-- wallCollision :: PongGame -> Radius -> PongGame 
-- wallCollision game radius =
--     if topCollided || bottomCollided then
--         game { ballVel = (x', y') }
--     else game where
--         topCollided    = y - radius <= (- fromIntegral width) / 2 
--         bottomCollided = y + radius >=  fromIntegral width / 2
--         (vx, vy) = ballVel game
--         (_, y) = ballLoc game
--         x' = vx * (-1)
--         y' = vy * (-1)

main :: IO ()
main = simulate window background fps initialState render update where
    update :: ViewPort -> Float -> PongGame -> PongGame
    update _ delta = wallBounce . paddleBounce . moveBall delta

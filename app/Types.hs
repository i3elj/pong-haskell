module Types where

import Graphics.Gloss

type Diameter = Float
type Position = (Float, Float)
type Height = Float
type Width = Float

type BPQueue = [BallParticle]

type CustomColor = (Float, Float, Float, Float)

-- type Side = LeftP | RightP

data Movement = IsMoving | IsStill deriving (Show, Eq)
data Direction = GoingUp | GoingDown | None deriving (Show, Eq)

data BallParticle = BallParticle
   { lifespan :: Float
   , format :: Ball
   } deriving Show

data Ball = Ball
   { ballPos :: Position    -- ^ Pong ball (x, y) location
   , ballDiam :: Float      -- ^ Pong ball radius
   , ballVel :: Position    -- ^ Pong ball (x, y) velocity
   , ballColor :: Color
   , ballShape :: Picture
   } deriving Show

data Player = Player
   { playerPos :: Position  -- ^ Player (x, y) location
   , playerHeight :: Height -- ^ Player height
   , playerWidth :: Width   -- ^ Player width
   , playerVel :: Float     -- ^ Player y velocity
   , playerColor :: Color
   , playerShape :: Picture
   , playerMov :: (Movement, Direction)
   } deriving Show

data Wall = Wall
   { wallPos :: Position
   , wallHeight :: Height
   , wallWidth :: Width
   , wallColor :: Color
   , wallShape :: Picture
   } deriving Show

data Game = Game
   { gameBall :: Ball
   , wall1 :: Wall
   , wall2 :: Wall
   , player1 :: Player
   , player2 :: Player
   , pQueue :: BPQueue
   , scores :: (Int, Int)
   } deriving Show

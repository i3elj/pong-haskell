module DataModels where

import Graphics.Gloss

-- | Data Models
type Diameter = Float
type Position = (Float, Float)
type Height = Float
type Width = Float

data Ball = Ball
   { ballPos :: Position -- ^ Pong ball (x, y) location
   , ballDiam :: Float -- ^ Pong ball radius
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
   , wall1 :: Wall
   , wall2 :: Wall
   , player1 :: Player
   , player2 :: Player
   } deriving Show

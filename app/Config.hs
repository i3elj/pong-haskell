module Config where

import Graphics.Gloss

import Types

fps :: Int
fps = 60

width, height, offset :: Int
width  = 400
height = 400
offset = 100

window :: Display
window = InWindow "dev:pong" (width, height) (offset, offset)

background :: Color
background = black


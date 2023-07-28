module Config where

import Graphics.Gloss

import Types

fps :: Int
fps = 80

width, height, offset :: Int
width  = 1620
height = 860
offset = 100

window :: Display
window = InWindow "dev:pong" (width, height) (offset, offset)

background :: Color
background = black

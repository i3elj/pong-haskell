module Game where

import Graphics.Gloss

import Types
import Ball
import Wall
import Player
import Particles

initialState :: Game
initialState = Game
   { gameBall = ball
   , wall1 = topWall
   , wall2 = bottomWall
   , player1 = playerOne 
   , player2 = playerTwo
   , pQueue = particleQueue
   }

checkForPoints :: Game -> Game
checkForPoints game@(Game b w1 w2 p1 p2 pq) = game

range :: [(Float, Float)]
range = [(0, x) | x <- [-200..210], fromIntegral (mod (round x) 10) == 0]

toTupleList :: [(Float, Float)] -> [((Float, Float), (Float, Float))]
toTupleList (x:xs) | tail xs == [] = []
                   | otherwise = [(x,head xs)] ++ toTupleList (tail xs)

makeLine :: ((Float, Float), (Float, Float)) -> Picture
makeLine ((a,as),(b,bs)) = color (greyN 0.5) $ line [a', b'] where
   a' = (a, as)
   b' = (b, bs)

renderDelimeter :: [Picture]
renderDelimeter = map makeLine $ toTupleList range

renderGame :: Game -> Picture
renderGame game@(Game b w1 w2 p1 p2 pQ) =
   pictures [ pictures renderDelimeter
            , renderBParticle pQ
            , renderWalls     [w1, w2]
            , renderPlayers   [p1, p2]
            , renderBall      b
            ]

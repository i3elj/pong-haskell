module Game where

import Graphics.Gloss

import Config
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
   , scores = (0, 0)
   }

checkForPoints :: Game -> Game
checkForPoints game@(Game b w1 w2 p1 p2 pq sc)
   | ballx < fromIntegral (- width) / 2 = game { scores = setScore 2 sc }
   | ballx > fromIntegral width / 2     = game { scores = setScore 1 sc }
   | otherwise = game
   where ballx = fst $ ballPos b

setScore :: Int -> (Int, Int) -> (Int, Int)
setScore 1 (a, b) = (a + 1, b)
setScore 2 (a, b) = (a, b + 1)

renderScores :: (Int, Int) -> Picture
renderScores (a,b) = pictures [ scale 0.2 0.2
                              $ translate 0 0 $ translate (-140) 800
                              $ color white
                              $ text (show a)
                              , scale 0.2 0.2
                              $ translate 0 0 $ translate (70) 800
                              $ color white
                              $ text (show b) ]

resetGameBall :: Game -> Game
resetGameBall game@(Game b w1 w2 p1 p2 pq sc)
   | ballx < fromIntegral (- width) / 2 = game { gameBall = resetBall b }
   | ballx > fromIntegral width / 2 = game { gameBall = resetBall b }
   | otherwise = game
   where ballx = fst $ ballPos b

resetGamePlayers :: Game -> Game
resetGamePlayers game@(Game b w1 w2 p1 p2 pq sc)
   | ballx < fromIntegral (- width) / 2
      = game { player1 = resetPlayer p1 1, player2 = resetPlayer p2 2 }
   | ballx > fromIntegral width / 2
      = game { player1 = resetPlayer p1 1, player2 = resetPlayer p2 2 }
   | otherwise = game
   where ballx = fst $ ballPos b

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
renderGame game@(Game b w1 w2 p1 p2 pQ sc) =
   pictures [ pictures renderDelimeter
            , renderScores sc
            , renderBParticle pQ
            , renderWalls     [w1, w2]
            , renderPlayers   [p1, p2]
            , renderBall      b
            ]

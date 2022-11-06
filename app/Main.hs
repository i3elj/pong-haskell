{-# LANGUAGE TemplateHaskell #-}

module Main where

import Config
import Types
import Ball
import Wall
import Player
import Game
import Particles
import Inputs

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play window background fps initialState renderGame handleInput update
   where update :: Float -> Game -> Game
         update delta = updateBPQueue
                      . addNewParticlesToGame
                      . checkForPoints
                      . paddleBounce
                      . wallBounce
                      . movePlayers
                      . moveBall delta

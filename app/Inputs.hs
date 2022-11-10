module Inputs where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Types
import Game
import Player
import Ball

handleInput :: Event -> Game -> Game
handleInput (EventKey key upDown _ _) game@(Game ball _ _ p1 p2 _ _)
   -- Start game
   | key == (SpecialKey KeySpace) = game { gameBall = setBallVel ball (150, 0) }
   -- Left Paddle
   | key == (SpecialKey KeyUp) && upDown == Up
      = game { player1 = setPlayerState p1 (IsStill, None) }
   | key == (SpecialKey KeyUp) && upDown == Down
      = game { player1 = setPlayerState p1 (IsMoving, GoingUp) }

   | key == (SpecialKey KeyDown) && upDown == Up
      = game { player1 = setPlayerState p1 (IsStill, None) }
   | key == (SpecialKey KeyDown) && upDown == Down
      = game { player1 = setPlayerState p1 (IsMoving, GoingDown) }

   -- -- Right Paddle
   | key == (SpecialKey KeyLeft) && upDown == Up
      = game { player2 = setPlayerState p2 (IsStill, None) }
   | key == (SpecialKey KeyLeft) && upDown == Down
      = game { player2 = setPlayerState p2 (IsMoving, GoingUp) }

   | key == (SpecialKey KeyRight) && upDown == Up
      = game { player2 = setPlayerState p2 (IsStill, None) }
   | key == (SpecialKey KeyRight) && upDown == Down
      = game { player2 = setPlayerState p2 (IsMoving, GoingDown) }

   | otherwise = game

handleInput _ game = game

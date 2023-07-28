module Inputs where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Types
import Game
import Player
import Ball

handleInput :: Event -> Game -> Game
handleInput (EventKey key upDown _ _) g@(Game ball _ _ p1 p2 _ _) = game
   where
      game = case (key, upDown) of
         (SpecialKey KeySpace, _)      -> g { gameBall = setBallVel ball (250, 0) }
         -- player 1
         (Char 'a', Up)                -> g { player1 = setPlayerState p1 (IsStill  , None      )}
         (Char 'a', Down)              -> g { player1 = setPlayerState p1 (IsMoving , GoingUp   )}
         (Char 'e', Up)                -> g { player1 = setPlayerState p1 (IsStill  , None      )}
         (Char 'e', Down)              -> g { player1 = setPlayerState p1 (IsMoving , GoingDown )}
         -- player 2
         ((SpecialKey KeyLeft), Up)    -> g { player2 = setPlayerState p2 (IsStill  , None      )}
         ((SpecialKey KeyLeft), Down)  -> g { player2 = setPlayerState p2 (IsMoving , GoingUp   )}
         ((SpecialKey KeyRight), Up)   -> g { player2 = setPlayerState p2 (IsStill  , None      )}
         ((SpecialKey KeyRight), Down) -> g { player2 = setPlayerState p2 (IsMoving , GoingDown )}
         _ -> g

handleInput _ game = game

-- handleInput (EventKey key upDown _ _) game@(Game ball _ _ p1 p2 _ _)
--    -- Start game
--    | key == (SpecialKey KeySpace) = game { gameBall = setBallVel ball (250, 0) }

--    -- Left Paddle
--    -- UP
--    | key == Char 'a' && upDown == Up
--       = game { player1 = setPlayerState p1 (IsStill, None) }
--    | key == Char 'a' && upDown == Down
--       = game { player1 = setPlayerState p1 (IsMoving, GoingUp) }
--    -- DOWN
--    | key == Char 'e' && upDown == Up
--       = game { player1 = setPlayerState p1 (IsStill, None) }
--    | key == Char 'e' && upDown == Down
--       = game { player1 = setPlayerState p1 (IsMoving, GoingDown) }
--    -- ROTATION
--    -- | key == (_ _) && _ == _
--    --    = game { player1 = rotatePlayerOne p1 2.0 }

--    -- -- Right Paddle
--    | key == (SpecialKey KeyLeft) && upDown == Up
--       = game { player2 = setPlayerState p2 (IsStill, None) }
--    | key == (SpecialKey KeyLeft) && upDown == Down
--       = game { player2 = setPlayerState p2 (IsMoving, GoingUp) }

--    | key == (SpecialKey KeyRight) && upDown == Up
--       = game { player2 = setPlayerState p2 (IsStill, None) }
--    | key == (SpecialKey KeyRight) && upDown == Down
--       = game { player2 = setPlayerState p2 (IsMoving, GoingDown) }

--    | otherwise = game

-- handleInput _ game = game

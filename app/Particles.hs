module Particles where

import Graphics.Gloss

import Ball
import Types

particleQueue :: BPQueue
particleQueue = []

createBParticle :: Game -> BPQueue
createBParticle g@(Game (Ball pos diam vel col shp) _ _ _ _ pq _)
   = pq ++ [ BallParticle { lifespan = 120, format =
                     Ball { ballPos = pos , ballDiam = (diam - 4)
                          , ballVel = vel , ballColor = makeColor 0.2 0.2 0.2 0.65
                          , ballShape = shp } } ]

addNewParticlesToGame :: Game -> Game
addNewParticlesToGame game@(Game _ _ _ _ _ pq _)
   = game { pQueue = createBParticle game }

updateBParticle :: BallParticle -> BallParticle
updateBParticle bp@(BallParticle ls (Ball _ diam _ col _))
   = setBParticleInfo bp (ls - 1.5) (diam - 0.125) (rgbaOfColor col)

updateBPQueue :: Game -> Game
updateBPQueue game@(Game _ _ _ _ _ pq _)
   = game { pQueue = filter (\x -> lifespan x > 0) $ map updateBParticle pq }

setBParticleInfo :: BallParticle -> Float -> Diameter -> CustomColor -> BallParticle
setBParticleInfo ballp@(BallParticle _ ball@(Ball pos diam vel col shp))
                 ls' diam' (r,g,b,a)
   = ballp { lifespan = ls' , format =
         Ball { ballPos = pos , ballDiam = diam'
              , ballVel = vel , ballColor = makeColor r g b (a - 0.015)
              , ballShape = circleSolid diam'
              }}

renderBParticle :: BPQueue -> Picture
renderBParticle bpqueue =
   pictures [ translate (fst $ ballPos $ format bp) (snd $ ballPos $ format bp) 
            $ color (ballColor $ format bp)
            $ ballShape $ format bp | bp <- bpqueue ]

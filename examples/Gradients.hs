{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Main where

import           Diagrams.Prelude
import           Diagrams.Backend.NanoVG.CmdLine

stops = mkStops [(orange, 0, 1), (blue, 1, 1)]
g     = defaultRG & _RG . rGradStops .~ stops

stops' = mkStops [(lightskyblue, 0, 1), (darkgreen, 1, 0.5)]
h      = mkLinearGradient stops ((-10) ^& (0)) (10 ^& (0)) GradPad
h'     = mkLinearGradient stops' ((-50) ^& 0) (50 ^& 0) GradPad

linear = mkLinearGradient (mkStops [(black,0,1), (white,1,1)]) (0 ^& (-300)) (0 ^& 300) GradPad
radial = mkRadialGradient (mkStops [(orange,   0.0, 0.4)
                                  , (teal,   1, 1)])
                          (0 ^& 0) 50
                          (0 ^& 0) 80 GradRepeat


s  = square 100 # fillTexture h # lineTexture h' # lw ultraThick # scaleX 1.5
s' = square 100 # fillTexture radial # lineTexture h' # lw ultraThick # scaleX 1.5

e1 = vcat' (with & sep .~ 35) [s', s # rotateBy (1/16), s # rotateBy (1/8)]
e2 = vcat' (with & sep .~ 35) [s # rotateBy (3/16), s' # rotateBy (1/4), s # rotateBy (5/16)]
e3 = vcat' (with & sep .~ 35) [s # rotateBy (3/8), s # rotateBy (7/16), s' # rotateBy (1/2)]
example = hcat' (with & sep .~ 25) [e1, e2, e3]

main = defaultMain $ (example # centerXY # pad 1.1) <> (square 600 # fillTexture linear)

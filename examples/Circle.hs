{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.NanoVG.CmdLine

main = mainWith ((square 200 # fc blue <> circle 300)
              # lw thick 
              # fc red
              # frame 10 :: Diagram NanoVG)

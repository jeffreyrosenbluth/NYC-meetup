{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

c1 = circle 0.5 # fc steelblue
c2 = circle 1   # fc orange

diagram :: Diagram B
diagram = beside (1 ^& 1) c1 c2 # showOrigin

main = mainWith $ frame 0.1 diagram

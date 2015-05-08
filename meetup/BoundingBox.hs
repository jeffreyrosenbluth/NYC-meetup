{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

c1 = circle 0.5 # fc steelblue <> square 1 # lc red
c2 = circle 1   # fc orange <> square 2 # lc red

diagram :: Diagram B
diagram = beside (1 ^& 1) c1 c2

main = mainWith $ frame 0.1 diagram

{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

c1 = circle 0.5 # lw none # showOrigin
c2 = circle 1   # fc orange
c3 = circle 0.5 # fc steelblue # showOrigin

diagram :: Diagram B
diagram = (c1 ||| c2) === c3

main = mainWith $ frame 0.1 diagram

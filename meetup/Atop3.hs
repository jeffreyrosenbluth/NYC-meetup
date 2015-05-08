{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

c1 = circle 0.5 # fc steelblue   # showOrigin
c2 = circle 1   # fc orange # showOrigin

diagram :: Diagram B
diagram = (c1 ||| c2)

main = mainWith $ frame 0.1 diagram

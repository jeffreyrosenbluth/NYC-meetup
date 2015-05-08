{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

diagram :: Diagram B
diagram = circle 1 # lw ultraThick # lc purple # fc orange

main = mainWith $ frame 0.1 diagram

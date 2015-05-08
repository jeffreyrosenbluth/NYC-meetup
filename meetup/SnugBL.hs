{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams.Prelude
import           Diagrams.Backend.SVG.CmdLine

h = hexagon 1 # fc lightgreen

sOrigin = showOrigin' (with & oScale .~ 0.04)

diagram :: Diagram B
diagram = h # snugBL # sOrigin

main = mainWith $ frame 0.1 diagram

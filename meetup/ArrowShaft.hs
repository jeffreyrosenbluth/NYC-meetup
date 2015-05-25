{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

sPt = p2 (0.0, 0.0)
ePt = p2 (1.0, 0.0)

shaft = arc xDir (-1/2 @@ turn)

diagram :: Diagram B
diagram = arrowBetween'
                   (with & lengths .~ (normalized 0.15)
                         & arrowHead .~ thorn
                         & arrowTail .~ thorn'
                         & arrowShaft .~ shaft
                         & shaftStyle %~ lwN 0.02
                         & headStyle %~ fc blue
                         & tailStyle %~ fc red) sPt ePt


main = mainWith $ frame 0.5 diagram

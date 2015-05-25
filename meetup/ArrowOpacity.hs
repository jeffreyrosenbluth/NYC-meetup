{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

sPt = p2 (0.0, 0.20)
ePt = p2 (1.0, 0.27)

diagram :: Diagram B
diagram = arrowBetween'
                   (with & lengths .~ (normalized 0.25)
                         & arrowHead .~ spike
                         & arrowTail .~ spike'
                         & shaftStyle %~ lwN 0.03 . lc orange
                         & headStyle %~ fc orange . opacity 0.5
                         & tailStyle %~ fc orange . opacity 0.75) sPt ePt
          <> rect 1 0.25 # fc black # alignBL # translateY (-0.01)


main = mainWith $ frame 0.1 diagram

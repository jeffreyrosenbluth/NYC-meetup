{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

illustrateEnvelope v d
  = mconcat
    [arrowAt' (with & arrowHead .~ spike) origin v
    , origin ~~ b
      # lc green # lw veryThick
    , p1 ~~ p2
      # lc red
    ]
    where
      b  = envelopeP v d
      v' = 1.5 *^ signorm v
      p1 = b .+^ (rotateBy (1/4) v')
      p2 = b .+^ (rotateBy (-1/4) v')

d1 :: Path V2 Double
d1 = circle 1

d2 :: Path V2 Double
d2 = (pentagon 1 === roundedRect 1.5 0.7 0.3)

diagram :: Diagram B
diagram = (stroke d1 # showOrigin <> illustrateEnvelope (r2 (-0.5, 0.3)) d1)
      ||| (stroke d2 # showOrigin <> illustrateEnvelope (r2 (0.5, 0.2)) d2
                                  <> illustrateEnvelope (r2 (0.5, -0.1)) d2)

main = mainWith $ frame 0.1 diagram

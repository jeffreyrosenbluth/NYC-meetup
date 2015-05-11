{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

illustrateEnvelope v d
  = mconcat
    [arrowAt' (with & arrowHead .~ spike) origin v
    -- , origin ~~ b
    --   # lc green # lw veryThick
    , p1 ~~ p2
      # lc red
    ]
    where
      b  = envelopeP v d
      v' = 1.5 *^ signorm v
      p1 = b .+^ (rotateBy (1/4) v')
      p2 = b .+^ (rotateBy (-1/4) v')

c1, c2 :: Path V2 Double
c1 = square 1
c2 = square 1

diagram :: Diagram B
-- diagram = beside (1.25 ^& 1)
--                  (stroke c1 # showOrigin <> illustrateEnvelope ((sqrt 2 / 4) ^& (sqrt 2 / 4)) c1)
--                  (stroke c2 # showOrigin <> illustrateEnvelope ((-sqrt 2 / 2) ^& (-sqrt 2 / 2)) c2)
diagram = beside (1 ^& 1) (stroke c1 # showOrigin) (stroke c2 # showOrigin)
      --  <> illustrateEnvelope (0.385 *^ (1.5 ^& 1)) c1
      --  <> moveOriginBy (negated (0.77 *^ (1.5 ^& 1))) (illustrateEnvelope ((-0.385) *^ (1.5 ^& 1)) c2)

main = mainWith $ frame 0.1 diagram

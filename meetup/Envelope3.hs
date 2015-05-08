{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

d1, d2 :: Diagram B
d1 = circle 1
d2 = (pentagon 1 === roundedRect 1.5 0.7 0.3)

s1,s2 :: Diagram B
s1 = square 1
s2 = square 1 # alignT

diagram = hsep 1
  [ s1 # showEnvelope' (with & ePoints .~ 360) # showOrigin
  , centerY $ s2 # showEnvelope' (with & ePoints .~ 360) # showOrigin
  ]

main = mainWith $ frame 0.1 diagram

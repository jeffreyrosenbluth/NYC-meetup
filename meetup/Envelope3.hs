{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

d1, d2 :: Diagram B
d1 = circle 1
d2 = (pentagon 1 === roundedRect 1.5 0.7 0.3)

diagram = hsep 1
  [ (d1 ||| d2)          # showEnvelope' (with & ePoints .~ 360) # showOrigin
  , (d1 ||| d2) # center # showEnvelope' (with & ePoints .~ 360) # showOrigin
  ]

main = mainWith $ frame 0.1 diagram

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}

import           Diagrams.Prelude
import           Diagrams.Backend.SVG.CmdLine

import Diagrams.Transform.ScaleInv
import Control.Lens ((^.))

class Drawable d where
  draw :: d -> Diagram B

instance (n ~ Double) => Drawable (QDiagram B V2 n Any) where
  draw = id

instance Drawable a => Drawable (ScaleInv a) where
  draw = draw . (^. scaleInvObj)

instance (Drawable a, Drawable b) => Drawable (a,b) where
  draw (x,y) = draw x <> draw y

arrowhead, shaft :: Diagram B
arrowhead = triangle 0.5 # fc black # rotateBy (-1/4)

shaft = origin ~~ p2 (3, 0) # lw thick
arrow1 = (shaft,          arrowhead       # translateX 3.15)
arrow2 = (shaft, scaleInv arrowhead unitX # translateX 3.15)
showT tr = draw (arrow1 # transform tr)
       ||| strutX 1
       ||| draw (arrow2 # transform tr)

diagram :: Diagram B
diagram = vcat' (with & sep .~ 1.5)
            (map (centerX . showT)
              [ scalingX 1
              , scalingX (2)
              -- , scalingY 2
              -- , scalingX (1/2) <> rotation (-1/12 @@ turn)
              ])

main = mainWith $ frame 0.1 example1

example1 :: Diagram B
example1 = draw arrow1 # scale (2/3.45) <> (circle 1 # showOrigin ||| square 2 # showOrigin)

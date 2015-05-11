{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Diagrams.ThreeD.Transform  (translateZ)
import Diagrams.ThreeD.Projection
import Diagrams.LinearMap         (amap)
import Linear.Matrix              ((!*!))

box :: Path V3 Double
box = Path [f p1 ~~ f p2 | p1 <- ps, p2 <- ps, quadrance (p1 .-. p2) == 4]
  where
    ps = getAllCorners $ fromCorners (-1) 1
    f  = fmap fromIntegral

roof :: Path V3 Double
roof = Path
  [ mkP3 1 1 1       ~~ mkP3 1 0 1.4
  , mkP3 1 (-1) 1    ~~ mkP3 1 0 1.4
  , mkP3 1 0 1.4     ~~ mkP3 (-1) 0 1.4
  , mkP3 (-1) 1 1    ~~ mkP3 (-1) 0 1.4
  , mkP3 (-1) (-1) 1 ~~ mkP3 (-1) 0 1.4
  ]

door :: Path V3 Double
door = fromVertices
  [ mkP3 1 (-0.2) (-1)
  , mkP3 1 (-0.2) (-0.4)
  , mkP3 1 (0.2) (-0.4)
  , mkP3 1 (0.2) (-1)
  ]
house = door <> roof <> box
-- Perspective projection
-- these bits are from Linear.Projection
m  = lookAt (V3 3.4 4 2.2) zero unitZ
pm = perspective (pi/3) 0.8 1 3 !*! m
pd = m44Deformation pm
perspectiveHouse = stroke $ deform pd (translateZ (-1) house)
-- Orthogonal projection
am = lookingAt (mkP3 3.4 4 2.2) zero zDir
orthogonalHouse = stroke $ amap am house
-- Isometric projection (specialised orthogonal)
isometricHouse = stroke $ isometricApply zDir house

example :: Diagram B
example = hsep 1 . map (sized (mkHeight 3) . centerXY) $
  [ perspectiveHouse, orthogonalHouse, isometricHouse ]

main = mainWith $ frame 0.1 example

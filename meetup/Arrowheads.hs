{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

drawHead h = arrowAt' (with & arrowHead .~ h & shaftStyle %~ lw none
                            & headLength .~ local 0.5)
        origin (r2 (1, 0))
     <> square 0.5 # alignL # lw none # frame 0.1

drawTail t = arrowAt' (with  & arrowTail .~ t & shaftStyle %~ lw none
                             & arrowHead .~ noHead & tailLength .~ local 0.5)
        origin (r2 (1, 0))
     <> square 0.5 # alignL # lw none # frame 0.1

diagram :: Diagram B
diagram = (drawHead tri ||| strutX 0.25 ||| drawHead dart ||| strutX 0.25 ||| drawHead halfDart)
          ===
          strutY 0.5
          ===
          (drawHead spike ||| strutX 0.25 ||| drawHead thorn ||| strutX 1 ||| drawTail quill)

diagram2 :: Diagram B
diagram2 = rect 2 0.2 # fc black <> (translateX 0.55 $ (drawHead dart))

main = mainWith $ diagram2 # frame 0.1

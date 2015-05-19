{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

sierpinski 1 = hexagon 1 # lw thick # lc purple # fc orange
sierpinski n =    s
                 ===
              (s ||| s) # centerX
  where s = sierpinski (n-1)

diagram :: Diagram B
diagram = sierpinski 5

main = mainWith $ diagram # frame 0.1

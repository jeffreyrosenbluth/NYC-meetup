{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

sierpinski shape color alignment 1 = shape 1 # fc color
sierpinski shape color alignment n =
                  s
                 ===
              (s ||| s) # alignment
  where s = sierpinski shape color alignment (n-1)

diagram :: Diagram B
diagram = sierpinski 7

main = mainWith $ diagram # frame 0.1

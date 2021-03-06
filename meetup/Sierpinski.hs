{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

sierpinski 1 = triangle 1
sierpinski n =    s
                 ===
              (s ||| s) # centerX
  where s = sierpinski (n-1)

diagram :: Diagram B
diagram = sierpinski 7

main = mainWith $ diagram # frame 0.1

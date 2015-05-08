{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Diagrams.Prelude
import           Diagrams.Backend.SVG.CmdLine
import           Data.Maybe (fromMaybe)

stripes :: Diagram B
stripes = (strutX 32
      ||| stripe
      ||| strutX 32
      ||| stripe
      ||| strutX 30) # centerXY
  where stripe = square 100 # scaleX 0.03 # fc white # lc white

main = mainWith $ stripes <> square 100 # fc black

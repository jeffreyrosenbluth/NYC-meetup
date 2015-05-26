{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Data.List.Split (chunksOf)

a = circle 1 # named "circle" # showOrigin
b = square 2 # named "square" # showOrigin
r = fc black $ rect 2 0.1 ||| (triangle 0.3 # rotateBy (1/12)) # named "arrow"
c = fc blue . lw none $ square 0.3 # rotateBy (1/8) # named "<>"
d = fc blue . lw none $ square 0.3 # rotateBy (1/8) # named "scale"

example :: Diagram B
example = connectOutside' arrow1 "circle" "<>"
        . connectOutside' arrow1 "square" "<>"
        . connectOutside' arrow1 "arrow" "scale"
        . connectOutside' arrow1 "<>" "scale"
        $ vsep 3 [d, vsep 4 [c, hsep 1 [a, b, r] # centerX]]
  where
  arrow1 = with & arrowHead .~ noHead & gaps .~ 10


-- example = connect'        arrow1 "1" "2"
--         . connect'        arrow2 "4" "3"
--         . connect'        arrow3 "1" "6"
--         . connectOutside' arrow4 "4" "8"
--         . connect'        arrow5 "9" "5"
--         . connectOutside' arrow6 "8" "9"
--         . connectOutside' arrow7 "8" "7"
--         $ cGrid
--  where
--     -- The arrows
--     arrow1 = with & arrowHead  .~ dart & headLength .~ veryLarge
--                   & arrowTail  .~ quill & shaftStyle %~ lw thick
--                   & arrowShaft .~ shaft0 & headStyle  %~ fc blue
--                   & tailStyle  %~ fc red & tailLength .~ large
--
--     arrow2 = with & arrowHead  .~ dart & headLength .~ large
--                   & arrowTail  .~ dart' & tailLength .~ large
--                   & shaftStyle %~ lw thin & arrowShaft .~ shaft1
--
--     arrow3 = with & arrowHead  .~ thorn & headLength .~ large
--                   & arrowShaft .~ quartercircle & arrowTail  .~ noTail
--                   & gaps .~ normal
--
--     arrow4 = with & arrowHead  .~ dart & arrowTail .~ dart'
--                   & headLength .~ large   & tailLength .~ large
--                   & arrowShaft .~ shaft2  & headStyle  %~ fc teal
--                   & tailStyle  %~ fc teal & shaftStyle %~ lw thick . lc teal
--
--     arrow5 = with & arrowTail  .~ spike' & tailLength .~ large
--                   & arrowShaft .~ semicircle & arrowHead  .~ spike
--                   & headLength .~ veryLarge  & headStyle  %~ fc darkorange
--                   & tailStyle  %~ fc darkorange
--                   & shaftStyle %~ lw veryThick . lc navy
--
--     arrow6 = with & arrowHead  .~ tri & arrowTail .~ tri'
--                   & headLength .~ large
--                   & headStyle  %~ fc black . opacity 0.5
--                   & tailStyle  %~ fc black . opacity 0.5
--                   & shaftStyle %~ lw thick
--                   & shaftStyle %~ dashingN [0.01,0.02,0.03,0.01] 0
--
--     arrow7 = arrow6 & arrowHead .~ tri & arrowTail .~ tri'

main = mainWith $ frame 0.2 example

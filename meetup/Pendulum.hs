{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Diagrams.Prelude
import           Diagrams.Backend.Rasterific.CmdLine
import           Data.Maybe (fromMaybe)

radial :: Texture Double
radial = mkRadialGradient (mkStops [(white,0,1), (black,1,1)])
                          ((-0.25) ^& (0.25)) 0.1 (0 ^& 0) 1.5
                          GradPad

pend :: V2 Double -> Diagram B
pend v = bob # translate (e .-. origin) <> rod
  where
    ellipsePath :: Diagram B
    ellipsePath = circle 25 # scaleX 1.5
    bob  = scale size $ circle 1 # fillTexture radial # lw none
    size = 3.5 * sqrt (2 - y)
    rod  = arrowBetween' (with & shaftStyle %~ lw thick # lc gray
                               & arrowHead .~ noHead) s e
    s = (0 ^& 50)
    e = fromMaybe origin (rayTraceP origin v ellipsePath)
    (x, y) = unr2 v -- v ^. r2Iso

stripes :: Diagram B
stripes = (strutX 32
      ||| stripe
      ||| strutX 32
      ||| stripe
      ||| strutX 30) # center
  where stripe = square 100 # scaleX 0.03 # fc white # lc white

mkFrame :: V2 Double -> Diagram B
mkFrame v
  | (snd $ unr2 v) > 0 = stripes <> pend v
  -- |  v ^. (r2Iso . _2) > 0 ...
  | otherwise = pend v <> stripes

dias = map mkFrame vs
  where vs = [fromDirection $ rotateBy(a/100) xDir | a <- [0..99]]

delays = take 100 (repeat 3)

gif :: [(Diagram B, Int)]
gif = zip dias delays

main = mainWith $ gif

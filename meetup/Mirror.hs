{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import           Control.Monad                 (replicateM)
import           Control.Monad.Random
import           Data.Colour.Palette.ColorSet
import           Data.List                     (zipWith, zipWith3)
import           Diagrams.Prelude
import           Diagrams.Backend.SVG.CmdLine
import           System.Random

sizeValue :: (RandomGen g) => Rand g Double
sizeValue = getRandomR (0.05, 0.25)

coordValue :: (RandomGen g) => Rand g Double
coordValue = getRandomR (-0.5, 0.5)

confetti :: Int -> Rand StdGen (Diagram B)
confetti n = do
  ss <- replicateM n sizeValue   -- radius
  cs <- replicateM n getRandom   -- color index
  as <- replicateM n getRandom   -- opacity
  xs <- replicateM n coordValue  -- x coordinate
  ys <- replicateM n coordValue  -- y coordinate
  let mkCirc :: Double -> Int -> Double -> Diagram B
      mkCirc s c a = circle s # fc (webColors c)
                              # opacity a # lw none
      pos  = zipWith mkP2 xs ys
      conf = zipWith3 mkCirc ss cs as
  return $ position (zip pos conf)

mkConfetti :: Int -> (StdGen -> Diagram B)
mkConfetti n = evalRand $ confetti n

isoceles :: (TrailLike t, V t ~ V2) => Int -> t
isoceles n = polygon
  (def & polyType   .~ PolySides [deg1 @@ turn, deg2 @@ turn] [1,1]
       & polyOrient .~ OrientH )
  where
  deg1 = 1/2 - (1 / fromIntegral n)
  deg2 = 1/2 - 1/2 * deg1

mkTriangle :: Int -> Diagram B -> Diagram B
mkTriangle n = clipped tri # lw none
  where
  tri = isoceles n # rotateBy (-1/4 - 1 / (2 * fromIntegral n))

iterateIdx :: Integral i => (i -> a -> a) -> a -> [a]
iterateIdx f t = go f t 0
  where
    go f t i = let t' = f i t
               in  t': go f t' (i + 1)

kaleidoscope :: Diagram B -> Int -> Diagram B
kaleidoscope d n = mconcat . take n $ iterateIdx next tri
  where
    tri    = alignBR $ mkTriangle n d
    next t = reflectAbout
             (0 ^& 0)
             (rotateBy (-fromIntegral t / fromIntegral n) xDir)

diagram :: Diagram B
diagram = mkConfetti 50 (mkStdGen 0)

diagram2 :: Diagram B
diagram2 = mkTriangle 6 . mkConfetti 50 $ mkStdGen 0

diagram3 :: Diagram B
diagram3 = kaleidoscope d 10
  where d = mkConfetti 50 $ mkStdGen 0

main = mainWith $ frame 0.1 diagram3

------------------------------------------------------------
-- Drawing trace diagrams
------------------------------------------------------------

import Data.Default.Class
import Control.Lens ((^.))
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Maybe (fromMaybe)

thingyT :: Trail V2 Double
thingyT =
  fromOffsets
    [ 3 *^ unitX, 3 *^ unitY, 2 *^ unit_X, 1 *^ unit_Y
    , 1 *^ unitX, 1 *^ unit_Y, 2 *^ unit_X, 1 *^ unit_Y ]

thingy = strokeTrail thingyT

data TraceDiaOpts
  = TDO { traceShape :: Diagram B
        , basePt     :: P2 Double
        , dirV       :: V2 Double
        , sFilter    :: [Double] -> [Double]
        , drawV      :: Bool
        }

instance Default TraceDiaOpts where
  def = TDO { traceShape = thingy
            , basePt     = pointB
            , dirV       = 0.3 ^& 0.5
            , sFilter    = id
            , drawV      = False
            }

pointA = 1 ^& (0.5)
pointB = 2.1 ^& 0.75
pointC = 1 ^& 1.2

dot' = circle 0.05 # lw none

mkTraceDia :: TraceDiaOpts -> Diagram B
mkTraceDia tdo = mconcat
  [ mconcat $ map (place (dot' # fc red)) pts
  , if drawV tdo then resultArrow else mempty
  -- , arrowAt (basePt tdo) (dirV tdo) # lc blue
  , dot' # fc blue # moveTo (basePt tdo)
  , traceLine (basePt tdo) maxPosPt
  , traceLine (basePt tdo) minNegPt
  , traceShape tdo
  ]
  # centerXY # pad 1.1
  where
    ss  = sFilter tdo . getSortedList
        $ appTrace (traceShape tdo ^. trace) (basePt tdo) (dirV tdo)
    pts = map mkPt ss
    mkPt s = basePt tdo .+^ (s *^ dirV tdo)
    maxPosPt = (mkPt <$>) . safeLast $ filter (>0) ss
    minNegPt = (mkPt <$>) . safeHead $ filter (<0) ss
    minPt = (mkPt <$>) . safeHead $ ss
    resultArrow = fromMaybe mempty (arrowBetween (basePt tdo) <$> minPt)
      # lc green

safeLast [] = Nothing
safeLast xs = Just $ last xs
safeHead [] = Nothing
safeHead (x:_) = Just x
dropAllBut1 [] = []
dropAllBut1 xs = [last xs]

traceLine _ Nothing = mempty
traceLine p (Just q) = (p ~~ q) # dashingG [0.1,0.1] 0

mkTraceDias :: [TraceDiaOpts] -> Diagram B
mkTraceDias = hcat' (with & sep .~ 1) . map mkTraceDia

mkTraceDiasABC :: TraceDiaOpts -> Diagram B
mkTraceDiasABC tdo = mkTraceDias (map (\p -> tdo { basePt = p }) [pointA, pointB, pointC])

main = mainWith $ mkTraceDiasABC def { drawV = True, sFilter = take 1 . filter (> 0) } # frame 0.1

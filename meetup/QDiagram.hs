type Diagram b = QDiagram b (V b) (N b) Any

newtype QDiagram b v n m = QD (DUALTree (DownAnnots v n)
                              (UpAnnots b v n m)
                               Annotation
                              (QDiaLeaf b v n m))

data QDiaLeaf b v n m = PrimLeaf (Prim b v n)
                      | DelayedLeaf (DownAnnots v n -> n -> n -> QDiagram b v n m)

type UpAnnots b v n m = Deletable (Envelope v n)
                    ::: Deletable (Trace v n)
                    ::: Deletable (SubMap b v n m)
                    ::: Query v n m ::: ()

type DownAnnots v n = (Transformation v n :+: Style v n) ::: Name ::: ()

data Annotation
  = Href String    -- ^ Hyperlink
  | OpacityGroup Double
  deriving Show

type Diagram = (Envelope, DiagramTree)

data DiagramTree
  = QDiaLeaf
  | Trans Transformation DiagramTree
  | Concat [DiagramTree]

data QDiaLeaf
  = PrimLeaf
  | DelayedLeaf (Transformation -> n -> n -> DiagramTree )

-- pseudo-code
diagram = Trans (scale 500) (Concat [arrow, Concat [circle 1, square 2]])

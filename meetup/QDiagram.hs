type Diagram b = QDiagram b (V b) (N b) Any

newtype QDiagram b v n m
  = QD (DUALTree (DownAnnots v n)
                 (UpAnnots b v n m)
                  Annotation
                 (QDiaLeaf b v n m) )
  deriving Typeable

data QDiaLeaf b v n m
  = PrimLeaf (Prim b v n)
  | DelayedLeaf (DownAnnots v n -> n -> n -> QDiagram b v n m)
  deriving Functor

type UpAnnots b v n m = Deletable (Envelope v n)
                    ::: Deletable (Trace v n)
                    ::: Deletable (SubMap b v n m)
                    ::: Query v n m
                    ::: ()

type DownAnnots v n = (Transformation v n :+: Style v n)
                  ::: Name
                  ::: ()

data Annotation
  = Href String    -- ^ Hyperlink
  | OpacityGroup Double
  deriving Show

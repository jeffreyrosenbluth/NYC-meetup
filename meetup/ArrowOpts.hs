data ArrowOpts n
  = ArrowOpts
    { _arrowHead  :: ArrowHT n
    , _arrowTail  :: ArrowHT n
    , _arrowShaft :: Trail V2 n
    , _headGap    :: Measure n
    , _tailGap    :: Measure n
    , _headStyle  :: Style V2 n
    , _headLength :: Measure n
    , _tailStyle  :: Style V2 n
    , _tailLength :: Measure n
    , _shaftStyle :: Style V2 n
    }

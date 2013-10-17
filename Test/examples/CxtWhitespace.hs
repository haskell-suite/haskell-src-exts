-- is not ok
instance ( Eq h) => Eq h

-- is ok
instance ( Eq h, Eq h) => Eq h


{-# LANGUAGE UnboxedTuples #-}

foo :: (a, b) -> (# b , a #)
foo (a, b) =
  case (# b, a #) of
    (# b, a #) -> (# , #) b a

instance Sized a => Sized (Digit a) where
       {-# SPECIALIZE instance Sized (Digit (Elem a)) #-}
       {-# SPECIALIZE instance Sized (Digit (Node a)) #-}
       size xs = foldl (\ i x -> i + size x) 0 xs
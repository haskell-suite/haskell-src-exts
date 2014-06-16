{-# LANGUAGE TypeFamilies #-}
f :: ( Eq a, (a ~ Int) ) => a -> Int
f _ = 3

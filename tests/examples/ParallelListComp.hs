{-# LANGUAGE ParallelListComp #-}

f xs ys zs = [ (x,y,z) | x <- xs | y <- ys, y > 2 | z <- zs ]

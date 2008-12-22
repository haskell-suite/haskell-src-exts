{-# LANGUAGE Rank2Types #-}

test :: Int -> forall a. [a] -> Int
test _ _ = 1

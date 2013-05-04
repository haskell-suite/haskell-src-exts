{-# SPECIALISE [1] x ::
        Integer -> Integer -> Integer,
        Integer -> Int -> Integer,
        Int -> Int -> Int #-}
{-# INLINABLE [1] x #-}    -- See Note [Inlining (^)]
x :: (Num a, Integral b) => a -> b -> a
x = undefined

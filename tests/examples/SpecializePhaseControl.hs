{-# SPECIALISE [1] x ::
        Integer -> Integer -> Integer,
        Integer -> Int -> Integer,
        Int -> Int -> Int #-}
{-# INLINABLE [1] x #-}
x :: (Num a, Integral b) => a -> b -> a
x = undefined

{-# SPECIALISE INLINE [999] y ::
        Integer -> Integer -> Integer,
        Integer -> Int -> Integer,
        Int -> Int -> Int #-}
{-# INLINABLE [1] y #-}
y :: (Num a, Integral b) => a -> b -> a
y = undefined

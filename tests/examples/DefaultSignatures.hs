{-# LANGUAGE DefaultSignatures #-}
class C a where
    f :: a -> a -> Bool
    default f :: (Eq a) => a -> a -> Bool
    f x y = x == y

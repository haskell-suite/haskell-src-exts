{-# LANGUAGE PatternSynonyms, BangPatterns, PolyKinds, DataKinds, GADTs,
 FlexibleContexts, ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}

pattern Single x <- [x]


single :: [a] -> Maybe a
single (Single x) = Just x
single _ = Nothing

pattern Single :: a -> [a]
pattern Single x = [x]


pattern Single :: () => (Show a) => a -> [a]
pattern Single x = [x]

f :: (Show a) => [a] -> a
f (Single x) = x

pattern SinglePair :: (a, a) -> [(a, a)]
pattern SinglePair x = [x]

f :: (Show a) => [(a, a)] -> String
f (SinglePair x) = show x

pattern Q = D

pattern C :: a -> X Maybe (Maybe a)
pattern C x = Y (Just x)


pattern Syn :: forall a b c    . () => () => Int

pattern C :: (Show (a, Bool)) => a -> X Maybe (Maybe (a, Bool))
pattern C x = Y (Just (x, True))

pattern P :: T Bool b
pattern P <- MkT True

pattern D :: a -> T (Maybe a) Bool
pattern D x = MkT (Just x)


pattern P a b = Just (a, b)

pattern Single x = [x]

pattern a :+: b = (a, b)


pattern P x <- MkT 42 x


pattern P x y <- MkT x y

pattern P x <- MkT (f -> True) x

data T where
    MkT :: b -> (b -> Bool) -> T

pattern P x f <- MkT x f

pattern Single x <- [x]

pattern P <- Just True

pattern P = 42


pattern P = ()


pattern Single x <- [x]


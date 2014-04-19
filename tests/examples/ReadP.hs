{-# LANGUAGE TypeOperators #-}
newtype ReadP a = R (forall b . (a -> P b) -> P b)

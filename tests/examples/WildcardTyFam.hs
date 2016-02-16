{-# LANGUAGE TypeFamilies #-}

type family Foo a where
  Foo _ = Int

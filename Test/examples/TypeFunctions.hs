{-# LANGUAGE TypeFamilies, KindSignatures #-}

data Id = Id

type family Rep (f :: * -> *) x :: *

type instance Rep Id x = x

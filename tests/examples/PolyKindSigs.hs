{-# LANGUAGE GADTs          #-}
--{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}

module Test where

data Foo :: * -> * where
  Foo :: Foo a

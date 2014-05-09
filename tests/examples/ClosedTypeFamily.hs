{-# LANGUAGE TypeFamilies, DataKinds #-}

module ClosedTypeFamily where

type family Eq x y where
  Eq x x = True
  Eq x y = False


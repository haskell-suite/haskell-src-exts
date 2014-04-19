{-# LANGUAGE LambdaCase #-}
module LambdaCase where

foo = \case
        Nothing -> e1
        Just e2 -> e2

bar = \    case { _ -> True }

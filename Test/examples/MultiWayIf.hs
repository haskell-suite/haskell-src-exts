{-# LANGUAGE MultiWayIf #-}
module MultiWayIf where

foo = if | test1 -> e1
         | test2 witharg -> e2
         | otherwise -> def

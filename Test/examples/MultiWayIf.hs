{-# LANGUAGE MultiWayIf #-}
module MultiWayIf where

foo = if | test1 -> e1
         | test2 witharg -> e2
         | otherwise -> def

bar = if { | test1 -> if { | test2 -> e1
                           | test3 -> e2 }
           | test4 -> e3
         }

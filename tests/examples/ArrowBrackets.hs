{-# LANGUAGE Arrows #-}
module ArrowBrackets where

foo = proc (x, y) -> (| f (g -< x) |) y

bar = proc (x, y) -> do
  z <- (| f (g -< x) |) y
  (| f (h -< z) |) y

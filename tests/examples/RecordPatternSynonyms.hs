{-# LANGUAGE PatternSynonyms #-}
module ShouldCompile where

pattern Single{x} = [x]

pattern Double{y,z} = (y,z)

pattern More{x} <- (x,_) where
  More x = (x, Nothing)

-- Selector
selector :: Int
selector = x [5]

update :: [String]
update = ["String"] { x = "updated" }

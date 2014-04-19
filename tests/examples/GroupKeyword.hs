-- {-# LANGUAGE TransformListComp #-}
module GroupKeyword where

a = map head $ group $ sort [1..100]

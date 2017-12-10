{-# LANGUAGE TemplateHaskell #-}
module PatternSpliceTest where

foo :: Int -> Bool
foo $( [p| 42 |] ) = True
foo _              = False

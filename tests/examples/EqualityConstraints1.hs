-- https://github.com/haskell-suite/haskell-src-exts/issues/91
{-# LANGUAGE GADTs #-}

one :: a ~ Int => a
one = 1

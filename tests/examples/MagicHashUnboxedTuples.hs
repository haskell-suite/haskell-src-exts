{-# LANGUAGE MagicHash, UnboxedTuples #-}
module MagicHashUnboxedTuples where

f (#x, y #) = (# a#, b #) where
  a# = a#
  b = b

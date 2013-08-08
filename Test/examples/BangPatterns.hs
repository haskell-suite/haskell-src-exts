{-# LANGUAGE BangPatterns #-}
module BangPatterns where

firstnonspace :: Ptr Word8 -> Int -> Int -> IO Int
firstnonspace !ptr !n !m
    | n >= m    = return n
    | otherwise = do w <- peekElemOff ptr n
                     if isSpaceWord8 w then firstnonspace ptr (n+1) m else return n

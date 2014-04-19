{-# LANGUAGE NondecreasingIndentation #-}
module Main where

import Control.Monad

main :: IO ()
main = do
  when ( 2 > 1) $ do
  putStrLn "a"
  putStrLn "b"

nestedDoBlocks = getChar >>= (\c1 -> do
                 getChar >>= (\c2 -> do
                 getChar >>= (\c3 -> return [c1,c2,c3])))

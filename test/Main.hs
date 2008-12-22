
module Main where

import Language.Haskell.Exts
import System.IO
import Control.Monad
import Data.List
import System.Directory


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    files <- getDirectoryContents "examples"
    src <- liftM lines $ readFile "failing.txt"
    sequence_ [check (x `elem` src) ("examples/" ++ x) | x <- files, not $ "." `isPrefixOf` x]
    putStrLn "\nAll tests completed"


check :: Bool -> FilePath -> IO ()
check expected file = do
    res <- parseFile file
    case res of
        ParseOk x -> putChar '.'
        err | expected -> putChar '!'
            | otherwise -> putStrLn $ "\nFailure when parsing " ++ show file ++ "\n" ++ show err


    
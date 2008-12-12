
module Main where

import Language.Haskell.Exts
import System.IO
import Data.List
import System.Directory


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    files <- getDirectoryContents "examples"
    mapM_ check $ ["examples/" ++ x | x <- files, not $ "." `isPrefixOf` x]
    putStrLn "\nAll tests passed"


check :: FilePath -> IO ()
check file = do
    res <- parseFile file
    case res of
        ParseOk x -> putChar '.'
        err -> error $ "\nFailure when parsing " ++ show file ++ "\n" ++ show err

    
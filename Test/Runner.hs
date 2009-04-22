-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
-- Particular files may be selected by supplying arguments.
module Test.Runner ( go ) where

import Language.Haskell.Exts
import System.IO
import Control.Monad
import Data.List
import System.Directory
import System.FilePath


go :: [FilePath] -> IO ()
go testsToRun = do
    hSetBuffering stdout NoBuffering
    files <- if null testsToRun then getDirectoryContents examplesDir else return testsToRun
    src <- liftM lines . readFile $ "Test" </> "failing.txt"
    sequence_ [check (x `elem` src) (examplesDir </> x) | x <- files, not $ "." `isPrefixOf` x]
    putStrLn "\nAll tests completed"


examplesDir :: FilePath
examplesDir = "Test" </> "examples"


check :: Bool -> FilePath -> IO ()
check expected file = do
    res <- parseFile file
    case res of
        ParseOk x | expected -> putStrLn $ "\n<unexpected pass for " ++ file ++ ">"
                  | otherwise -> putChar '.'
        err | expected -> putChar '!'
            | otherwise -> putStrLn $ "\nFailure when parsing " ++ show file ++ "\n" ++ show err


    
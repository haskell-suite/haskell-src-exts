-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
-- Particular files may be selected by supplying their names as arguments.
module Main where

import Language.Haskell.Exts
import System.IO
import Control.Monad
import Data.List
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath


main :: IO ()
main = go =<< getArgs


-- | Run the selected tests - or all of them if the supplied list is empty
go :: [FilePath] -> IO ()
go testsToRun = do
    hSetBuffering stdout NoBuffering
    files <- if null testsToRun then getDirectoryContents examplesDir else return testsToRun
    src <- liftM lines . readFile $ "Test" </> "failing.txt"
    results <- sequence [check (x `elem` src) (examplesDir </> x) | x <- files, not $ "." `isPrefixOf` x]
    putStrLn "\nAll tests completed"
    when (not $ all id results) exitFailure


-- | Where all the tests are to be found
examplesDir :: FilePath
examplesDir = "Test" </> "examples"


-- | Runs the test, and returns True unless there is an unexpected result
check :: Bool -> FilePath -> IO Bool
check expected file = do
    res <- parseFile file
    case res of
        ParseOk x | expected -> putStrLn ("\n<unexpected pass for " ++ file ++ ">") >> return False
                  | otherwise -> putChar '.' >> return True
        err | expected -> putChar '!' >> return True
            | otherwise -> putStrLn ("\nFailure when parsing " ++ show file ++ "\n" ++ show err) >> return False


    
-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
-- Particular files may be selected by supplying their names as arguments.
module Main where

import Language.Haskell.Exts.Annotated
import System.IO
import Control.Monad
import Data.List
import Data.Char
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
    putStrLn "Testing parser:"
    src <- liftM lines . readFile $ "Test" </> "failing.txt"
    results <- sequence [check (x `elem` src) (examplesDir </> x) | x <- files, not $ "." `isPrefixOf` x]
    putStrLn "\nAll parsing tests completed!\n"
    putStrLn "Testing exact printer:"
    pSrc <- liftM lines . readFile $ "Test" </> "printFail.txt"
    pResults <- sequence [roundTrip (x `elem` pSrc) (examplesDir </> x)
                            | x <- files, not (x `elem` src), not $ "." `isPrefixOf` x]
    putStrLn "\nAll printing tests completed!\n"
    when (not $ all id $ results ++ pResults) exitFailure


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


roundTrip :: Bool -> FilePath -> IO Bool
roundTrip expected file = do
    fc <- readFile file
    (ast,cs) <- liftM fromParseResult $ parseFileWithComments (defaultParseMode { parseFilename = file }) file
    let res      = exactPrint ast cs
        xs       = dropWhile (uncurry (==))
                        $ zip (map (reverse . dropWhile isSpace . reverse) $ lines fc)
                              (map (reverse . dropWhile isSpace . reverse) $ lines res)
    case xs of
     [] | expected  -> putStrLn ("\n<unexpected pass for " ++ file ++ ">") >> return False
        | otherwise -> putChar '.' >> return True
     (lfc, lres):_
        | expected  -> putChar '!' >> return True
        | otherwise -> do
            putStrLn $ "Result of print does not match input when printing " ++ show file
            putStrLn $ "First unmatching lines are (line length):"
            putStrLn $ "  Input  (" ++ show (length lfc)  ++ "): " ++ lfc
            putStrLn $ "  Result (" ++ show (length lres) ++ "): " ++ lres
            return False

-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Language.Haskell.Exts.Annotated
import System.IO
import Control.Monad
import Control.Applicative
import Data.List
import Data.Char
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath


main :: IO ()
main = do
    files <- getDirectoryContents examplesDir
    defaultMain . testGroup "Tests" =<<
        sequence [ parserTests files, printerTests files ]


-- | Where all the tests are to be found
examplesDir :: FilePath
examplesDir = "Test" </> "examples"


getParserFailing, getPrinterFailing :: IO [FilePath]
(getParserFailing, getPrinterFailing) = (get "failing.txt", get "printFail.txt")
    where
    get fname = liftM (map (head . words) . lines) . readFile $ "Test" </> fname


parserTests :: [FilePath] -> IO TestTree
parserTests files = testGroup "Parser tests" <$> do
    failing <- getParserFailing
    return [check (x `elem` failing) (examplesDir </> x) | x <- files, not $ "." `isPrefixOf` x]


check :: Bool -> FilePath -> TestTree
check expected file = testCase file $ do
    res <- parseFile file
    case res of
        ParseOk x | expected -> assertFailure $ "Unexpected pass for " ++ file
                  | otherwise -> return ()
        err | expected -> return ()
            | otherwise -> assertFailure $ "Failure when parsing " ++ show file ++ "\n" ++ show err


printerTests :: [FilePath] -> IO TestTree
printerTests files = testGroup "Exact printer tests" <$> do
    parserFailing <- getParserFailing
    printerFailing <- getPrinterFailing
    return
        [ roundTrip (x `elem` printerFailing) (examplesDir </> x)
        | x <- files
        , not $ "." `isPrefixOf` x
        , not $ x `elem` parserFailing ]


roundTrip :: Bool -> FilePath -> TestTree
roundTrip expected file = testCase file $ do
    fc <- readFile file
    pr <- parseFileWithComments (defaultParseMode { parseFilename = file }) file
    case pr of
     ParseOk (ast,cs) -> do
      let res      = exactPrint ast cs
          xs       = dropWhile (uncurry (==))
                        $ zip (map (reverse . dropWhile isSpace . reverse) $ lines fc)
                              (map (reverse . dropWhile isSpace . reverse) $ lines res)
      case xs of
       [] | expected  -> assertFailure $ "Unexpected pass for " ++ file
          | otherwise -> return ()
       (lfc, lres):_
          | expected  -> return ()
          | otherwise -> assertFailure $ unlines
              [ "Result of print does not match input when printing " ++ show file
              , "First unmatching lines are (line length):"
              , "  Input  (" ++ show (length lfc)  ++ "): " ++ lfc
              , "  Result (" ++ show (length lres) ++ "): " ++ lres
              ]
     err -> assertFailure $ "Failure when parsing " ++ show file ++ "\n" ++ show err

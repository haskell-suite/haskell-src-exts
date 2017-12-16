{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import Control.Monad
import Data.Time.Clock
import Data.Time.Format
import Debug.Trace
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import Test.Common
import Test.CommonUtils
import Test.HUnit
import qualified Data.Set as S

-- ---------------------------------------------------------------------

data Verbosity = Debug | Status | None deriving (Eq, Show, Ord, Enum)

verb :: Verbosity
verb = Debug

-- ---------------------------------------------------------------------

writeCPP :: FilePath -> IO ()
writeCPP fp = appendFileFlush cppFile (('\n' : fp))

writeError = writeCPP

writeParseFail :: FilePath -> String -> IO ()
writeParseFail fp _s = appendFileFlush parseFailFile (('\n' : fp))
-- writeParseFail fp s = appendFileFlush parseFailFile (('\n' : (fp ++ " " ++ s)))

writeProcessed :: FilePath -> IO ()
writeProcessed fp = appendFileFlush processed (('\n' : fp))

writeFailed :: FilePath -> IO ()
writeFailed fp = appendFileFlush processedFailFile (('\n' : fp))

writeLog :: String -> IO ()
writeLog msg = appendFileFlush logFile (('\n' : msg))

getTimeStamp :: IO String
getTimeStamp = do
  t <- getCurrentTime
  return $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H%M%S")) t

writeFailure :: FilePath -> String -> IO ()
writeFailure fp db = do
  ts <- getTimeStamp
  let outname = failuresDir </> takeFileName fp <.> ts <.> "out"
  writeFile outname db

appendFileFlush      :: FilePath -> String -> IO ()
appendFileFlush f txt = withFile f AppendMode (\ hdl -> hPutStr hdl txt >> hFlush hdl)

-- ---------------------------------------------------------------------

readFileIfPresent fileName = do
  isPresent <- doesFileExist fileName
  if isPresent
    then lines <$> readFile fileName
    else return []

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  createDirectoryIfMissing True workDir
  createDirectoryIfMissing True configDir
  createDirectoryIfMissing True failuresDir
  as <- getArgs
  case as of
    [] -> putStrLn "Must enter directory to process"
    ["failures"] -> do
      fs <- lines <$> readFile origFailuresFile
      () <$ runTests (TestList (map mkParserTest fs))
    ["clean"] -> do
      putStrLn "Cleaning..."
      writeFile processed ""
      writeFile parseFailFile ""
      writeFile cppFile ""
      writeFile logFile ""
      writeFile processedFailFile ""
      removeDirectoryRecursive failuresDir
      createDirectory failuresDir
      putStrLn "Done."
    -- ds -> () <$ (runTests =<< (TestList <$> mapM tests ds))
    ds -> do
      !blackList     <- readFileIfPresent blackListed
      !knownFailures <- readFileIfPresent knownFailuresFile
      !processedList <- lines <$> readFile processed
      !cppList       <- lines <$> readFile cppFile
      !parseFailList <- lines <$> readFile parseFailFile
      let done = S.fromList (processedList ++ cppList ++ blackList ++ knownFailures ++ parseFailList)
      tsts <- TestList <$> mapM (tests done) ds
      runTests tsts
      return ()

runTests :: Test -> IO Counts
runTests t = do
  let n = testCaseCount t
  putStrLn $ "Running " ++ show n ++ " tests."
  putStrLn $ "Verbosity: " ++ show verb
  runTestTT t

tests :: S.Set String ->  FilePath -> IO Test
tests done dir = do
  roundTripHackage done dir

-- Selection:

-- Hackage dir
roundTripHackage :: S.Set String -> FilePath -> IO Test
roundTripHackage done hackageDir = do
  packageDirs <- drop 2 <$> getDirectoryContents hackageDir
  when (verb <= Debug) (traceShowM hackageDir)
  when (verb <= Debug) (traceShowM packageDirs)
  TestList <$> mapM (roundTripPackage done) (zip [0..] (map (hackageDir </>) packageDirs))


roundTripPackage :: S.Set String -> (Int, FilePath) -> IO Test
roundTripPackage done (n, dir) = do
  putStrLn (show n)
  when (verb <= Status) (traceM dir)
  hsFiles <- filter (flip S.notMember done)  <$> findSrcFiles dir

  return (TestLabel (dropFileName dir) (TestList $ map mkParserTest hsFiles))

mkParserTest :: FilePath -> Test
mkParserTest fp =
    TestLabel fp $
    TestCase (do writeLog $ "starting:" ++ fp
                 r1 <- catchAny (roundTripTest fp) $ \e -> do
                   writeError fp
                   throwIO e
                 case r1 of
                   Left (ParseFailure _ s) -> do
                     writeParseFail fp s
                     exitFailure
                   Right r -> do
                     writeProcessed fp
                     unless (status r == Success) (writeFailure fp (debugTxt r) >> writeFailed fp)
                     assertBool fp (status r == Success))

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch


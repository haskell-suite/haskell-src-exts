{-# LANGUAGE ViewPatterns #-}
module Main where

-- Static site generator for failing tests
import Data.Algorithm.Diff (getGroupedDiff)
import Data.Algorithm.DiffOutput (ppDiff)

import System.Directory
import System.FilePath

import Test.CommonUtils
import Control.Monad

import Debug.Trace

import Data.List
import System.Environment
import Data.Maybe
import Text.Read

main :: IO ()
main = do
  createDirectoryIfMissing True failuresHtmlDir
  n <- getArgs
  case readMaybe =<< listToMaybe n of
    Nothing -> site 100
    Just k  -> site k

site :: Int -> IO ()
site n = do
  putStrLn $ "Generating site for first: " ++ show n
  failPaths <- filterM doesFileExist =<< (map (failuresDir </>)  . take n <$> getDirectoryContents failuresDir)
  traceShowM failPaths
  fails <- mapM parseFail failPaths
  writeFile origFailuresFile (intercalate "\n" (map getfname fails))
  -- writeFile "failures/failures.html" (makeIndex failPaths)
  writeFile (failuresHtmlDir </> failuresHtmlFile) (makeIndex failPaths)
  let padded = failuresHtmlFile : (map makeFailLink failPaths ++ [failuresHtmlFile])
  let resolved = zipWith (\x (y,z) -> (x, y, z)) padded (zip (tail padded) (tail (tail padded)))
  mapM_ (uncurry page) (zip resolved fails)

makeFailLink :: FilePath -> String
makeFailLink fp = takeFileName fp  <.> "html"

makeIndex :: [FilePath] -> String
makeIndex files =
  intercalate "</br>" (map mkIndexLink files)
  where
    mkIndexLink f = mkLink (takeFileName f <.> "html") f



page :: (FilePath, FilePath, FilePath) -> Failure -> IO ()
page (prev, out, next) (Failure res fname) = do
--  traceM out
  original <- readFile fname
  -- let diff = getDiff (tokenize original) (tokenize res)
  let lres = lines res
  let maxLines = 50000
  let diff = getGroupedDiff (lines original) (take maxLines lres)
  let l = length lres
  if (l > maxLines)
    then  do -- putStrLn ("Skipping: " ++ fname) >> print l
      let resTrunc = (intercalate "\n" $ take maxLines lres)
                  ++ "\n*****************TRUNCATED*******"
      writeFile (failuresHtmlDir </> out) (mkPage fname (ppDiff diff) prev next original resTrunc)
    else
      -- writeFile ("failures" </> out) (mkPage (ppDiff diff) prev next original res)
      writeFile (failuresHtmlDir </> out) (mkPage fname (ppDiff diff) prev next original res)
  where
    tokenize :: String -> [[String]]
    tokenize s = map (:[]) . lines $ s

mkPage :: FilePath -> String -> String -> String -> String -> String -> String
mkPage filename diff prev next original printed  =
  intercalate "</br>"
  [mkLink prev "prev"
  , mkLink failuresHtmlFile "home"
  , mkLink next "next"
  , ""
  , "<pre>" ++ filename ++ "</pre>"
  , ""
  , "<pre>" ++ diff ++ "</pre>"
  , "<h2>original</h2>"
  , "<pre>" ++ original ++ "</pre>"
  , "<h2>printed</h2>"
  , "<pre>" ++ printed ++ "</pre>"
  ]

mkLink :: String -> String -> String
mkLink s label =
  "<a href=\"" ++ s ++ "\">" ++ label ++ "</a>"

data Failure = Failure String FilePath

getfname :: Failure -> FilePath
getfname (Failure _ fp) = fp

parseFail :: FilePath -> IO Failure
parseFail fp = do
  res <- lines <$> readFile fp
  let (finalres, head . tail -> fname) = break (=="==============") res
  return (Failure (unlines finalres) fname)


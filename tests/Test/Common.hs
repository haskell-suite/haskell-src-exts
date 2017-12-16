{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Common (
                RoundtripReport (..)
              , Report
              , ParseFailure(..)
              , ReportType(..)
              , roundTripTest
              , mkParsingTest
              , getModSummaryForFile

              , testList
              , testPrefix
              , Changer
              , genTest
              , noChange
              , mkDebugOutput
              ) where



import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Parsers (parseModuleApiAnnsWithCpp)
import Language.Haskell.GHC.ExactPrint.Preprocess
import Language.Haskell.Exts


import qualified ApiAnnotation as GHC
import qualified DynFlags      as GHC
-- import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)
-- import qualified Lexer         as GHC
import qualified MonadUtils    as GHC
-- import qualified Parser        as GHC
-- import qualified SrcLoc        as GHC
-- import qualified StringBuffer  as GHC

#if __GLASGOW_HASKELL__ <= 710
#else
import qualified GHC.LanguageExtensions as LangExt
#endif

-- import qualified Data.Map as Map

import Control.Monad
import Data.List hiding (find)

import System.Directory

import Test.HUnit
import System.FilePath

-- import Debug.Trace
testPrefix :: FilePath
testPrefix = "tests" </> "examples"

testList :: String -> [Test] -> Test
testList s ts = TestLabel s (TestList ts)

-- ---------------------------------------------------------------------
-- Roundtrip machinery

type Report = Either ParseFailure RoundtripReport

data RoundtripReport =
  Report
   { debugTxt     :: String
   , status       :: ReportType
   , cppStatus    :: Maybe String -- Result of CPP if invoked
   }

data ParseFailure = ParseFailure SrcLoc String

data ReportType =
   Success
 | RoundTripFailure deriving (Eq, Show)

{-
runParser :: GHC.P a -> GHC.DynFlags -> FilePath -> String -> GHC.ParseResult a
runParser parser flags filename str = GHC.unP parser parseState
    where
      location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
      buffer = GHC.stringToStringBuffer str
      parseState = GHC.mkPState flags buffer location

parseFile :: GHC.DynFlags -> FilePath -> String -> GHC.ParseResult (GHC.Located (GHC.HsModule GHC.RdrName))
parseFile = runParser GHC.parseModule

mkApiAnns :: GHC.PState -> GHC.ApiAnns
mkApiAnns pstate = (Map.fromListWith (++) . GHC.annotations $ pstate
                   , Map.fromList ((GHC.noSrcSpan, GHC.comment_q pstate) : (GHC.annotations_comments pstate)))

removeSpaces :: String -> String
removeSpaces = map (\case {'\160' -> ' '; s -> s})
-}

roundTripTest :: FilePath -> IO Report
roundTripTest f = genTest noChange f f


mkParsingTest :: (FilePath -> IO Report) -> FilePath -> FilePath -> Test
mkParsingTest tester dir fp =
  let basename       = testPrefix </> dir </> fp
      writeFailure   = writeFile (basename <.> "out")
      writeHsPP      = writeFile (basename <.> "hspp")
  in
    TestCase (do r <- either (\(ParseFailure _ s) -> error (s ++ basename)) id
                        <$> tester basename
                 writeFailure (debugTxt r)
                 forM_ (cppStatus r) writeHsPP
                 assertBool fp (status r == Success))


type Changer = (Anns -> GHC.ParsedSource -> IO (Anns,GHC.ParsedSource))

noChange :: Changer
noChange ans parsed = return (ans,parsed)

defaultExtensions :: [Extension]
defaultExtensions = [e | e@EnableExtension{} <- knownExtensions] \\ map EnableExtension badExtensions

badExtensions =
    [Arrows -- steals proc
    ,TransformListComp -- steals the group keyword
    ,XmlSyntax, RegularPatterns -- steals a-b
    ,UnboxedTuples -- breaks (#) lens operator
    ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
    ,DoRec, RecursiveDo -- breaks rec
    ,TypeApplications -- HSE fails on @ patterns
    ]

genTest :: Changer -> FilePath -> FilePath -> IO Report
genTest f origFile expectedFile  = do
      res <- parseFileWithExts defaultExtensions origFile
      expected <- GHC.liftIO $ readFileGhc expectedFile
      orig <- GHC.liftIO $ readFileGhc origFile
      -- let pristine = removeSpaces expected
      let pristine = expected
      case res of
        ParseFailed ss s -> do
          let debugTxt = mkNewDebugText expected s ss
              useCpp = Nothing
              status = RoundTripFailure
          return (Right (Report{..}))
        ParseOk res -> do
          let debugTxt = ""
              useCpp = Nothing
              status = Success
          return (Right Report{..})

mkNewDebugText :: String -> String -> SrcLoc -> String
mkNewDebugText file err ss =
  unlines [ "-- " ++ show ss
        , "-- " ++ err
        , "--==========================================="
        , file ]

{-
      case res of
        Left (ss, m) -> return . Left $ ParseFailure ss m
        Right (apianns, injectedComments, dflags, pmod)  -> do
          (printed', anns, pmod') <- GHC.liftIO (runRoundTrip f apianns pmod injectedComments)
#if __GLASGOW_HASKELL__ <= 710
          let useCpp = GHC.xopt GHC.Opt_Cpp dflags
#else
          let useCpp = GHC.xopt LangExt.Cpp dflags
#endif
              printed = trimPrinted printed'
          -- let (printed, anns) = first trimPrinted $ runRoundTrip apianns pmod injectedComments
              -- Clang cpp adds an extra newline character
              -- Do not remove this line!
              trimPrinted p = if useCpp
                                then unlines $ take (length (lines pristine)) (lines p)
                                else p
              debugTxt = mkDebugOutput origFile printed pristine apianns anns pmod'
              status = if printed == pristine then Success else RoundTripFailure
              cppStatus = if useCpp then Just orig else Nothing
          return $ Right Report {..}
          -}


mkDebugOutput :: FilePath -> String -> String
              -> GHC.ApiAnns
              -> Anns
              -> GHC.ParsedSource -> String
mkDebugOutput filename printed original apianns anns parsed =
  intercalate sep [ printed
                 , filename
                 , "lengths:" ++ show (length printed,length original) ++ "\n"
                 , showAnnData anns 0 parsed
                 , showGhc anns
                 , showGhc apianns
                ]
  where
    sep = "\n==============\n"


{-
runRoundTrip :: Changer
             -> GHC.ApiAnns -> GHC.Located (GHC.HsModule GHC.RdrName)
             -> [Comment]
             -> IO (String, Anns, GHC.ParsedSource)
runRoundTrip f !anns !parsedOrig cs = do
  let !relAnns = relativiseApiAnnsWithComments cs parsedOrig anns
  (annsMod, pmod) <- f relAnns parsedOrig
  let !printed = exactPrint pmod annsMod
  -- return (printed,  relAnns, pmod)
  return (printed,  annsMod, pmod)
  -}

-- ---------------------------------------------------------------------`

canonicalizeGraph ::
  [GHC.ModSummary] -> IO [(Maybe (FilePath), GHC.ModSummary)]
canonicalizeGraph graph = do
  let mm = map (\m -> (GHC.ml_hs_file $ GHC.ms_location m, m)) graph
      canon ((Just fp),m) = do
        fp' <- canonicalizePath fp
        return $ (Just fp',m)
      canon (Nothing,m)  = return (Nothing,m)

  mm' <- mapM canon mm

  return mm'

-- ---------------------------------------------------------------------

getModSummaryForFile :: (GHC.GhcMonad m) => FilePath -> m (Maybe GHC.ModSummary)
getModSummaryForFile fileName = do
  cfileName <- GHC.liftIO $ canonicalizePath fileName

  graph <- GHC.getModuleGraph
  cgraph <- GHC.liftIO $ canonicalizeGraph graph

  let mm = filter (\(mfn,_ms) -> mfn == Just cfileName) cgraph
  case mm of
   [] -> return Nothing
   fs -> return (Just (snd $ head fs))


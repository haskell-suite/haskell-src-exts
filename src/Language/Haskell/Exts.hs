-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts
-- Copyright   :  (c) Niklas Broberg 2004-2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- An umbrella module for the various functionality
-- of the package. Also provides some convenient
-- functionality for dealing directly with source files.
--
-----------------------------------------------------------------------------
module Language.Haskell.Exts (
    -- * Re-exported modules
      module Language.Haskell.Exts.Syntax
    , module Language.Haskell.Exts.Build
    , module Language.Haskell.Exts.Lexer
    , module Language.Haskell.Exts.Pretty
    , module Language.Haskell.Exts.Fixity
    , module Language.Haskell.Exts.ExactPrint
    , module Language.Haskell.Exts.SrcLoc
    , module Language.Haskell.Exts.Comments
    , module Language.Haskell.Exts.Extension
    , module Language.Haskell.Exts.Parser
    -- * Parsing of Haskell source files
    , parseFile
    , parseFileWithMode
    , parseFileWithExts
    , parseFileWithComments
    , parseFileWithCommentsAndPragmas
    , parseFileContents
    , parseFileContentsWithMode
    , parseFileContentsWithExts
    , parseFileContentsWithComments
    -- * Read extensions declared in LANGUAGE pragmas
    , readExtensions
    ) where

import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Lexer ( lexTokenStream, lexTokenStreamWithMode, Token(..) )
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Fixity
import Language.Haskell.Exts.ExactPrint
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Extension

import Data.List
import Data.Maybe (fromMaybe)
import Language.Preprocessor.Unlit
import System.IO

-- | Parse a source file on disk, using the default parse mode.
parseFile :: FilePath -> IO (ParseResult (Module SrcSpanInfo))
parseFile fp = parseFileWithMode (defaultParseMode { parseFilename = fp }) fp

-- | Parse a source file on disk, with an extra set of extensions to know about
--   on top of what the file itself declares.
parseFileWithExts :: [Extension] -> FilePath -> IO (ParseResult (Module SrcSpanInfo))
parseFileWithExts exts fp =
    parseFileWithMode (defaultParseMode {
                         extensions = exts,
                         parseFilename = fp }) fp

-- | Parse a source file on disk, supplying a custom parse mode.
parseFileWithMode :: ParseMode -> FilePath -> IO (ParseResult (Module SrcSpanInfo))
parseFileWithMode p fp = readUTF8File fp >>= return . parseFileContentsWithMode p

parseFileWithComments :: ParseMode -> FilePath -> IO (ParseResult (Module SrcSpanInfo, [Comment]))
parseFileWithComments p fp = readUTF8File fp >>= return . parseFileContentsWithComments p

-- | Parse a source file on disk, supplying a custom parse mode, and retaining comments
--  as well as unknown pragmas.
parseFileWithCommentsAndPragmas
  :: ParseMode -> FilePath
  -> IO (ParseResult (Module SrcSpanInfo, [Comment], [UnknownPragma]))
parseFileWithCommentsAndPragmas p fp =
    readUTF8File fp >>= return . parseFileContentsWithCommentsAndPragmas p

-- | Parse a source file from a string using a custom parse mode retaining comments
--   as well as unknown pragmas.
parseFileContentsWithCommentsAndPragmas
  :: ParseMode -> String
  -> ParseResult (Module SrcSpanInfo, [Comment], [UnknownPragma])
parseFileContentsWithCommentsAndPragmas pmode str = separatePragmas parseResult
    where parseResult = parseFileContentsWithComments pmode str

-- | Parse a source file from a string using the default parse mode.
parseFileContents :: String -> ParseResult (Module SrcSpanInfo)
parseFileContents = parseFileContentsWithMode defaultParseMode

-- | Parse a source file from a string, with an extra set of extensions to know about
--   on top of what the file itself declares.
parseFileContentsWithExts :: [Extension] -> String -> ParseResult (Module SrcSpanInfo)
parseFileContentsWithExts exts =
    parseFileContentsWithMode (defaultParseMode { extensions = exts })

-- | Parse a source file from a string using a custom parse mode.
parseFileContentsWithMode :: ParseMode -> String -> ParseResult (Module SrcSpanInfo)
parseFileContentsWithMode p@(ParseMode fn oldLang exts ign _ _ _) rawStr =
        let md = delit fn $ ppContents rawStr
            (bLang, extraExts) =
                case (ign, readExtensions md) of
                  (False, Just (mLang, es)) ->
                       (fromMaybe oldLang mLang, es)
                  _ -> (oldLang, [])
         in -- trace (fn ++ ": " ++ show extraExts) $
              parseModuleWithMode (p { baseLanguage = bLang, extensions = exts ++ extraExts }) md

parseFileContentsWithComments :: ParseMode -> String -> ParseResult (Module SrcSpanInfo, [Comment])
parseFileContentsWithComments p@(ParseMode fn oldLang exts ign _ _ _) rawStr =
        let md = delit fn $ ppContents rawStr
            (bLang, extraExts) =
                case (ign, readExtensions md) of
                  (False, Just (mLang, es)) ->
                       (fromMaybe oldLang mLang, es)
                  _ -> (oldLang, [])
         in parseModuleWithComments (p { baseLanguage = bLang, extensions = exts ++ extraExts }) md

-- | Gather the extensions declared in LANGUAGE pragmas
--   at the top of the file. Returns 'Nothing' if the
--   parse of the pragmas fails.
readExtensions :: String -> Maybe (Maybe Language, [Extension])
readExtensions str = case getTopPragmas str of
        ParseOk pgms -> extractLang $ concatMap getExts pgms
        _            -> Nothing
  where getExts :: ModulePragma l -> [Either Language Extension]
        getExts (LanguagePragma _ ns) = map readExt ns
        getExts _ = []

        readExt (Ident _ e) =
            case classifyLanguage e of
              UnknownLanguage _ -> Right $ classifyExtension e
              lang -> Left lang
        readExt Symbol {} = error "readExt: Symbol"

        extractLang = extractLang' Nothing []

        extractLang' lacc eacc [] = Just (lacc, eacc)
        extractLang' Nothing eacc (Left l : rest) = extractLang' (Just l) eacc rest
        extractLang' (Just l1) eacc (Left l2:rest)
            | l1 == l2  = extractLang' (Just l1) eacc rest
            | otherwise = Nothing
        extractLang' lacc eacc (Right ext : rest) = extractLang' lacc (ext:eacc) rest

ppContents :: String -> String
ppContents = unlines . f . lines
  where f (('#':_):rest) = rest
        f x = x

delit :: String -> String -> String
delit fn = if ".lhs" `isSuffixOf` fn then unlit fn else id

readUTF8File :: FilePath -> IO String
readUTF8File fp = do
  h <- openFile fp ReadMode
  hSetEncoding h utf8
  c <- hGetContents h
  close h

-- | Converts a parse result with comments to a parse result with comments and
--   unknown pragmas.
separatePragmas :: ParseResult (Module SrcSpanInfo, [Comment])
                -> ParseResult (Module SrcSpanInfo, [Comment], [UnknownPragma])
separatePragmas r =
    case r of
        ParseOk (m, comments) ->
            let (pragmas, comments') = partition pragLike comments
              in  ParseOk (m, comments', map commentToPragma pragmas)
                where commentToPragma (Comment _ l s) =
                            UnknownPragma l $ init $ drop 1 s
                      pragLike (Comment b _ s) = b && pcond s
                      pcond s = length s > 1 && take 1 s == "#" && last s == '#'
        ParseFailed l s ->  ParseFailed l s

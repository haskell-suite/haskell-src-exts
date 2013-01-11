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
    , module Language.Haskell.Exts.Parser
    , module Language.Haskell.Exts.Pretty
    , module Language.Haskell.Exts.Extension
    , module Language.Haskell.Exts.Fixity
    , module Language.Haskell.Exts.Comments
    -- * Parsing of Haskell source files
    , parseFile
    , parseFileWithMode
    , parseFileWithExts
    , parseFileWithComments
    , parseFileContents
    , parseFileContentsWithMode
    , parseFileContentsWithExts
    , parseFileContentsWithComments
    -- * Read extensions declared in LANGUAGE pragmas
    , readExtensions
    ) where

import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Fixity
import Language.Haskell.Exts.Comments

import Data.List
import Language.Preprocessor.Unlit

-- | Parse a source file on disk, using the default parse mode.
parseFile :: FilePath -> IO (ParseResult Module)
parseFile fp = parseFileWithMode (defaultParseMode { parseFilename = fp }) fp

-- | Parse a source file on disk, with an extra set of extensions to know about
--   on top of what the file itself declares.
parseFileWithExts :: [Extension] -> FilePath -> IO (ParseResult Module)
parseFileWithExts exts fp = parseFileWithMode (defaultParseMode { extensions = exts, parseFilename = fp }) fp

-- | Parse a source file on disk, supplying a custom parse mode.
parseFileWithMode :: ParseMode -> FilePath -> IO (ParseResult Module)
parseFileWithMode p fp = readFile fp >>= (return . parseFileContentsWithMode p)

-- | Parse a source file on disk, supplying a custom parse mode, and retaining comments.
parseFileWithComments :: ParseMode -> FilePath -> IO (ParseResult (Module, [Comment]))
parseFileWithComments p fp = readFile fp >>= (return . parseFileContentsWithComments p)

-- | Parse a source file from a string using the default parse mode.
parseFileContents :: String -> ParseResult Module
parseFileContents = parseFileContentsWithMode defaultParseMode

-- | Parse a source file from a string, with an extra set of extensions to know about
--   on top of what the file itself declares.
parseFileContentsWithExts :: [Extension] -> String -> ParseResult Module
parseFileContentsWithExts exts = parseFileContentsWithMode (defaultParseMode { extensions = exts })

-- | Parse a source file from a string using a custom parse mode.
parseFileContentsWithMode :: ParseMode -> String -> ParseResult Module
parseFileContentsWithMode p@(ParseMode fn oldLang exts ign _ _) rawStr =
        let md = delit fn $ ppContents rawStr
            (bLang, extraExts) = 
                case (ign, readExtensions md) of
                  (False, Just (mLang, es)) -> 
                       (case mLang of {Nothing -> oldLang;Just newLang -> newLang}, es)
                  _ -> (oldLang, [])
         in parseWithMode (p { baseLanguage = bLang, extensions = exts ++ extraExts }) md

-- | Parse a source file from a string using a custom parse mode and retaining comments.
parseFileContentsWithComments :: ParseMode -> String -> ParseResult (Module, [Comment])
parseFileContentsWithComments p@(ParseMode fn oldLang exts ign _ _) rawStr =
        let md = delit fn $ ppContents rawStr
            (bLang, extraExts) = 
                case (ign, readExtensions md) of
                  (False, Just (mLang, es)) -> 
                       (case mLang of {Nothing -> oldLang;Just newLang -> newLang}, es)
                  _ -> (oldLang, [])
         in parseWithComments (p { baseLanguage = bLang, extensions = exts ++ extraExts }) md


{-- | Gather the extensions declared in LANGUAGE pragmas
--   at the top of the file. Returns 'Nothing' if the
--   parse of the pragmas fails.
readExtensions :: String -> Maybe [Extension]
readExtensions str = case getTopPragmas str of
        ParseOk pgms -> Just (concatMap getExts pgms)
        _            -> Nothing
  where getExts :: ModulePragma -> [Extension]
        getExts (LanguagePragma _ ns) = map readExt ns
        getExts _ = []

        readExt (Ident e) = classifyExtension e -}

-- | Gather the extensions declared in LANGUAGE pragmas
--   at the top of the file. Returns 'Nothing' if the
--   parse of the pragmas fails.
readExtensions :: String -> Maybe (Maybe Language, [Extension])
readExtensions str = case getTopPragmas str of
        ParseOk pgms -> extractLang $ concatMap getExts pgms
        _            -> Nothing
  where getExts :: ModulePragma -> [Either Language Extension]
        getExts (LanguagePragma _ ns) = map readExt ns
        getExts _ = []

        readExt (Ident e) = 
            case classifyLanguage e of
              UnknownLanguage _ -> Right $ classifyExtension e
              lang -> Left lang

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
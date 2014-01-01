{-# LANGUAGE FlexibleInstances #-}
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
module Language.Haskell.Exts.Annotated (
    -- * Re-exported modules
      module Language.Haskell.Exts.Annotated.Build
    , module Language.Haskell.Exts.Annotated.ExactPrint
    , module Language.Haskell.Exts.Annotated.Fixity
    , module Language.Haskell.Exts.Annotated.Parser
    , module Language.Haskell.Exts.Annotated.Syntax
    , module Language.Haskell.Exts.Comments
    , module Language.Haskell.Exts.Extension
    , module Language.Haskell.Exts.Lexer
    , module Language.Haskell.Exts.Pretty
    , module Language.Haskell.Exts.SrcLoc
    -- * Parsing of Haskell source files
    , parseFile
    , parseFileWithMode
    , parseFileWithExts
    , parseFileWithComments
    , parseFileContents
    , parseFileContentsWithMode
    , parseFileContentsWithExts
    , parseFileContentsWithComments
    -- * Parse results
    , ParseResult(..), fromParseResult
    -- * Parse modes
    , ParseMode(..), defaultParseMode
    ) where

import Language.Haskell.Exts.Annotated.Build
import Language.Haskell.Exts.Annotated.ExactPrint
import Language.Haskell.Exts.Annotated.Fixity
import Language.Haskell.Exts.Annotated.Parser
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.ParseMonad ( ParseResult(..), fromParseResult, ParseMode(..), defaultParseMode )
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc

import Data.List
import Language.Preprocessor.Unlit

-- | Parse a source file on disk, using the default 'ParseMode'.
parseFile :: FilePath -> IO (ParseResult (Module SrcSpanInfo))
parseFile fp = parseFileWithMode (defaultParseMode { parseFilename = fp }) fp

-- | Parse a source file on disk, with an extra set of extensions to know about
--   on top of what the file itself declares.
parseFileWithExts :: [Extension] -> FilePath -> IO (ParseResult (Module SrcSpanInfo))
parseFileWithExts exts fp =
    parseFileWithMode (defaultParseMode {
                         extensions = exts,
                         parseFilename = fp }) fp

-- | Parse a source file on disk, supplying a custom 'ParseMode'.
parseFileWithMode :: ParseMode -> FilePath -> IO (ParseResult (Module SrcSpanInfo))
parseFileWithMode p fp = readFile fp >>= (return . parseFileContentsWithMode p)

parseFileWithComments :: ParseMode -> FilePath -> IO (ParseResult (Module SrcSpanInfo, [Comment]))
parseFileWithComments p fp = readFile fp >>= (return . parseFileContentsWithComments p)

-- | Parse a source file from a string using the default 'ParseMode'.
parseFileContents :: String -> ParseResult (Module SrcSpanInfo)
parseFileContents = parseFileContentsWithMode defaultParseMode

-- | Parse a source file from a string, with an extra set of extensions to know about
--   on top of what the file itself declares.
parseFileContentsWithExts :: [Extension] -> String -> ParseResult (Module SrcSpanInfo)
parseFileContentsWithExts exts =
    parseFileContentsWithMode (defaultParseMode { extensions = exts })

-- | Parse a source file from a string using a custom 'ParseMode'.
parseFileContentsWithMode :: ParseMode -> String -> ParseResult (Module SrcSpanInfo)
parseFileContentsWithMode p@(ParseMode fn oldLang exts ign _ _) rawStr =
        let md = delit fn $ ppContents rawStr
            (bLang, extraExts) =
                case (ign, parseExtensions md) of
                  (False, ParseOk ([newLang], es)) -> (newLang, es)
                  (False, ParseOk (_, es)) -> (oldLang, es)
                  _ -> (oldLang, [])
         in -- trace (fn ++ ": " ++ show extraExts) $
              parseModuleWithMode (p { baseLanguage = bLang, extensions = exts ++ extraExts }) md

parseFileContentsWithComments :: ParseMode -> String -> ParseResult (Module SrcSpanInfo, [Comment])
parseFileContentsWithComments p@(ParseMode fn oldLang exts ign _ _) rawStr =
        let md = delit fn $ ppContents rawStr
            (bLang, extraExts) =
                case (ign, parseExtensions md) of
                  (False, ParseOk ([newLang], es)) -> (newLang, es)
                  (False, ParseOk (_, es)) -> (oldLang, es)
                  _ -> (oldLang, [])
         in parseModuleWithComments (p { baseLanguage = bLang, extensions = exts ++ extraExts }) md

ppContents :: String -> String
ppContents = unlines . f . lines
  where f (('#':_):rest) = rest
        f x = x

delit :: String -> String -> String
delit fn = if ".lhs" `isSuffixOf` fn then unlit fn else id

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
    ) where

import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Fixity
import Language.Haskell.Exts.Comments

import Control.Arrow (first)
import qualified Language.Haskell.Exts.Annotated as A
import Language.Haskell.Exts.Annotated.Simplify ( sModule )

-- | Parse a source file on disk, using the default parse mode.
parseFile :: FilePath -> IO (ParseResult Module)
parseFile = fmap (fmap sModule) . A.parseFile

-- | Parse a source file on disk, with an extra set of extensions to know about
--   on top of what the file itself declares.
parseFileWithExts :: [Extension] -> FilePath -> IO (ParseResult Module)
parseFileWithExts exts = fmap (fmap sModule) . A.parseFileWithExts exts

-- | Parse a source file on disk, supplying a custom parse mode.
parseFileWithMode :: ParseMode -> FilePath -> IO (ParseResult Module)
parseFileWithMode mode = fmap (fmap sModule) . A.parseFileWithMode mode

-- | Parse a source file on disk, supplying a custom parse mode, and retaining comments.
parseFileWithComments :: ParseMode -> FilePath -> IO (ParseResult (Module, [Comment]))
parseFileWithComments mode = fmap (fmap (first sModule)) . A.parseFileWithComments mode

-- | Parse a source file from a string using the default parse mode.
parseFileContents :: String -> ParseResult Module
parseFileContents = fmap sModule . A.parseFileContents

-- | Parse a source file from a string, with an extra set of extensions to know about
--   on top of what the file itself declares.
parseFileContentsWithExts :: [Extension] -> String -> ParseResult Module
parseFileContentsWithExts exts = fmap sModule . A.parseFileContentsWithExts exts

-- | Parse a source file from a string using a custom parse mode.
parseFileContentsWithMode :: ParseMode -> String -> ParseResult Module
parseFileContentsWithMode mode = fmap sModule . A.parseFileContentsWithMode mode

-- | Parse a source file from a string using a custom parse mode and retaining comments.
parseFileContentsWithComments :: ParseMode -> String -> ParseResult (Module, [Comment])
parseFileContentsWithComments mode = fmap (first sModule) . A.parseFileContentsWithComments mode

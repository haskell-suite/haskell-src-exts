module Language.Haskell.Exts (
      module Language.Haskell.Exts.Syntax
    , module Language.Haskell.Exts.Build
    , module Language.Haskell.Exts.Parser
    , module Language.Haskell.Exts.Pretty
    , module Language.Haskell.Exts.Extension
    , parseFileContents
    , parseFileContentsWithMode
    , parseFile
    , parseFileWithExts
    ) where

import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Extension

import Data.List
import Language.Preprocessor.Unlit

parseFile :: FilePath -> IO (ParseResult Module)
parseFile = parseFileWithExts []

parseFileWithExts :: [Extension] -> FilePath -> IO (ParseResult Module)
parseFileWithExts exts fp = readFile fp >>= (return . parseFileContentsWithMode (ParseMode fp exts))

parseFileContents :: String -> ParseResult Module
parseFileContents = parseFileContentsWithMode defaultParseMode

parseFileContentsWithMode :: ParseMode -> String -> ParseResult Module
parseFileContentsWithMode p rawStr = parseModuleWithMode p (delit $ unlines $ map f $ lines rawStr)
    where
        f ('#':_) = ""
        f x = x

        filename = parseFilename p
        delit = if ".lhs" `isSuffixOf` filename then unlit filename else id

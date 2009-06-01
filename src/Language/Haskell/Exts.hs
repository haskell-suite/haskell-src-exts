module Language.Haskell.Exts (
      module Language.Haskell.Exts.Syntax
    , module Language.Haskell.Exts.Build
    , module Language.Haskell.Exts.Parser
    , module Language.Haskell.Exts.Pretty
    , parseFileContents
    , parseFileContentsWithMode
    , parseFile
    ) where

import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Data.List
import Language.Preprocessor.Unlit

parseFile :: FilePath -> IO (ParseResult Module)
parseFile fp = readFile fp >>= (return . parseFileContentsWithMode (ParseMode fp []))

parseFileContents :: String -> ParseResult Module
parseFileContents = parseFileContentsWithMode defaultParseMode

parseFileContentsWithMode :: ParseMode -> String -> ParseResult Module
parseFileContentsWithMode p rawStr = parseModuleWithMode p (delit $ unlines $ map f $ lines rawStr)
    where
        f ('#':_) = ""
        f x = x

        filename = parseFilename p
        delit = if ".lhs" `isSuffixOf` filename then unlit filename else id

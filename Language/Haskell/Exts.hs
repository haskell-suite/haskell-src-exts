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

parseFile :: FilePath -> IO (ParseResult HsModule)
parseFile fp = readFile fp >>= (return . parseFileContentsWithMode (ParseMode fp))

parseFileContents :: String -> ParseResult HsModule
parseFileContents = parseFileContentsWithMode defaultParseMode

parseFileContentsWithMode :: ParseMode -> String -> ParseResult HsModule
parseFileContentsWithMode p rawStr = parseModuleWithMode p (unlines $ map f $ lines rawStr)
    where
        f ('#':_) = ""
        f x = x

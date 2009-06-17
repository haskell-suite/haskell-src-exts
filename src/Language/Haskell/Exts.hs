module Language.Haskell.Exts (
      module Language.Haskell.Exts.Syntax
    , module Language.Haskell.Exts.Build
    , module Language.Haskell.Exts.Parser
    , module Language.Haskell.Exts.Pretty
    , module Language.Haskell.Exts.Extension
    , module Language.Haskell.Exts.Fixity
    , parseFile
    , parseFileWithMode
    , parseFileWithExts
    , parseFileContents
    , parseFileContentsWithMode
    , readExtensions
    ) where

import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Fixity

import Data.List
import Language.Preprocessor.Unlit

parseFile :: FilePath -> IO (ParseResult Module)
parseFile fp = parseFileWithMode (defaultParseMode { parseFilename = fp }) fp

parseFileWithExts :: [Extension] -> FilePath -> IO (ParseResult Module)
parseFileWithExts exts fp = parseFileWithMode (defaultParseMode { extensions = exts, parseFilename = fp }) fp

parseFileWithMode :: ParseMode -> FilePath -> IO (ParseResult Module)
parseFileWithMode p fp = readFile fp >>= (return . parseFileContentsWithMode p)

parseFileContents :: String -> ParseResult Module
parseFileContents = parseFileContentsWithMode defaultParseMode

parseFileContentsWithMode :: ParseMode -> String -> ParseResult Module
parseFileContentsWithMode p@(ParseMode fn exts ign _) rawStr =
        let md = delit fn $ ppContents rawStr
            plusExts = case (ign, readExtensions md) of
                        (False,Just exts) -> exts
                        _                 -> []
         in parseModuleWithMode (p { extensions = exts ++ plusExts }) md


readExtensions :: String -> Maybe [Extension]
readExtensions str = case getTopPragmas str of
        ParseOk pgms -> Just (concatMap getExts pgms)
        _            -> Nothing
  where getExts :: OptionPragma -> [Extension]
        getExts (LanguagePragma _ ns) = map readExt ns
        getExts _ = []

        readExt (Ident e) = read e

ppContents :: String -> String
ppContents = unlines . map f . lines
  where f ('#':_) = ""
        f x = x

delit :: String -> String -> String
delit fn = if ".lhs" `isSuffixOf` fn then unlit fn else id
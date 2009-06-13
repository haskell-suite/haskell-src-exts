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
    , readExtensions
    ) where

import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Extension

import Data.List
import Language.Preprocessor.Unlit

parseFile :: FilePath -> IO (ParseResult Module)
parseFile fp = do
        fc <- readFile fp
        let md = delit fp $ ppContents fc
            exts = case readExtensions md of
                    Just exts -> exts
                    Nothing   -> []
        return $ parseModuleWithMode (ParseMode fp exts) md

parseFileWithExts :: [Extension] -> FilePath -> IO (ParseResult Module)
parseFileWithExts exts fp = readFile fp >>= (return . parseFileContentsWithMode (ParseMode fp exts))

parseFileContents :: String -> ParseResult Module
parseFileContents = parseFileContentsWithMode defaultParseMode

parseFileContentsWithMode :: ParseMode -> String -> ParseResult Module
parseFileContentsWithMode p rawStr = parseModuleWithMode p (delit filename $ ppContents rawStr)
  where filename = parseFilename p

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
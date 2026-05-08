-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Parser.Text
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Stability   :  stable
-- Portability :  portable
--
-- 'Text'-input variants of the parser entry points in
-- "Language.Haskell.Exts.Parser".  These skip the eager 'T.pack' that
-- the 'String' entry points do at the boundary; the returned AST is
-- still the existing 'String'-valued 'Syntax' AST.
--
-----------------------------------------------------------------------------
module Language.Haskell.Exts.Parser.Text
    ( parseTextWithMode
    , parseTextWithComments
    , parseModuleText
    , parseModuleTextWithMode
    , parseModuleTextWithComments
    , parseExpText
    , parseExpTextWithMode
    , parseExpTextWithComments
    , parseDeclText
    , parseDeclTextWithMode
    , parseDeclTextWithComments
    , parseTypeText
    , parseTypeTextWithMode
    , parseTypeTextWithComments
    , parsePatText
    , parsePatTextWithMode
    , parsePatTextWithComments
    , parseStmtText
    , parseStmtTextWithMode
    , parseStmtTextWithComments
    , parseImportDeclText
    , parseImportDeclTextWithMode
    , parseImportDeclTextWithComments
    ) where

import Data.Text (Text)
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.ParseMonad
import Language.Haskell.Exts.Parser ()
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax

parseModuleText :: Text -> ParseResult (Module SrcSpanInfo)
parseModuleText = parseTextWithMode defaultParseMode

parseModuleTextWithMode :: ParseMode -> Text -> ParseResult (Module SrcSpanInfo)
parseModuleTextWithMode = parseTextWithMode

parseModuleTextWithComments :: ParseMode -> Text -> ParseResult (Module SrcSpanInfo, [Comment])
parseModuleTextWithComments = parseTextWithComments

parseExpText :: Text -> ParseResult (Exp SrcSpanInfo)
parseExpText = parseTextWithMode defaultParseMode

parseExpTextWithMode :: ParseMode -> Text -> ParseResult (Exp SrcSpanInfo)
parseExpTextWithMode = parseTextWithMode

parseExpTextWithComments :: ParseMode -> Text -> ParseResult (Exp SrcSpanInfo, [Comment])
parseExpTextWithComments = parseTextWithComments

parseDeclText :: Text -> ParseResult (Decl SrcSpanInfo)
parseDeclText = parseTextWithMode defaultParseMode

parseDeclTextWithMode :: ParseMode -> Text -> ParseResult (Decl SrcSpanInfo)
parseDeclTextWithMode = parseTextWithMode

parseDeclTextWithComments :: ParseMode -> Text -> ParseResult (Decl SrcSpanInfo, [Comment])
parseDeclTextWithComments = parseTextWithComments

parseTypeText :: Text -> ParseResult (Type SrcSpanInfo)
parseTypeText = parseTextWithMode defaultParseMode

parseTypeTextWithMode :: ParseMode -> Text -> ParseResult (Type SrcSpanInfo)
parseTypeTextWithMode = parseTextWithMode

parseTypeTextWithComments :: ParseMode -> Text -> ParseResult (Type SrcSpanInfo, [Comment])
parseTypeTextWithComments = parseTextWithComments

parsePatText :: Text -> ParseResult (Pat SrcSpanInfo)
parsePatText = parseTextWithMode defaultParseMode

parsePatTextWithMode :: ParseMode -> Text -> ParseResult (Pat SrcSpanInfo)
parsePatTextWithMode = parseTextWithMode

parsePatTextWithComments :: ParseMode -> Text -> ParseResult (Pat SrcSpanInfo, [Comment])
parsePatTextWithComments = parseTextWithComments

parseStmtText :: Text -> ParseResult (Stmt SrcSpanInfo)
parseStmtText = parseTextWithMode defaultParseMode

parseStmtTextWithMode :: ParseMode -> Text -> ParseResult (Stmt SrcSpanInfo)
parseStmtTextWithMode = parseTextWithMode

parseStmtTextWithComments :: ParseMode -> Text -> ParseResult (Stmt SrcSpanInfo, [Comment])
parseStmtTextWithComments = parseTextWithComments

parseImportDeclText :: Text -> ParseResult (ImportDecl SrcSpanInfo)
parseImportDeclText = parseTextWithMode defaultParseMode

parseImportDeclTextWithMode :: ParseMode -> Text -> ParseResult (ImportDecl SrcSpanInfo)
parseImportDeclTextWithMode = parseTextWithMode

parseImportDeclTextWithComments :: ParseMode -> Text -> ParseResult (ImportDecl SrcSpanInfo, [Comment])
parseImportDeclTextWithComments = parseTextWithComments

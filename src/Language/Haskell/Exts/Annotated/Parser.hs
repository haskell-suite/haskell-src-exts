{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Annotated.Parser
-- Copyright   :  (c) Niklas Broberg 2004-2009
--                (c) Michael Sloan 2013
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Annotated parser for Haskell with extensions.
--
-----------------------------------------------------------------------------
module Language.Haskell.Exts.Annotated.Parser
    (
    -- * General parsing
      Parseable(parse, parseWithMode, parseWithComments)
    -- * Parsing of specific AST elements
    -- ** Modules
    , parseModule, parseModuleWithMode, parseModuleWithComments
    , parseModules, parseModulesWithMode, parseModulesWithComments
    -- ** Expressions
    , parseExp, parseExpWithMode, parseExpWithComments
    -- ** Statements
    , parseStmt, parseStmtWithMode, parseStmtWithComments
    -- ** Patterns
    , parsePat, parsePatWithMode, parsePatWithComments
    -- ** Declarations
    , parseDecl, parseDeclWithMode, parseDeclWithComments
    -- ** Types
    , parseType, parseTypeWithMode, parseTypeWithComments
    -- ** Module pragmas
    , getTopPragmas, parseExtensions, readExtensions
    ) where

import Language.Haskell.Exts.Annotated.Fixity
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.InternalParser
import Language.Haskell.Exts.ParseMonad
import Language.Haskell.Exts.SrcLoc

import Data.Either (partitionEithers)

applyFixities' :: AppFixity a => P a -> Maybe [Fixity] -> P a
applyFixities' p Nothing = p
applyFixities' p (Just fixs) = p >>= \ast -> applyFixities fixs ast `atSrcLoc` noLoc

-- It's safe to forget about the previous SrcSpanInfo 'srcInfoPoints', because
-- I've checked all of the relevant parsers, and these particular ones
-- (presently) are all created with 'noInfoSpan' ('nIS'), '(<^^>)', or '(<++>)',
-- all of which have empty 'srcInfoPoints'. Ideally, the parsers would return
-- better types, but this works.
handleSpans :: (a, [SrcSpan], SrcSpanInfo) -> (a, SrcSpanInfo)
handleSpans (x, ss, l) = (x, infoSpan (srcInfoSpan l) ss)

instance Parseable (Activation     SrcSpanInfo) where parser = applyFixities' mparseActivation
instance Parseable (Alt            SrcSpanInfo) where parser = applyFixities' mparseAlt
instance Parseable (Annotation     SrcSpanInfo) where parser = applyFixities' mparseAnnotation
instance Parseable (BangType       SrcSpanInfo) where parser = applyFixities' mparseBangType -- NOTE: allows type syntax even where it might be invalid (when used in an infix data constructor declaration)
instance Parseable (Binds          SrcSpanInfo) where parser = applyFixities' mparseBinds -- NOTE: doesn't parse "IPBinds"
instance Parseable (CallConv       SrcSpanInfo) where parser = applyFixities' mparseCallConv
instance Parseable (ClassDecl      SrcSpanInfo) where parser = applyFixities' mparseClassDecl
instance Parseable (CName          SrcSpanInfo) where parser = applyFixities' mparseCName
instance Parseable (ConDecl        SrcSpanInfo) where parser = applyFixities' mparseConDecl
instance Parseable (Decl           SrcSpanInfo) where parser = applyFixities' mparseDecl
instance Parseable (Exp            SrcSpanInfo) where parser = applyFixities' mparseExp
instance Parseable (ExportSpec     SrcSpanInfo) where parser = applyFixities' mparseExportSpec
instance Parseable (ExportSpecList SrcSpanInfo) where parser = applyFixities' mparseExportSpecList
instance Parseable (FieldDecl      SrcSpanInfo) where parser = applyFixities' mparseFieldDecl
instance Parseable (FunDep         SrcSpanInfo) where parser = applyFixities' mparseFunDep
instance Parseable (GadtDecl       SrcSpanInfo) where parser = applyFixities' mparseGadtDecl
instance Parseable (GuardedAlt     SrcSpanInfo) where parser = applyFixities' mparseGuardedAlt
instance Parseable (GuardedAlts    SrcSpanInfo) where parser = applyFixities' mparseGuardedAlts2
instance Parseable (GuardedRhs     SrcSpanInfo) where parser = applyFixities' mparseGuardedRhs
instance Parseable (ImportDecl     SrcSpanInfo) where parser = applyFixities' mparseImportDecl
instance Parseable (ImportSpec     SrcSpanInfo) where parser = applyFixities' mparseImportSpec
instance Parseable (ImportSpecList SrcSpanInfo) where parser = applyFixities' mparseImportSpecList
instance Parseable (InstDecl       SrcSpanInfo) where parser = applyFixities' mparseInstDecl
instance Parseable (IPBind         SrcSpanInfo) where parser = applyFixities' mparseIPBind
instance Parseable (IPName         SrcSpanInfo) where parser = applyFixities' mparseIPName
instance Parseable (Kind           SrcSpanInfo) where parser = applyFixities' mparseKind
instance Parseable (Literal        SrcSpanInfo) where parser = applyFixities' mparseLiteral
instance Parseable (Module         SrcSpanInfo) where parser = applyFixities' mparseModule
instance Parseable (ModuleHead     SrcSpanInfo) where parser = applyFixities' mparseModuleHead
instance Parseable (ModuleName     SrcSpanInfo) where parser = applyFixities' mparseModuleName
instance Parseable (ModulePragma   SrcSpanInfo) where parser = applyFixities' mparseModulePragma
instance Parseable (Name           SrcSpanInfo) where parser = applyFixities' mparseName
instance Parseable (Op             SrcSpanInfo) where parser = applyFixities' mparseOp
instance Parseable (Pat            SrcSpanInfo) where parser = applyFixities' mparsePat
instance Parseable (QName          SrcSpanInfo) where parser = applyFixities' mparseQName
instance Parseable (QOp            SrcSpanInfo) where parser = applyFixities' mparseQOp
instance Parseable (QualConDecl    SrcSpanInfo) where parser = applyFixities' mparseQualConDecl
instance Parseable (QualStmt       SrcSpanInfo) where parser = applyFixities' mparseQualStmt
instance Parseable (Rhs            SrcSpanInfo) where parser = applyFixities' mparseRhs
instance Parseable (Rule           SrcSpanInfo) where parser = applyFixities' mparseRule
instance Parseable (RuleVar        SrcSpanInfo) where parser = applyFixities' mparseRuleVar
instance Parseable (Safety         SrcSpanInfo) where parser = applyFixities' mparseSafety
instance Parseable (Stmt           SrcSpanInfo) where parser = applyFixities' mparseStmt
instance Parseable (Type           SrcSpanInfo) where parser = applyFixities' mparseType
instance Parseable (TyVarBind      SrcSpanInfo) where parser = applyFixities' mparseTyVarBind
instance Parseable (XName          SrcSpanInfo) where parser = applyFixities' mparseXName

instance Parseable [Module         SrcSpanInfo] where parser = applyFixities' mparseModules
instance Parseable [Rule           SrcSpanInfo] where parser = applyFixities' mparseRules
instance Parseable [RuleVar        SrcSpanInfo] where parser = applyFixities' mparseRuleVars

instance Parseable ([ClassDecl     SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseClassDecls
instance Parseable ([CName         SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseCNames
instance Parseable ([Decl          SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseDecls
instance Parseable ([Exp           SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseExps
instance Parseable ([FieldDecl     SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseFieldDecls
instance Parseable ([FunDep        SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseFunDeps
instance Parseable ([GadtDecl      SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseGadtDecls
instance Parseable ([GuardedAlt    SrcSpanInfo], SrcSpanInfo) where parser =                    applyFixities' mparseGuardedAlts
instance Parseable ([GuardedRhs    SrcSpanInfo], SrcSpanInfo) where parser =                    applyFixities' mparseGuardedRhss
instance Parseable ([ImportDecl    SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseImportDecls
instance Parseable ([InstDecl      SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseInstDecls
instance Parseable ([IPBind        SrcSpanInfo], SrcSpanInfo) where parser =                    applyFixities' mparseIPBinds
instance Parseable ([ModulePragma  SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseModulePragmas
instance Parseable ([Name          SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseNames
instance Parseable ([Op            SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseOps
instance Parseable ([Pat           SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparsePats
instance Parseable ([QName         SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseQNames
instance Parseable ([QualConDecl   SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseQualConDecls
instance Parseable ([QualStmt      SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseQualStmts
instance Parseable ([Stmt          SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseStmts
instance Parseable ([TyVarBind     SrcSpanInfo], SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseTyVarBinds

instance Parseable ([[QualStmt     SrcSpanInfo]],SrcSpanInfo) where parser = fmap handleSpans . applyFixities' mparseParQualStmts

{-
instance Parseable (DeclHead       SrcSpanInfo) where parser = applyFixities' mparseDeclHead
instance Parseable (InstHead       SrcSpanInfo) where parser = applyFixities' mparseInstHead
instance Parseable (Asst           SrcSpanInfo) where parser = applyFixities' mparseAsst
instance Parseable (Match          SrcSpanInfo) where parser = applyFixities' mparseMatch
instance Parseable (Context        SrcSpanInfo) where parser = applyFixities' mparseContext
instance Parseable (FieldUpdate    SrcSpanInfo) where parser = applyFixities' mparseFieldUpdate
instance Parseable (XAttr          SrcSpanInfo) where parser = applyFixities' mparseXAttr
instance Parseable (PXAttr         SrcSpanInfo) where parser = applyFixities' mparsePXAttr
instance Parseable (RPat           SrcSpanInfo) where parser = applyFixities' mparseRPat
instance Parseable (RPatOp         SrcSpanInfo) where parser = applyFixities' mparseRPatOp
instance Parseable (SpecialCon     SrcSpanInfo) where parser = applyFixities' mparseSpecialCon
instance Parseable (Bracket        SrcSpanInfo) where parser = applyFixities' mparseBracket
instance Parseable (Splice         SrcSpanInfo) where parser = applyFixities' mparseSplice
instance Parseable (Tool           SrcSpanInfo) where parser = applyFixities' mparseTool
instance Parseable (PatField       SrcSpanInfo) where parser = applyFixities' mparsePatField
-}


-- Type-specific functions

-- | Parse of a string, which should contain a complete Haskell module, using 'defaultParseMode'.
parseModule :: String -> ParseResult (Module SrcSpanInfo)
parseModule = parse

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode'.
parseModuleWithMode :: ParseMode -> String -> ParseResult (Module SrcSpanInfo)
parseModuleWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseModuleWithComments :: ParseMode -> String -> ParseResult (Module SrcSpanInfo, [Comment])
parseModuleWithComments = parseWithComments

-- | Parse of a string, which should contain complete Haskell modules, using 'defaultParseMode'.
parseModules :: String -> ParseResult [Module SrcSpanInfo]
parseModules = parse

-- | Parse of a string containing complete Haskell modules, using an explicit 'ParseMode'.
parseModulesWithMode :: ParseMode -> String -> ParseResult [Module SrcSpanInfo]
parseModulesWithMode = parseWithMode

-- | Parse of a string containing complete Haskell modules, using an explicit 'ParseMode', retaining comments.
parseModulesWithComments :: ParseMode -> String -> ParseResult ([Module SrcSpanInfo], [Comment])
parseModulesWithComments = parseWithComments

-- | Parse of a string containing a Haskell expression, using 'defaultParseMode'.
parseExp :: String -> ParseResult (Exp SrcSpanInfo)
parseExp = parse

-- | Parse of a string containing a Haskell expression, using an explicit 'ParseMode'.
parseExpWithMode :: ParseMode -> String -> ParseResult (Exp SrcSpanInfo)
parseExpWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseExpWithComments :: ParseMode -> String -> ParseResult (Exp SrcSpanInfo, [Comment])
parseExpWithComments = parseWithComments

-- | Parse of a string containing a Haskell pattern, using 'defaultParseMode'.
parsePat :: String -> ParseResult (Pat SrcSpanInfo)
parsePat = parse

-- | Parse of a string containing a Haskell pattern, using an explicit 'ParseMode'.
parsePatWithMode :: ParseMode -> String -> ParseResult (Pat SrcSpanInfo)
parsePatWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parsePatWithComments :: ParseMode -> String -> ParseResult (Pat SrcSpanInfo, [Comment])
parsePatWithComments = parseWithComments

-- | Parse of a string containing a Haskell top-level declaration, using 'defaultParseMode'.
parseDecl :: String -> ParseResult (Decl SrcSpanInfo)
parseDecl = parse

-- | Parse of a string containing a Haskell top-level declaration, using an explicit 'ParseMode'.
parseDeclWithMode :: ParseMode -> String -> ParseResult (Decl SrcSpanInfo)
parseDeclWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseDeclWithComments :: ParseMode -> String -> ParseResult (Decl SrcSpanInfo, [Comment])
parseDeclWithComments = parseWithComments

-- | Parse of a string containing a Haskell type, using 'defaultParseMode'.
parseType :: String -> ParseResult (Type SrcSpanInfo)
parseType = parse

-- | Parse of a string containing a Haskell type, using an explicit 'ParseMode'.
parseTypeWithMode :: ParseMode -> String -> ParseResult (Type SrcSpanInfo)
parseTypeWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseTypeWithComments :: ParseMode -> String -> ParseResult (Type SrcSpanInfo, [Comment])
parseTypeWithComments = parseWithComments

-- | Parse of a string containing a Haskell statement, using 'defaultParseMode'.
parseStmt :: String -> ParseResult (Stmt SrcSpanInfo)
parseStmt = parse

-- | Parse of a string containing a Haskell type, using an explicit 'ParseMode'.
parseStmtWithMode :: ParseMode -> String -> ParseResult (Stmt SrcSpanInfo)
parseStmtWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseStmtWithComments :: ParseMode -> String -> ParseResult (Stmt SrcSpanInfo, [Comment])
parseStmtWithComments = parseWithComments

-- Pragma parsers

-- | Partial parse of a string starting with a series of top-level option pragmas.
getTopPragmas :: String -> ParseResult [ModulePragma SrcSpanInfo]
getTopPragmas = runParser (mfindOptPragmas >>= \(ps,_,_) -> return ps)

-- | Partial parse of the languages and extensions in LANGUAGE pragmas.
parseExtensions :: String -> ParseResult ([Language], [Extension])
parseExtensions str =
    fmap (partitionEithers . concatMap getExts) $ getTopPragmas str
  where
    getExts :: ModulePragma l -> [Either Language Extension]
    getExts (LanguagePragma _ ns) = map readExt ns
    getExts _ = []

    readExt (Ident _ e) =
        case classifyLanguage e of
          UnknownLanguage _ -> Right $ classifyExtension e
          lang -> Left lang

-- | Gather the extensions declared in LANGUAGE pragmas
--   at the top of the file. Returns 'Nothing' if the
--   parse of the pragmas fails.
readExtensions :: String -> Maybe (Maybe Language, [Extension])
readExtensions str =
    case parseExtensions str of
        ParseOk ([], es) -> Just (Nothing, es)
        ParseOk ([l], es) -> Just (Just l, es)
        ParseOk (_, _) -> Nothing
        ParseFailed _ _ -> Nothing
{-# DEPRECATED readExtensions "Prefer parseExtensions" #-}

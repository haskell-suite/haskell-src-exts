{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, DeriveFunctor #-}
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
    , ListOf(..), unListOf
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
    -- * Non-greedy parsers
    , NonGreedy(..)
    -- ** Module head parsers
    , getTopPragmas, readExtensions, pragmasToExtensions
    , PragmasAndModuleName(..), PragmasAndModuleHead(..), ModuleHeadAndImports(..)
    -- * CPP Utilities
    , ignoreCpp, ignoreCppLines
    ) where

import Language.Haskell.Exts.Annotated.Fixity
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.InternalParser
import Language.Haskell.Exts.ParseMonad hiding (getModuleName)
import Language.Haskell.Exts.SrcLoc

#ifdef __GLASGOW_HASKELL__
#ifdef BASE4
import Data.Data hiding (Fixity)
#else
import Data.Generics (Data(..),Typeable(..))
#endif
#endif

import Data.Either (partitionEithers)
import Data.List (foldl1')
import Data.Maybe (listToMaybe)

-- | @ListOf a@ stores lists of the AST type @a@, along with a 'SrcSpanInfo',
--   in order to provide 'Parseable' instances for lists.  These instances are
--   provided when the type is used as a list in the syntax, and the same
--   delimiters are used in all of its usages. Some exceptions are made:
--
--     * @ListOf (Exp SrcSpanInfo)@ is expected to have syntax like @ e1, e2 @.
--       'XTag', 'XChildTag', 'XmlPage', and 'XmlHybrid' don't use this syntax.
--
--     * @ListOf (Pat SrcSpanInfo)@ is expected to have syntax like @ p1, p2 @.
--       'Match', 'Lambda', 'PApp', and 'PXTag' don't use this syntax.
--
--     * Similarly, @ListOf (Name SrcSpanInfo)@ is expected to have syntax like
--       @ n1, n2 @.  However, this is only used directly in DEPRECATED and
--       WARNING pragamas.  Most of the other usages of @[Name l]@ in the AST
--       are compatible with this, except that they demand specific varieties of
--       names. 'TypeSig' and 'FieldDecl' expect value identifiers, whereas
--       'LanguagePragma' expects uppercase identifiers.  Also, 'FunDep' expects
--       space-separated identifiers.
--
--     * @[QName l]@ is never used in the syntax, but a parser for @ qn1, qn2 @
--       is provided anyway.
--
--     * @ListOf [Stmt SrcSpanInfo]@ parses statements used in 'Do', 'MDo', or
--       'RecStmt'.  However, statements are also separated by commas when used
--        as guards in 'RPGuard', 'GuardedAlt', and 'GuardedRhs'.
--
--     * @ListOf [QualStmt SrcSpanInfo]@ parses the portion of a parallel list
--       comprehension that comes after the expression.  There aren't any
--       exceptions to this in the AST, but it's worth pointing out.
--
data ListOf a = ListOf SrcSpanInfo [a]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
#else
  deriving (Eq,Ord,Show)
#endif

unListOf :: ListOf a -> [a]
unListOf (ListOf _ xs) = xs

-- It's safe to forget about the previous SrcSpanInfo 'srcInfoPoints', because
-- I've checked all of the relevant parsers, and these particular ones
-- (presently) are all created with 'noInfoSpan' ('nIS'), '(<^^>)', or '(<++>)',
-- all of which have empty 'srcInfoPoints'. Ideally, the parsers would return
-- better types, but this works.
toListOf :: ([a], [SrcSpan], SrcSpanInfo) -> ListOf a
toListOf (xs, ss, l) = ListOf (infoSpan (srcInfoSpan l) ss) xs

toListOf' :: ([a], SrcSpanInfo) -> ListOf a
toListOf' (xs, l) = ListOf l xs

toListOf'' :: (a -> SrcSpanInfo) -> [a] -> ListOf a
toListOf'' f [] = ListOf (noInfoSpan (mkSrcSpan noLoc noLoc)) []
toListOf'' f xs = ListOf (foldl1' (<++>) $ map f xs) xs

normalParser :: AppFixity a => P a -> Maybe [Fixity] -> P a
normalParser p Nothing = p
normalParser p (Just fixs) = p >>= \ast -> applyFixities fixs ast `atSrcLoc` noLoc

listParser :: AppFixity a => P ([a], [SrcSpan], SrcSpanInfo) -> Maybe [Fixity] -> P (ListOf a)
listParser f = fmap toListOf . normalParser f

listParser' :: AppFixity a => P ([a], SrcSpanInfo) -> Maybe [Fixity] -> P (ListOf a)
listParser' f = fmap toListOf' . normalParser f

listParser'' :: AppFixity a => (a -> SrcSpanInfo) -> P [a] -> Maybe [Fixity] -> P (ListOf a)
listParser'' ann f = fmap (toListOf'' ann) . normalParser f

instance Parseable (Activation     SrcSpanInfo) where parser = normalParser mparseActivation
instance Parseable (Alt            SrcSpanInfo) where parser = normalParser mparseAlt
instance Parseable (Annotation     SrcSpanInfo) where parser = normalParser mparseAnnotation
instance Parseable (BangType       SrcSpanInfo) where parser = normalParser mparseBangType -- NOTE: allows type syntax even where it might be invalid (when used in an infix data constructor declaration)
instance Parseable (Binds          SrcSpanInfo) where parser = normalParser mparseBinds -- NOTE: doesn't parse "IPBinds"
instance Parseable (CallConv       SrcSpanInfo) where parser = normalParser mparseCallConv
instance Parseable (ClassDecl      SrcSpanInfo) where parser = normalParser mparseClassDecl
instance Parseable (CName          SrcSpanInfo) where parser = normalParser mparseCName
instance Parseable (ConDecl        SrcSpanInfo) where parser = normalParser mparseConDecl
instance Parseable (Decl           SrcSpanInfo) where parser = normalParser mparseDecl
instance Parseable (Exp            SrcSpanInfo) where parser = normalParser mparseExp
instance Parseable (ExportSpec     SrcSpanInfo) where parser = normalParser mparseExportSpec
instance Parseable (ExportSpecList SrcSpanInfo) where parser = normalParser mparseExportSpecList
instance Parseable (FieldDecl      SrcSpanInfo) where parser = normalParser mparseFieldDecl
instance Parseable (FunDep         SrcSpanInfo) where parser = normalParser mparseFunDep
instance Parseable (GadtDecl       SrcSpanInfo) where parser = normalParser mparseGadtDecl
instance Parseable (GuardedAlt     SrcSpanInfo) where parser = normalParser mparseGuardedAlt
instance Parseable (GuardedAlts    SrcSpanInfo) where parser = normalParser mparseGuardedAlts2
instance Parseable (GuardedRhs     SrcSpanInfo) where parser = normalParser mparseGuardedRhs
instance Parseable (ImportDecl     SrcSpanInfo) where parser = normalParser mparseImportDecl
instance Parseable (ImportSpec     SrcSpanInfo) where parser = normalParser mparseImportSpec
instance Parseable (ImportSpecList SrcSpanInfo) where parser = normalParser mparseImportSpecList
instance Parseable (InstDecl       SrcSpanInfo) where parser = normalParser mparseInstDecl
instance Parseable (IPBind         SrcSpanInfo) where parser = normalParser mparseIPBind
instance Parseable (IPName         SrcSpanInfo) where parser = normalParser mparseIPName
instance Parseable (Kind           SrcSpanInfo) where parser = normalParser mparseKind
instance Parseable (Literal        SrcSpanInfo) where parser = normalParser mparseLiteral
instance Parseable (Module         SrcSpanInfo) where parser = normalParser mparseModule
instance Parseable (ModuleHead     SrcSpanInfo) where parser = normalParser mparseModuleHead
instance Parseable (ModuleName     SrcSpanInfo) where parser = normalParser mparseModuleName
instance Parseable (ModulePragma   SrcSpanInfo) where parser = normalParser mparseModulePragma
instance Parseable (Name           SrcSpanInfo) where parser = normalParser mparseName
instance Parseable (Op             SrcSpanInfo) where parser = normalParser mparseOp
instance Parseable (Pat            SrcSpanInfo) where parser = normalParser mparsePat
instance Parseable (QName          SrcSpanInfo) where parser = normalParser mparseQName
instance Parseable (QOp            SrcSpanInfo) where parser = normalParser mparseQOp
instance Parseable (QualConDecl    SrcSpanInfo) where parser = normalParser mparseQualConDecl
instance Parseable (QualStmt       SrcSpanInfo) where parser = normalParser mparseQualStmt
instance Parseable (Rhs            SrcSpanInfo) where parser = normalParser mparseRhs
instance Parseable (Rule           SrcSpanInfo) where parser = normalParser mparseRule
instance Parseable (RuleVar        SrcSpanInfo) where parser = normalParser mparseRuleVar
instance Parseable (Safety         SrcSpanInfo) where parser = normalParser mparseSafety
instance Parseable (Stmt           SrcSpanInfo) where parser = normalParser mparseStmt
instance Parseable (Type           SrcSpanInfo) where parser = normalParser mparseType
instance Parseable (TyVarBind      SrcSpanInfo) where parser = normalParser mparseTyVarBind
instance Parseable (XName          SrcSpanInfo) where parser = normalParser mparseXName

instance Parseable (ListOf (ClassDecl    SrcSpanInfo)) where parser = listParser       mparseClassDecls
instance Parseable (ListOf (CName        SrcSpanInfo)) where parser = listParser       mparseCNames
instance Parseable (ListOf (Decl         SrcSpanInfo)) where parser = listParser       mparseDecls
instance Parseable (ListOf (Exp          SrcSpanInfo)) where parser = listParser       mparseExps
instance Parseable (ListOf (FieldDecl    SrcSpanInfo)) where parser = listParser       mparseFieldDecls
instance Parseable (ListOf (FunDep       SrcSpanInfo)) where parser = listParser       mparseFunDeps
instance Parseable (ListOf (GadtDecl     SrcSpanInfo)) where parser = listParser       mparseGadtDecls
instance Parseable (ListOf (GuardedAlt   SrcSpanInfo)) where parser = listParser'      mparseGuardedAlts
instance Parseable (ListOf (GuardedRhs   SrcSpanInfo)) where parser = listParser'      mparseGuardedRhss
instance Parseable (ListOf (ImportDecl   SrcSpanInfo)) where parser = listParser       mparseImportDecls
instance Parseable (ListOf (InstDecl     SrcSpanInfo)) where parser = listParser       mparseInstDecls
instance Parseable (ListOf (IPBind       SrcSpanInfo)) where parser = listParser'      mparseIPBinds
instance Parseable (ListOf (Module       SrcSpanInfo)) where parser = listParser'' ann mparseModules
instance Parseable (ListOf (ModulePragma SrcSpanInfo)) where parser = listParser       mparseModulePragmas
instance Parseable (ListOf (Name         SrcSpanInfo)) where parser = listParser       mparseNames
instance Parseable (ListOf (Op           SrcSpanInfo)) where parser = listParser       mparseOps
instance Parseable (ListOf (Pat          SrcSpanInfo)) where parser = listParser       mparsePats
instance Parseable (ListOf (QName        SrcSpanInfo)) where parser = listParser       mparseQNames
instance Parseable (ListOf (QualConDecl  SrcSpanInfo)) where parser = listParser       mparseQualConDecls
instance Parseable (ListOf (QualStmt     SrcSpanInfo)) where parser = listParser       mparseQualStmts
instance Parseable (ListOf (Rule         SrcSpanInfo)) where parser = listParser'' ann mparseRules
instance Parseable (ListOf (RuleVar      SrcSpanInfo)) where parser = listParser'' ann mparseRuleVars
instance Parseable (ListOf (Stmt         SrcSpanInfo)) where parser = listParser       mparseStmts
instance Parseable (ListOf (TyVarBind    SrcSpanInfo)) where parser = listParser       mparseTyVarBinds

instance Parseable (ListOf [QualStmt     SrcSpanInfo]) where parser = listParser mparseParQualStmts

-- Non-greedy parsers (should use ng- prefixed parses exported by InternalParser)

ngnormalParser :: AppFixity a => P a -> Maybe [Fixity] -> P (NonGreedy a)
ngnormalParser p = fmap NonGreedy . normalParser p

nglistParser :: AppFixity a => P ([a], [SrcSpan], SrcSpanInfo) -> Maybe [Fixity] -> P (NonGreedy (ListOf a))
nglistParser f = fmap (NonGreedy . toListOf) . normalParser f

nglistParser' :: AppFixity a => P ([a], SrcSpanInfo) -> Maybe [Fixity] -> P (NonGreedy (ListOf a))
nglistParser' f = fmap (NonGreedy . toListOf') . normalParser f

nglistParser'' :: AppFixity a => (a -> SrcSpanInfo) -> P [a] -> Maybe [Fixity] -> P (NonGreedy (ListOf a))
nglistParser'' ann f = fmap (NonGreedy . toListOf'' ann) . normalParser f

instance Parseable (NonGreedy (Activation     SrcSpanInfo)) where parser = ngnormalParser ngparseActivation
instance Parseable (NonGreedy (Alt            SrcSpanInfo)) where parser = ngnormalParser ngparseAlt
instance Parseable (NonGreedy (Annotation     SrcSpanInfo)) where parser = ngnormalParser ngparseAnnotation
instance Parseable (NonGreedy (BangType       SrcSpanInfo)) where parser = ngnormalParser ngparseBangType -- NOTE: allows type syntax even where it might be invalid (when used in an infix data constructor declaration)
instance Parseable (NonGreedy (Binds          SrcSpanInfo)) where parser = ngnormalParser ngparseBinds -- NOTE: doesn't parse "IPBinds"
instance Parseable (NonGreedy (CallConv       SrcSpanInfo)) where parser = ngnormalParser ngparseCallConv
instance Parseable (NonGreedy (ClassDecl      SrcSpanInfo)) where parser = ngnormalParser ngparseClassDecl
instance Parseable (NonGreedy (CName          SrcSpanInfo)) where parser = ngnormalParser ngparseCName
instance Parseable (NonGreedy (ConDecl        SrcSpanInfo)) where parser = ngnormalParser ngparseConDecl
instance Parseable (NonGreedy (Decl           SrcSpanInfo)) where parser = ngnormalParser ngparseDecl
instance Parseable (NonGreedy (Exp            SrcSpanInfo)) where parser = ngnormalParser ngparseExp
instance Parseable (NonGreedy (ExportSpec     SrcSpanInfo)) where parser = ngnormalParser ngparseExportSpec
instance Parseable (NonGreedy (ExportSpecList SrcSpanInfo)) where parser = ngnormalParser ngparseExportSpecList
instance Parseable (NonGreedy (FieldDecl      SrcSpanInfo)) where parser = ngnormalParser ngparseFieldDecl
instance Parseable (NonGreedy (FunDep         SrcSpanInfo)) where parser = ngnormalParser ngparseFunDep
instance Parseable (NonGreedy (GadtDecl       SrcSpanInfo)) where parser = ngnormalParser ngparseGadtDecl
instance Parseable (NonGreedy (GuardedAlt     SrcSpanInfo)) where parser = ngnormalParser ngparseGuardedAlt
instance Parseable (NonGreedy (GuardedAlts    SrcSpanInfo)) where parser = ngnormalParser ngparseGuardedAlts2
instance Parseable (NonGreedy (GuardedRhs     SrcSpanInfo)) where parser = ngnormalParser ngparseGuardedRhs
instance Parseable (NonGreedy (ImportDecl     SrcSpanInfo)) where parser = ngnormalParser ngparseImportDecl
instance Parseable (NonGreedy (ImportSpec     SrcSpanInfo)) where parser = ngnormalParser ngparseImportSpec
instance Parseable (NonGreedy (ImportSpecList SrcSpanInfo)) where parser = ngnormalParser ngparseImportSpecList
instance Parseable (NonGreedy (InstDecl       SrcSpanInfo)) where parser = ngnormalParser ngparseInstDecl
instance Parseable (NonGreedy (IPBind         SrcSpanInfo)) where parser = ngnormalParser ngparseIPBind
instance Parseable (NonGreedy (IPName         SrcSpanInfo)) where parser = ngnormalParser ngparseIPName
instance Parseable (NonGreedy (Kind           SrcSpanInfo)) where parser = ngnormalParser ngparseKind
instance Parseable (NonGreedy (Literal        SrcSpanInfo)) where parser = ngnormalParser ngparseLiteral
instance Parseable (NonGreedy (Module         SrcSpanInfo)) where parser = ngnormalParser ngparseModule
instance Parseable (NonGreedy (ModuleHead     SrcSpanInfo)) where parser = ngnormalParser ngparseModuleHead
instance Parseable (NonGreedy (ModuleName     SrcSpanInfo)) where parser = ngnormalParser ngparseModuleName
instance Parseable (NonGreedy (ModulePragma   SrcSpanInfo)) where parser = ngnormalParser ngparseModulePragma
instance Parseable (NonGreedy (Name           SrcSpanInfo)) where parser = ngnormalParser ngparseName
instance Parseable (NonGreedy (Op             SrcSpanInfo)) where parser = ngnormalParser ngparseOp
instance Parseable (NonGreedy (Pat            SrcSpanInfo)) where parser = ngnormalParser ngparsePat
instance Parseable (NonGreedy (QName          SrcSpanInfo)) where parser = ngnormalParser ngparseQName
instance Parseable (NonGreedy (QOp            SrcSpanInfo)) where parser = ngnormalParser ngparseQOp
instance Parseable (NonGreedy (QualConDecl    SrcSpanInfo)) where parser = ngnormalParser ngparseQualConDecl
instance Parseable (NonGreedy (QualStmt       SrcSpanInfo)) where parser = ngnormalParser ngparseQualStmt
instance Parseable (NonGreedy (Rhs            SrcSpanInfo)) where parser = ngnormalParser ngparseRhs
instance Parseable (NonGreedy (Rule           SrcSpanInfo)) where parser = ngnormalParser ngparseRule
instance Parseable (NonGreedy (RuleVar        SrcSpanInfo)) where parser = ngnormalParser ngparseRuleVar
instance Parseable (NonGreedy (Safety         SrcSpanInfo)) where parser = ngnormalParser ngparseSafety
instance Parseable (NonGreedy (Stmt           SrcSpanInfo)) where parser = ngnormalParser ngparseStmt
instance Parseable (NonGreedy (Type           SrcSpanInfo)) where parser = ngnormalParser ngparseType
instance Parseable (NonGreedy (TyVarBind      SrcSpanInfo)) where parser = ngnormalParser ngparseTyVarBind
instance Parseable (NonGreedy (XName          SrcSpanInfo)) where parser = ngnormalParser ngparseXName

instance Parseable (NonGreedy (ListOf (ClassDecl    SrcSpanInfo))) where parser = nglistParser       ngparseClassDecls
instance Parseable (NonGreedy (ListOf (CName        SrcSpanInfo))) where parser = nglistParser       ngparseCNames
instance Parseable (NonGreedy (ListOf (Decl         SrcSpanInfo))) where parser = nglistParser       ngparseDecls
instance Parseable (NonGreedy (ListOf (Exp          SrcSpanInfo))) where parser = nglistParser       ngparseExps
instance Parseable (NonGreedy (ListOf (FieldDecl    SrcSpanInfo))) where parser = nglistParser       ngparseFieldDecls
instance Parseable (NonGreedy (ListOf (FunDep       SrcSpanInfo))) where parser = nglistParser       ngparseFunDeps
instance Parseable (NonGreedy (ListOf (GadtDecl     SrcSpanInfo))) where parser = nglistParser       ngparseGadtDecls
instance Parseable (NonGreedy (ListOf (GuardedAlt   SrcSpanInfo))) where parser = nglistParser'      ngparseGuardedAlts
instance Parseable (NonGreedy (ListOf (GuardedRhs   SrcSpanInfo))) where parser = nglistParser'      ngparseGuardedRhss
instance Parseable (NonGreedy (ListOf (ImportDecl   SrcSpanInfo))) where parser = nglistParser       ngparseImportDecls
instance Parseable (NonGreedy (ListOf (InstDecl     SrcSpanInfo))) where parser = nglistParser       ngparseInstDecls
instance Parseable (NonGreedy (ListOf (IPBind       SrcSpanInfo))) where parser = nglistParser'      ngparseIPBinds
instance Parseable (NonGreedy (ListOf (ModulePragma SrcSpanInfo))) where parser = nglistParser       ngparseModulePragmas
instance Parseable (NonGreedy (ListOf (Name         SrcSpanInfo))) where parser = nglistParser       ngparseNames
instance Parseable (NonGreedy (ListOf (Op           SrcSpanInfo))) where parser = nglistParser       ngparseOps
instance Parseable (NonGreedy (ListOf (Pat          SrcSpanInfo))) where parser = nglistParser       ngparsePats
instance Parseable (NonGreedy (ListOf (QName        SrcSpanInfo))) where parser = nglistParser       ngparseQNames
instance Parseable (NonGreedy (ListOf (QualConDecl  SrcSpanInfo))) where parser = nglistParser       ngparseQualConDecls
instance Parseable (NonGreedy (ListOf (QualStmt     SrcSpanInfo))) where parser = nglistParser       ngparseQualStmts
instance Parseable (NonGreedy (ListOf (Rule         SrcSpanInfo))) where parser = nglistParser'' ann ngparseRules
instance Parseable (NonGreedy (ListOf (RuleVar      SrcSpanInfo))) where parser = nglistParser'' ann ngparseRuleVars
instance Parseable (NonGreedy (ListOf (Stmt         SrcSpanInfo))) where parser = nglistParser       ngparseStmts
instance Parseable (NonGreedy (ListOf (TyVarBind    SrcSpanInfo))) where parser = nglistParser       ngparseTyVarBinds

instance Parseable (NonGreedy (ListOf [QualStmt     SrcSpanInfo])) where parser = nglistParser       ngparseParQualStmts

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
parseModules = fmap (fmap unListOf) parse

-- | Parse of a string containing complete Haskell modules, using an explicit 'ParseMode'.
parseModulesWithMode :: ParseMode -> String -> ParseResult [Module SrcSpanInfo]
parseModulesWithMode = fmap (fmap (fmap unListOf)) parseWithMode

-- | Parse of a string containing complete Haskell modules, using an explicit 'ParseMode', retaining comments.
parseModulesWithComments :: ParseMode -> String -> ParseResult ([Module SrcSpanInfo], [Comment])
parseModulesWithComments = fmap (fmap (fmap (\(x, cs) -> (unListOf x, cs)))) parseWithComments

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

-- Module head parsers

-- | Non-greedy parse of a string starting with a series of top-level option pragmas.
getTopPragmas :: String -> ParseResult [ModulePragma SrcSpanInfo]
getTopPragmas = fmap unListOf . parse

-- | Gather the extensions declared in LANGUAGE pragmas
--   at the top of the file. Returns 'Nothing' if the
--   parse of the pragmas fails.
readExtensions :: String -> Maybe (Maybe Language, [Extension])
readExtensions str =
    case parse str of
        ParseOk xs ->
            case pragmasToExtensions (unListOf xs :: [ModulePragma SrcSpanInfo]) of
                ([], es) -> Just (Nothing, es)
                ([l], es) -> Just (Just l, es)
                _ -> Nothing
        ParseFailed _ _ -> Nothing

pragmasToExtensions :: [ModulePragma l] -> ([Language], [Extension])
pragmasToExtensions = partitionEithers . concatMap getExts
  where
    getExts (LanguagePragma _ ns) = map readExt ns
    getExts _ = []
    readExt (Ident _ e) =
        case classifyLanguage e of
            UnknownLanguage _ -> Right $ classifyExtension e
            lang -> Left lang

-- | Instances of 'Parseable' for @NonGreedy a@ will only consume the input
--   until @a@ is fully parsed.  This means that parse errors that come later
--   in the input will be ignored.  It's also more efficient, as it's fully lazy
--   in the remainder of the input:
--
--   >>> parse (unlines ("module A where" : "main =" : repeat "blah")) :: ParseResult PragmasAndModuleHead
--   ParseOk (NonGreedy {unNonGreedy = PragmasAndModuleHead [] (ModuleName "A",Nothing,Nothing)})
--
--   (this example uses the simplified AST)
--
newtype NonGreedy a = NonGreedy { unNonGreedy :: a }
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Functor NonGreedy where
    fmap f (NonGreedy x) = NonGreedy (f x)

{-
instance Parseable (NonGreedy (ListOf (ModulePragma SrcSpanInfo))) where
    parser _ = fmap (NonGreedy . toListOf) mfindOptPragmas
-}

handleSpans :: ([a], [SrcSpan], SrcSpanInfo) -> ([a], SrcSpanInfo)
handleSpans x = (xs, l)
  where
    ListOf l xs = toListOf x

-- | Type intended to be used with 'Parseable', with instances that implement a
--   non-greedy parse of the module name, including top-level pragmas.  This
--   means that a parse error that comes after the module header won't be
--   returned. If the 'Maybe' value is 'Nothing', then this means that there was
--   no module header.
data PragmasAndModuleName l = PragmasAndModuleName
    ([ModulePragma l], l)
    (Maybe (ModuleName l))
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable (NonGreedy (PragmasAndModuleName SrcSpanInfo)) where
    parser _ = do
        (ps, mn) <- mfindModuleName
        return $ NonGreedy $ PragmasAndModuleName (handleSpans ps) mn

--   Type intended to be used with 'Parseable', with instances that implement a
--   non-greedy parse of the module name, including top-level pragmas.  This
--   means that a parse error that comes after the module header won't be
--   returned. If the 'Maybe' value is 'Nothing', this means that there was no
--   module head.
--
--   Note that the 'ParseMode' particularly matters for this due to the
--   'MagicHash' changing the lexing of identifiers to include \"#\".
data PragmasAndModuleHead l = PragmasAndModuleHead
    ([ModulePragma l], l)
    (Maybe (ModuleHead l))
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable (NonGreedy (PragmasAndModuleHead SrcSpanInfo)) where
    parser _ = do
        (ps, mn) <- mfindModuleHead
        return $ NonGreedy $ PragmasAndModuleHead (handleSpans ps) mn

--   Type intended to be used with 'Parseable', with instances that implement a
--   non-greedy parse of the module head, including top-level pragmas, module
--   name, export list, and import list. This means that if a parse error that
--   comes after the imports won't be returned.  If the 'Maybe' value is
--   'Nothing', this means that there was no module head.
--
--   Note that the 'ParseMode' particularly matters for this due to the
--   'MagicHash' changing the lexing of identifiers to include \"#\".
data ModuleHeadAndImports l = ModuleHeadAndImports
    ([ModulePragma l], l)
    (Maybe (ModuleHead l))
    (Maybe ([ImportDecl l], l))
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable (NonGreedy (ModuleHeadAndImports SrcSpanInfo)) where
    parser _ = do
        (ps, mh, mimps) <- mfindModuleImports
        return $ NonGreedy $ ModuleHeadAndImports
            (handleSpans ps)
            mh
            (fmap handleSpans mimps)

-- | Filter out lines which use CPP macros.  This is a convenient wrapper around
--   'ignoreCppLines' which applies 'lines' / 'unlines'.
ignoreCpp :: String -> String
ignoreCpp = unlines . ignoreCppLines . lines

-- | Filter out lines which use CPP macros.
ignoreCppLines :: [String] -> [String]
ignoreCppLines = go False
  where
    go _           []       = []
    go isMultiline (x : xs) =
        let isCpp         = isMultiline || listToMaybe x == Just '#'
            nextMultiline = isCpp && not (null x) && last x == '\\'
        in (if isCpp then "" else x) : go nextMultiline xs

{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances #-}
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
    -- ** Module head parsers
    , getTopPragmas, readExtensions
    , NonGreedyTopPragmas(..), NonGreedyExtensions(..)
    , NonGreedyModuleName(..), NonGreedyModuleHead(..), NonGreedyModuleImports(..)
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

-- | Parse of a string, which should contain a complete Haskell module.
parseModule :: String -> ParseResult (Module SrcSpanInfo)
parseModule = parse

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode'.
parseModuleWithMode :: ParseMode -> String -> ParseResult (Module SrcSpanInfo)
parseModuleWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseModuleWithComments :: ParseMode -> String -> ParseResult (Module SrcSpanInfo, [Comment])
parseModuleWithComments = parseWithComments

-- | Parse of a string, which should contain complete Haskell modules.
parseModules :: String -> ParseResult [Module SrcSpanInfo]
parseModules = parse

-- | Parse of a string containing complete Haskell modules, using an explicit 'ParseMode'.
parseModulesWithMode :: ParseMode -> String -> ParseResult [Module SrcSpanInfo]
parseModulesWithMode = parseWithMode

-- | Parse of a string containing complete Haskell modules, using an explicit 'ParseMode', retaining comments.
parseModulesWithComments :: ParseMode -> String -> ParseResult ([Module SrcSpanInfo], [Comment])
parseModulesWithComments = parseWithComments

-- | Parse of a string containing a Haskell expression.
parseExp :: String -> ParseResult (Exp SrcSpanInfo)
parseExp = parse

-- | Parse of a string containing a Haskell expression, using an explicit 'ParseMode'.
parseExpWithMode :: ParseMode -> String -> ParseResult (Exp SrcSpanInfo)
parseExpWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseExpWithComments :: ParseMode -> String -> ParseResult (Exp SrcSpanInfo, [Comment])
parseExpWithComments = parseWithComments

-- | Parse of a string containing a Haskell pattern.
parsePat :: String -> ParseResult (Pat SrcSpanInfo)
parsePat = parse

-- | Parse of a string containing a Haskell pattern, using an explicit 'ParseMode'.
parsePatWithMode :: ParseMode -> String -> ParseResult (Pat SrcSpanInfo)
parsePatWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parsePatWithComments :: ParseMode -> String -> ParseResult (Pat SrcSpanInfo, [Comment])
parsePatWithComments = parseWithComments

-- | Parse of a string containing a Haskell top-level declaration.
parseDecl :: String -> ParseResult (Decl SrcSpanInfo)
parseDecl = parse

-- | Parse of a string containing a Haskell top-level declaration, using an explicit 'ParseMode'.
parseDeclWithMode :: ParseMode -> String -> ParseResult (Decl SrcSpanInfo)
parseDeclWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseDeclWithComments :: ParseMode -> String -> ParseResult (Decl SrcSpanInfo, [Comment])
parseDeclWithComments = parseWithComments

-- | Parse of a string containing a Haskell type.
parseType :: String -> ParseResult (Type SrcSpanInfo)
parseType = parse

-- | Parse of a string containing a Haskell type, using an explicit 'ParseMode'.
parseTypeWithMode :: ParseMode -> String -> ParseResult (Type SrcSpanInfo)
parseTypeWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseTypeWithComments :: ParseMode -> String -> ParseResult (Type SrcSpanInfo, [Comment])
parseTypeWithComments = parseWithComments

-- | Parse of a string containing a Haskell statement.
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
getTopPragmas = fmap (\(NonGreedyTopPragmas ps _) -> ps) . parse

-- | Gather the extensions declared in LANGUAGE pragmas
--   at the top of the file. Returns 'Nothing' if the
--   parse of the pragmas fails.
readExtensions :: String -> Maybe (Maybe Language, [Extension])
readExtensions str =
    case parse str of
        ParseOk (NonGreedyExtensions [] es) -> Just (Nothing, es)
        ParseOk (NonGreedyExtensions [l] es) -> Just (Just l, es)
        ParseOk (NonGreedyExtensions _ _) -> Nothing
        ParseFailed _ _ -> Nothing
{-# DEPRECATED readExtensions "Prefer using parse with NonGreedyExtensions" #-}

data NonGreedyTopPragmas l = NonGreedyTopPragmas [ModulePragma l] l
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable (NonGreedyTopPragmas SrcSpanInfo) where
    parser _ = do
        (ps, l) <- fmap handleSpans mfindOptPragmas
        return $ NonGreedyTopPragmas ps l

data NonGreedyExtensions = NonGreedyExtensions [Language] [Extension]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable NonGreedyExtensions where
    parser fixs = do
        NonGreedyTopPragmas ps _ <- parser fixs
        let (ls, es) = partitionEithers $ concatMap getExts ps
        return $ NonGreedyExtensions ls es
      where
        getExts :: ModulePragma SrcSpanInfo -> [Either Language Extension]
        getExts (LanguagePragma _ ns) = map readExt ns
        getExts _ = []

        readExt (Ident _ e) =
            case classifyLanguage e of
                UnknownLanguage _ -> Right $ classifyExtension e
                lang -> Left lang

--   Type intended to be used with 'Parseable', with instances that implement a
--   non-greedy parse of the module name, including top-level pragmas.  This
--   means that a parse error that comes after the module header won't be
--   returned. If the 'Maybe' value is 'Nothing', then this means that there was
--   no module header.
data NonGreedyModuleName l = NonGreedyModuleName
    ([ModulePragma l], l)
    (Maybe (ModuleName l))
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable (NonGreedyModuleName SrcSpanInfo) where
    parser _ = do
        (ps, mn) <- mfindModuleName
        return $ NonGreedyModuleName (handleSpans ps) mn

--   Type intended to be used with 'Parseable', with instances that implement a
--   non-greedy parse of the module name, including top-level pragmas.  This
--   means that a parse error that comes after the module header won't be
--   returned. If the 'Maybe' value is 'Nothing', this means that there was no
--   module head.
--
--   Note that the 'ParseMode' particularly matters for this due to the
--   'MagicHash' changing the lexing of identifiers to include \"#\".
data NonGreedyModuleHead l = NonGreedyModuleHead
    ([ModulePragma l], l)
    (Maybe (ModuleHead l))
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable (NonGreedyModuleHead SrcSpanInfo) where
    parser _ = do
        (ps, mh) <- mfindModuleHead
        return $ NonGreedyModuleHead (handleSpans ps) mh

--   Type intended to be used with 'Parseable', with instances that implement a
--   non-greedy parse of the module head, including top-level pragmas, module
--   name, export list, and import list. This means that if a parse error that
--   comes after the imports won't be returned.  If the 'Maybe' value is
--   'Nothing', this means that there was no module head.
--
--   Note that the 'ParseMode' particularly matters for this due to the
--   'MagicHash' changing the lexing of identifiers to include \"#\".
data NonGreedyModuleImports l = NonGreedyModuleImports
    ([ModulePragma l], l)
    (Maybe (ModuleHead l))
    (Maybe ([ImportDecl l], l))
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable (NonGreedyModuleImports SrcSpanInfo) where
    parser _ = do
        (ps, mh, mimps) <- mfindModuleImports
        return $ NonGreedyModuleImports
            (handleSpans ps)
            mh
            (fmap handleSpans mimps)

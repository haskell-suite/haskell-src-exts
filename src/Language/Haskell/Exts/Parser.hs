{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Parser
-- Copyright   :  (c) The GHC Team, 1997-2000
--                (c) Niklas Broberg, 2004-2012
--                (c) Michael Sloan, 2013
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, niklas.broberg@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Parser for Haskell with extensions.
--
-----------------------------------------------------------------------------
module Language.Haskell.Exts.Parser
            (
                -- * General parsing
                Parseable(parse, parseWithMode, parseWithComments),
                ParseMode(..), defaultParseMode, ParseResult(..), fromParseResult,
                -- * Parsing of specific AST elements
                -- ** Modules
                parseModule, parseModuleWithMode, parseModuleWithComments,
                parseModules, parseModulesWithMode, parseModulesWithComments,
                -- ** Expressions
                parseExp, parseExpWithMode, parseExpWithComments,
                -- ** Statements
                parseStmt, parseStmtWithMode, parseStmtWithComments,
                -- ** Patterns
                parsePat, parsePatWithMode, parsePatWithComments,
                -- ** Declarations
                parseDecl, parseDeclWithMode, parseDeclWithComments,
                -- ** Types
                parseType, parseTypeWithMode, parseTypeWithComments,
                -- ** Module head parsers
                getTopPragmas, readExtensions,
                NonGreedyTopPragmas(..), NonGreedyExtensions(..),
                NonGreedyModuleName(..), NonGreedyModuleHead(..), NonGreedyModuleImports(..)
            ) where

import Language.Haskell.Exts.Annotated.Fixity
import Language.Haskell.Exts.Annotated.Parser (readExtensions, NonGreedyExtensions(..))
import Language.Haskell.Exts.Annotated.Simplify
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.ParseMonad hiding (getModuleName)
import Language.Haskell.Exts.SrcLoc
import Data.Maybe (fromMaybe)

import qualified Language.Haskell.Exts.Annotated.Parser as A
import qualified Language.Haskell.Exts.Annotated.Syntax as A
import qualified Language.Haskell.Exts.Syntax as S

#ifdef __GLASGOW_HASKELL__
#ifdef BASE4
import Data.Data hiding (Fixity)
#else
import Data.Generics (Data(..),Typeable(..))
#endif
#endif

parseWithSimplify :: Parseable a => a -> (a -> a') -> Maybe [Fixity] -> P a'
parseWithSimplify _witness simpl mfixs = parser mfixs >>= return . simpl

a :: a
a = undefined

instance Parseable (S.ModuleName, Maybe S.WarningText, Maybe [S.ExportSpec]) where
    parser = parseWithSimplify (a :: ModuleHead SrcSpanInfo) (sModuleHead . Just)

instance Parseable S.Activation             where parser = parseWithSimplify (a :: Activation     SrcSpanInfo) sActivation
instance Parseable S.Alt                    where parser = parseWithSimplify (a :: Alt            SrcSpanInfo) sAlt
instance Parseable S.Annotation             where parser = parseWithSimplify (a :: Annotation     SrcSpanInfo) sAnnotation
instance Parseable S.BangType               where parser = parseWithSimplify (a :: BangType       SrcSpanInfo) sBangType
instance Parseable S.Binds                  where parser = parseWithSimplify (a :: Binds          SrcSpanInfo) sBinds
instance Parseable S.CallConv               where parser = parseWithSimplify (a :: CallConv       SrcSpanInfo) sCallConv
instance Parseable S.ClassDecl              where parser = parseWithSimplify (a :: ClassDecl      SrcSpanInfo) sClassDecl
instance Parseable S.CName                  where parser = parseWithSimplify (a :: CName          SrcSpanInfo) sCName
instance Parseable S.ConDecl                where parser = parseWithSimplify (a :: ConDecl        SrcSpanInfo) sConDecl
instance Parseable S.Decl                   where parser = parseWithSimplify (a :: Decl           SrcSpanInfo) sDecl
instance Parseable S.Exp                    where parser = parseWithSimplify (a :: Exp            SrcSpanInfo) sExp
instance Parseable S.ExportSpec             where parser = parseWithSimplify (a :: ExportSpec     SrcSpanInfo) sExportSpec
instance Parseable [S.ExportSpec]           where parser = parseWithSimplify (a :: ExportSpecList SrcSpanInfo) sExportSpecList
instance Parseable S.FunDep                 where parser = parseWithSimplify (a :: FunDep         SrcSpanInfo) sFunDep
instance Parseable S.GadtDecl               where parser = parseWithSimplify (a :: GadtDecl       SrcSpanInfo) sGadtDecl
instance Parseable S.GuardedAlt             where parser = parseWithSimplify (a :: GuardedAlt     SrcSpanInfo) sGuardedAlt
instance Parseable S.GuardedAlts            where parser = parseWithSimplify (a :: GuardedAlts    SrcSpanInfo) sGuardedAlts
instance Parseable S.GuardedRhs             where parser = parseWithSimplify (a :: GuardedRhs     SrcSpanInfo) sGuardedRhs
instance Parseable S.ImportDecl             where parser = parseWithSimplify (a :: ImportDecl     SrcSpanInfo) sImportDecl
instance Parseable S.ImportSpec             where parser = parseWithSimplify (a :: ImportSpec     SrcSpanInfo) sImportSpec
instance Parseable S.InstDecl               where parser = parseWithSimplify (a :: InstDecl       SrcSpanInfo) sInstDecl
instance Parseable S.IPBind                 where parser = parseWithSimplify (a :: IPBind         SrcSpanInfo) sIPBind
instance Parseable S.IPName                 where parser = parseWithSimplify (a :: IPName         SrcSpanInfo) sIPName
instance Parseable S.Kind                   where parser = parseWithSimplify (a :: Kind           SrcSpanInfo) sKind
instance Parseable S.Literal                where parser = parseWithSimplify (a :: Literal        SrcSpanInfo) sLiteral
instance Parseable S.Module                 where parser = parseWithSimplify (a :: Module         SrcSpanInfo) sModule
instance Parseable S.ModuleName             where parser = parseWithSimplify (a :: ModuleName     SrcSpanInfo) sModuleName
instance Parseable S.ModulePragma           where parser = parseWithSimplify (a :: ModulePragma   SrcSpanInfo) sModulePragma
instance Parseable S.Name                   where parser = parseWithSimplify (a :: Name           SrcSpanInfo) sName
instance Parseable S.Op                     where parser = parseWithSimplify (a :: Op             SrcSpanInfo) sOp
instance Parseable S.Pat                    where parser = parseWithSimplify (a :: Pat            SrcSpanInfo) sPat
instance Parseable S.QName                  where parser = parseWithSimplify (a :: QName          SrcSpanInfo) sQName
instance Parseable S.QOp                    where parser = parseWithSimplify (a :: QOp            SrcSpanInfo) sQOp
instance Parseable S.QualConDecl            where parser = parseWithSimplify (a :: QualConDecl    SrcSpanInfo) sQualConDecl
instance Parseable S.QualStmt               where parser = parseWithSimplify (a :: QualStmt       SrcSpanInfo) sQualStmt
instance Parseable S.Rhs                    where parser = parseWithSimplify (a :: Rhs            SrcSpanInfo) sRhs
instance Parseable S.Rule                   where parser = parseWithSimplify (a :: Rule           SrcSpanInfo) sRule
instance Parseable S.RuleVar                where parser = parseWithSimplify (a :: RuleVar        SrcSpanInfo) sRuleVar
instance Parseable S.Safety                 where parser = parseWithSimplify (a :: Safety         SrcSpanInfo) sSafety
instance Parseable S.Stmt                   where parser = parseWithSimplify (a :: Stmt           SrcSpanInfo) sStmt
instance Parseable S.Type                   where parser = parseWithSimplify (a :: Type           SrcSpanInfo) sType
instance Parseable S.TyVarBind              where parser = parseWithSimplify (a :: TyVarBind      SrcSpanInfo) sTyVarBind
instance Parseable S.XName                  where parser = parseWithSimplify (a :: XName          SrcSpanInfo) sXName

instance Parseable ([S.Name], S.BangType)   where parser = parseWithSimplify (a :: FieldDecl      SrcSpanInfo) sFieldDecl
instance Parseable (Bool, [S.ImportSpec])   where parser = parseWithSimplify (a :: ImportSpecList SrcSpanInfo) sImportSpecList

instance Parseable [S.Module]               where parser = parseWithSimplify (a :: [Module        SrcSpanInfo]) (map sModule)
instance Parseable [S.Rule]                 where parser = parseWithSimplify (a :: [Rule          SrcSpanInfo]) (map sRule)
instance Parseable [S.RuleVar]              where parser = parseWithSimplify (a :: [RuleVar       SrcSpanInfo]) (map sRuleVar)

instance Parseable [([S.Name], S.BangType)] where parser = parseWithSimplify (a :: ([FieldDecl    SrcSpanInfo], SrcSpanInfo)) (map sFieldDecl      . fst)
instance Parseable [S.ClassDecl]            where parser = parseWithSimplify (a :: ([ClassDecl    SrcSpanInfo], SrcSpanInfo)) (map sClassDecl      . fst)
instance Parseable [S.CName]                where parser = parseWithSimplify (a :: ([CName        SrcSpanInfo], SrcSpanInfo)) (map sCName          . fst)
instance Parseable [S.Decl]                 where parser = parseWithSimplify (a :: ([Decl         SrcSpanInfo], SrcSpanInfo)) (map sDecl           . fst)
instance Parseable [S.FunDep]               where parser = parseWithSimplify (a :: ([FunDep       SrcSpanInfo], SrcSpanInfo)) (map sFunDep         . fst)
instance Parseable [S.GadtDecl]             where parser = parseWithSimplify (a :: ([GadtDecl     SrcSpanInfo], SrcSpanInfo)) (map sGadtDecl       . fst)
instance Parseable [S.GuardedAlt]           where parser = parseWithSimplify (a :: ([GuardedAlt   SrcSpanInfo], SrcSpanInfo)) (map sGuardedAlt     . fst)
instance Parseable [S.GuardedRhs]           where parser = parseWithSimplify (a :: ([GuardedRhs   SrcSpanInfo], SrcSpanInfo)) (map sGuardedRhs     . fst)
instance Parseable [S.ImportDecl]           where parser = parseWithSimplify (a :: ([ImportDecl   SrcSpanInfo], SrcSpanInfo)) (map sImportDecl     . fst)
instance Parseable [S.InstDecl]             where parser = parseWithSimplify (a :: ([InstDecl     SrcSpanInfo], SrcSpanInfo)) (map sInstDecl       . fst)
instance Parseable [S.IPBind]               where parser = parseWithSimplify (a :: ([IPBind       SrcSpanInfo], SrcSpanInfo)) (map sIPBind         . fst)
instance Parseable [S.ModulePragma]         where parser = parseWithSimplify (a :: ([ModulePragma SrcSpanInfo], SrcSpanInfo)) (map sModulePragma   . fst)
instance Parseable [S.Name]                 where parser = parseWithSimplify (a :: ([Name         SrcSpanInfo], SrcSpanInfo)) (map sName           . fst)
instance Parseable [S.Op]                   where parser = parseWithSimplify (a :: ([Op           SrcSpanInfo], SrcSpanInfo)) (map sOp             . fst)
instance Parseable [S.QName]                where parser = parseWithSimplify (a :: ([QName        SrcSpanInfo], SrcSpanInfo)) (map sQName          . fst)
instance Parseable [S.QualConDecl]          where parser = parseWithSimplify (a :: ([QualConDecl  SrcSpanInfo], SrcSpanInfo)) (map sQualConDecl    . fst)
instance Parseable [S.QualStmt]             where parser = parseWithSimplify (a :: ([QualStmt     SrcSpanInfo], SrcSpanInfo)) (map sQualStmt       . fst)
instance Parseable [S.Stmt]                 where parser = parseWithSimplify (a :: ([Stmt         SrcSpanInfo], SrcSpanInfo)) (map sStmt           . fst)
instance Parseable [S.TyVarBind]            where parser = parseWithSimplify (a :: ([TyVarBind    SrcSpanInfo], SrcSpanInfo)) (map sTyVarBind      . fst)
instance Parseable [S.Exp]                  where parser = parseWithSimplify (a :: ([Exp          SrcSpanInfo], SrcSpanInfo)) (map sExp            . fst)
instance Parseable [S.Pat]                  where parser = parseWithSimplify (a :: ([Pat          SrcSpanInfo], SrcSpanInfo)) (map sPat            . fst)

instance Parseable [[S.QualStmt]]           where parser = parseWithSimplify (a :: ([[QualStmt    SrcSpanInfo]],SrcSpanInfo)) (map (map sQualStmt) . fst)

{-
instance Parseable (S.Name, [S.TyVarBind])  where parser = parseWithSimplify
instance Parseable (S.QName, [S.Type])      where parser = parseWithSimplify
instance Parseable [(S.QName, [S.Type])]    where parser = parseWithSimplify
instance Parseable S.Assoc                  where parser = parseWithSimplify
instance Parseable S.Asst                   where parser = parseWithSimplify
instance Parseable S.Bracket                where parser = parseWithSimplify
instance Parseable S.Context                where parser = parseWithSimplify
instance Parseable S.FieldUpdate            where parser = parseWithSimplify
instance Parseable S.Match                  where parser = parseWithSimplify
instance Parseable S.PatField               where parser = parseWithSimplify
instance Parseable S.PXAttr                 where parser = parseWithSimplify
instance Parseable S.RPat                   where parser = parseWithSimplify
instance Parseable S.RPatOp                 where parser = parseWithSimplify
instance Parseable S.SpecialCon             where parser = parseWithSimplify
instance Parseable S.Splice                 where parser = parseWithSimplify
instance Parseable S.WarningText            where parser = parseWithSimplify
instance Parseable S.XAttr                  where parser = parseWithSimplify
-}

-- Type-specific instances

-- | Parse of a string, which should contain a complete Haskell module, using 'defaultParseMode'.
parseModule :: String -> ParseResult S.Module
parseModule = parse

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode'.
parseModuleWithMode :: ParseMode -> String -> ParseResult S.Module
parseModuleWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseModuleWithComments :: ParseMode -> String -> ParseResult (S.Module, [Comment])
parseModuleWithComments = parseWithComments

-- | Parse of a string, which should contain complete Haskell modules, using 'defaultParseMode'.
parseModules :: String -> ParseResult [S.Module]
parseModules = parse

-- | Parse of a string containing complete Haskell modules, using an explicit 'ParseMode'.
parseModulesWithMode :: ParseMode -> String -> ParseResult [S.Module]
parseModulesWithMode = parseWithMode

-- | Parse of a string containing complete Haskell modules, using an explicit 'ParseMode', retaining comments.
parseModulesWithComments :: ParseMode -> String -> ParseResult ([S.Module], [Comment])
parseModulesWithComments = parseWithComments

-- | Parse of a string containing a Haskell expression, using 'defaultParseMode'.
parseExp :: String -> ParseResult S.Exp
parseExp = parse

-- | Parse of a string containing a Haskell expression, using an explicit 'ParseMode'.
parseExpWithMode :: ParseMode -> String -> ParseResult S.Exp
parseExpWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseExpWithComments :: ParseMode -> String -> ParseResult (S.Exp, [Comment])
parseExpWithComments = parseWithComments

-- | Parse of a string containing a Haskell pattern, using 'defaultParseMode'.
parsePat :: String -> ParseResult S.Pat
parsePat = parse

-- | Parse of a string containing a Haskell pattern, using an explicit 'ParseMode'.
parsePatWithMode :: ParseMode -> String -> ParseResult S.Pat
parsePatWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parsePatWithComments :: ParseMode -> String -> ParseResult (S.Pat, [Comment])
parsePatWithComments = parseWithComments

-- | Parse of a string containing a Haskell top-level declaration, using 'defaultParseMode'
parseDecl :: String -> ParseResult S.Decl
parseDecl = parse

-- | Parse of a string containing a Haskell top-level declaration, using an explicit 'ParseMode'.
parseDeclWithMode :: ParseMode -> String -> ParseResult S.Decl
parseDeclWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseDeclWithComments :: ParseMode -> String -> ParseResult (S.Decl, [Comment])
parseDeclWithComments = parseWithComments

-- | Parse of a string containing a Haskell type, using 'defaultParseMode'.
parseType :: String -> ParseResult S.Type
parseType = parse

-- | Parse of a string containing a Haskell type, using an explicit 'ParseMode'.
parseTypeWithMode :: ParseMode -> String -> ParseResult  S.Type
parseTypeWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseTypeWithComments :: ParseMode -> String -> ParseResult (S.Type, [Comment])
parseTypeWithComments = parseWithComments

-- | Parse of a string containing a Haskell type, using 'defaultParseMode'.
parseStmt :: String -> ParseResult S.Stmt
parseStmt = parse

-- | Parse of a string containing a Haskell type, using an explicit 'ParseMode'.
parseStmtWithMode :: ParseMode -> String -> ParseResult S.Stmt
parseStmtWithMode = parseWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit 'ParseMode', retaining comments.
parseStmtWithComments :: ParseMode -> String -> ParseResult (S.Stmt, [Comment])
parseStmtWithComments = parseWithComments

-- Module head parsers

-- | Partial parse of a string starting with a series of top-level option pragmas.
getTopPragmas :: String -> ParseResult [S.ModulePragma]
getTopPragmas = fmap (\(NonGreedyTopPragmas ps) -> ps) . parse

data NonGreedyTopPragmas = NonGreedyTopPragmas [S.ModulePragma]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable NonGreedyTopPragmas where
    parser fixs = do
        A.NonGreedyTopPragmas ps _ <- parser fixs
        return $ NonGreedyTopPragmas
            (map sModulePragma (ps :: [A.ModulePragma SrcSpanInfo]))

--   Type intended to be used with 'Parseable', with instances that implement a
--   non-greedy parse of the module name, including top-level pragmas.  This
--   means that a parse error that comes after the module header won't be
--   returned. If no module name is found (and no parse error occurs), then
--   \"Main\" is returned.  This is the same behavior that 'parseModule' has.
data NonGreedyModuleName = NonGreedyModuleName
    [S.ModulePragma]
    S.ModuleName
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable NonGreedyModuleName where
    parser fixs = do
        A.NonGreedyModuleName (ps, _) mmn <- parser fixs
        return $ NonGreedyModuleName
            (map sModulePragma (ps :: [A.ModulePragma SrcSpanInfo]))
            (maybe S.main_mod sModuleName mmn)

--   Type intended to be used with 'Parseable', with instances that implement a
--   non-greedy parse of the module name, including top-level pragmas.  This
--   means that a parse error that comes after the module header won't be
--   returned. If no module head is found, then a default simple head like
--   \"module Main where\" is assumed. This is the same behavior that
--   'parseModule' has.
--
--   Note that the 'ParseMode' particularly matters for this due to the
--   'MagicHash' changing the lexing of identifiers to include \"#\".
data NonGreedyModuleHead = NonGreedyModuleHead
    [S.ModulePragma]
    (S.ModuleName, Maybe S.WarningText, Maybe [S.ExportSpec])
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable NonGreedyModuleHead where
    parser fixs = do
        A.NonGreedyModuleHead (ps, _) mmh <- parser fixs
        return $ NonGreedyModuleHead
            (map sModulePragma (ps :: [A.ModulePragma SrcSpanInfo]))
            (sModuleHead mmh)

--   Type intended to be used with 'Parseable', with instances that implement a
--   non-greedy parse of the module head, including top-level pragmas, module
--   name, export list, and import list. This means that if a parse error that
--   comes after the imports won't be returned.  If no module head is found,
--   then a default simple head like \"module Main where\" is assumed. This is
--   the same behavior that 'parseModule' has.
--
--   Note that the 'ParseMode' particularly matters for this due to the
--   'MagicHash' changing the lexing of identifiers to include \"#\".
data NonGreedyModuleImports = NonGreedyModuleImports
    [S.ModulePragma]
    (S.ModuleName, Maybe S.WarningText, Maybe [S.ExportSpec])
    [S.ImportDecl]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable NonGreedyModuleImports where
    parser fixs = do
        A.NonGreedyModuleImports (ps, _) mmh mimps <- parser fixs
        return $ NonGreedyModuleImports
            (map sModulePragma (ps :: [A.ModulePragma SrcSpanInfo]))
            (sModuleHead mmh)
            (fromMaybe [] $ fmap (map sImportDecl . fst) mimps)

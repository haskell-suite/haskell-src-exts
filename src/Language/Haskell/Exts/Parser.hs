
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
-- Along with exporting utilities commonly used with parsers, this module also
-- provides 'Parseable' instances for the simplified AST (all of the instances
-- that don't mention 'SrcSpanInfo').  While most of these should be self
-- explanatory, the instances for lists are a little tricky. See the
-- documentation for 'Language.Haskell.Exts.Annotated.Parser.ListOf' for an
-- explanation of these.
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
                PragmasAndModuleName(..), PragmasAndModuleHead(..), ModuleHeadAndImports(..),
                NonGreedy(..),
                -- * CPP Utilities
                ignoreCpp, ignoreCppLines
            ) where

import Data.Maybe (fromMaybe)
import Language.Haskell.Exts.Annotated.Fixity
import Language.Haskell.Exts.Annotated.Parser (readExtensions, unListOf, ListOf, NonGreedy(..), ignoreCpp, ignoreCppLines)
import Language.Haskell.Exts.Annotated.Simplify
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.ParseMonad hiding (getModuleName)
import Language.Haskell.Exts.SrcLoc

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
instance Parseable ([S.Name], S.BangType)   where parser = parseWithSimplify (a :: FieldDecl      SrcSpanInfo) sFieldDecl
instance Parseable S.FunDep                 where parser = parseWithSimplify (a :: FunDep         SrcSpanInfo) sFunDep
instance Parseable S.GadtDecl               where parser = parseWithSimplify (a :: GadtDecl       SrcSpanInfo) sGadtDecl
instance Parseable S.GuardedAlt             where parser = parseWithSimplify (a :: GuardedAlt     SrcSpanInfo) sGuardedAlt
instance Parseable S.GuardedAlts            where parser = parseWithSimplify (a :: GuardedAlts    SrcSpanInfo) sGuardedAlts
instance Parseable S.GuardedRhs             where parser = parseWithSimplify (a :: GuardedRhs     SrcSpanInfo) sGuardedRhs
instance Parseable S.ImportDecl             where parser = parseWithSimplify (a :: ImportDecl     SrcSpanInfo) sImportDecl
instance Parseable S.ImportSpec             where parser = parseWithSimplify (a :: ImportSpec     SrcSpanInfo) sImportSpec
instance Parseable (Bool, [S.ImportSpec])   where parser = parseWithSimplify (a :: ImportSpecList SrcSpanInfo) sImportSpecList
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

instance Parseable [S.ClassDecl]            where parser = parseWithSimplify (a :: ListOf (ClassDecl    SrcSpanInfo)) (map sClassDecl      . unListOf)
instance Parseable [S.CName]                where parser = parseWithSimplify (a :: ListOf (CName        SrcSpanInfo)) (map sCName          . unListOf)
instance Parseable [S.Decl]                 where parser = parseWithSimplify (a :: ListOf (Decl         SrcSpanInfo)) (map sDecl           . unListOf)
instance Parseable [S.Exp]                  where parser = parseWithSimplify (a :: ListOf (Exp          SrcSpanInfo)) (map sExp            . unListOf)
instance Parseable [([S.Name], S.BangType)] where parser = parseWithSimplify (a :: ListOf (FieldDecl    SrcSpanInfo)) (map sFieldDecl      . unListOf)
instance Parseable [S.FunDep]               where parser = parseWithSimplify (a :: ListOf (FunDep       SrcSpanInfo)) (map sFunDep         . unListOf)
instance Parseable [S.GadtDecl]             where parser = parseWithSimplify (a :: ListOf (GadtDecl     SrcSpanInfo)) (map sGadtDecl       . unListOf)
instance Parseable [S.GuardedAlt]           where parser = parseWithSimplify (a :: ListOf (GuardedAlt   SrcSpanInfo)) (map sGuardedAlt     . unListOf)
instance Parseable [S.GuardedRhs]           where parser = parseWithSimplify (a :: ListOf (GuardedRhs   SrcSpanInfo)) (map sGuardedRhs     . unListOf)
instance Parseable [S.ImportDecl]           where parser = parseWithSimplify (a :: ListOf (ImportDecl   SrcSpanInfo)) (map sImportDecl     . unListOf)
instance Parseable [S.InstDecl]             where parser = parseWithSimplify (a :: ListOf (InstDecl     SrcSpanInfo)) (map sInstDecl       . unListOf)
instance Parseable [S.IPBind]               where parser = parseWithSimplify (a :: ListOf (IPBind       SrcSpanInfo)) (map sIPBind         . unListOf)
instance Parseable [S.Module]               where parser = parseWithSimplify (a :: ListOf (Module       SrcSpanInfo)) (map sModule         . unListOf)
instance Parseable [S.ModulePragma]         where parser = parseWithSimplify (a :: ListOf (ModulePragma SrcSpanInfo)) (map sModulePragma   . unListOf)
instance Parseable [S.Name]                 where parser = parseWithSimplify (a :: ListOf (Name         SrcSpanInfo)) (map sName           . unListOf)
instance Parseable [S.Op]                   where parser = parseWithSimplify (a :: ListOf (Op           SrcSpanInfo)) (map sOp             . unListOf)
instance Parseable [S.Pat]                  where parser = parseWithSimplify (a :: ListOf (Pat          SrcSpanInfo)) (map sPat            . unListOf)
instance Parseable [S.QName]                where parser = parseWithSimplify (a :: ListOf (QName        SrcSpanInfo)) (map sQName          . unListOf)
instance Parseable [S.QualConDecl]          where parser = parseWithSimplify (a :: ListOf (QualConDecl  SrcSpanInfo)) (map sQualConDecl    . unListOf)
instance Parseable [S.QualStmt]             where parser = parseWithSimplify (a :: ListOf (QualStmt     SrcSpanInfo)) (map sQualStmt       . unListOf)
instance Parseable [S.Rule]                 where parser = parseWithSimplify (a :: ListOf (Rule         SrcSpanInfo)) (map sRule           . unListOf)
instance Parseable [S.RuleVar]              where parser = parseWithSimplify (a :: ListOf (RuleVar      SrcSpanInfo)) (map sRuleVar        . unListOf)
instance Parseable [S.Stmt]                 where parser = parseWithSimplify (a :: ListOf (Stmt         SrcSpanInfo)) (map sStmt           . unListOf)
instance Parseable [S.TyVarBind]            where parser = parseWithSimplify (a :: ListOf (TyVarBind    SrcSpanInfo)) (map sTyVarBind      . unListOf)

instance Parseable [[S.QualStmt]]           where parser = parseWithSimplify (a :: ListOf [QualStmt     SrcSpanInfo]) (map (map sQualStmt) . unListOf)

instance Parseable S.FieldUpdate            where parser = parseWithSimplify (a :: FieldUpdate SrcSpanInfo) sFieldUpdate
instance Parseable S.PatField               where parser = parseWithSimplify (a :: PatField    SrcSpanInfo) sPatField
instance Parseable S.XAttr                  where parser = parseWithSimplify (a :: XAttr       SrcSpanInfo) sXAttr
instance Parseable S.PXAttr                 where parser = parseWithSimplify (a :: PXAttr      SrcSpanInfo) sPXAttr
instance Parseable S.RPat                   where parser = parseWithSimplify (a :: RPat        SrcSpanInfo) sRPat
instance Parseable S.RPatOp                 where parser = parseWithSimplify (a :: RPatOp      SrcSpanInfo) sRPatOp
instance Parseable S.Bracket                where parser = parseWithSimplify (a :: Bracket     SrcSpanInfo) sBracket
instance Parseable S.Splice                 where parser = parseWithSimplify (a :: Splice      SrcSpanInfo) sSplice
instance Parseable S.Context                where parser = parseWithSimplify (a :: Context     SrcSpanInfo) sContext

instance Parseable [S.FieldUpdate]          where parser = parseWithSimplify (a :: ListOf (FieldUpdate SrcSpanInfo)) (map sFieldUpdate . unListOf)
instance Parseable [S.PatField]             where parser = parseWithSimplify (a :: ListOf (PatField    SrcSpanInfo)) (map sPatField    . unListOf)
instance Parseable [S.XAttr]                where parser = parseWithSimplify (a :: ListOf (XAttr       SrcSpanInfo)) (map sXAttr       . unListOf)
instance Parseable [S.PXAttr]               where parser = parseWithSimplify (a :: ListOf (PXAttr      SrcSpanInfo)) (map sPXAttr      . unListOf)
instance Parseable [S.RPat]                 where parser = parseWithSimplify (a :: ListOf (RPat        SrcSpanInfo)) (map sRPat        . unListOf)

-- Non greedy parsers

instance Parseable (NonGreedy [S.ModulePragma]        ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (ModulePragma SrcSpanInfo))) (fmap (map sModulePragma . unListOf))

{-
instance Parseable (NonGreedy S.Activation          ) where parser = parseWithSimplify (a :: NonGreedy (Activation     SrcSpanInfo)) (fmap sActivation)
instance Parseable (NonGreedy S.Alt                 ) where parser = parseWithSimplify (a :: NonGreedy (Alt            SrcSpanInfo)) (fmap sAlt)
instance Parseable (NonGreedy S.Annotation          ) where parser = parseWithSimplify (a :: NonGreedy (Annotation     SrcSpanInfo)) (fmap sAnnotation)
instance Parseable (NonGreedy S.BangType            ) where parser = parseWithSimplify (a :: NonGreedy (BangType       SrcSpanInfo)) (fmap sBangType)
instance Parseable (NonGreedy S.Binds               ) where parser = parseWithSimplify (a :: NonGreedy (Binds          SrcSpanInfo)) (fmap sBinds)
instance Parseable (NonGreedy S.CallConv            ) where parser = parseWithSimplify (a :: NonGreedy (CallConv       SrcSpanInfo)) (fmap sCallConv)
instance Parseable (NonGreedy S.ClassDecl           ) where parser = parseWithSimplify (a :: NonGreedy (ClassDecl      SrcSpanInfo)) (fmap sClassDecl)
instance Parseable (NonGreedy S.CName               ) where parser = parseWithSimplify (a :: NonGreedy (CName          SrcSpanInfo)) (fmap sCName)
instance Parseable (NonGreedy S.ConDecl             ) where parser = parseWithSimplify (a :: NonGreedy (ConDecl        SrcSpanInfo)) (fmap sConDecl)
instance Parseable (NonGreedy S.Decl                ) where parser = parseWithSimplify (a :: NonGreedy (Decl           SrcSpanInfo)) (fmap sDecl)
instance Parseable (NonGreedy S.Exp                 ) where parser = parseWithSimplify (a :: NonGreedy (Exp            SrcSpanInfo)) (fmap sExp)
instance Parseable (NonGreedy S.ExportSpec          ) where parser = parseWithSimplify (a :: NonGreedy (ExportSpec     SrcSpanInfo)) (fmap sExportSpec)
instance Parseable (NonGreedy [S.ExportSpec]        ) where parser = parseWithSimplify (a :: NonGreedy (ExportSpecList SrcSpanInfo)) (fmap sExportSpecList)
instance Parseable (NonGreedy ([S.Name], S.BangType)) where parser = parseWithSimplify (a :: NonGreedy (FieldDecl      SrcSpanInfo)) (fmap sFieldDecl)
instance Parseable (NonGreedy S.FunDep              ) where parser = parseWithSimplify (a :: NonGreedy (FunDep         SrcSpanInfo)) (fmap sFunDep)
instance Parseable (NonGreedy S.GadtDecl            ) where parser = parseWithSimplify (a :: NonGreedy (GadtDecl       SrcSpanInfo)) (fmap sGadtDecl)
instance Parseable (NonGreedy S.GuardedAlt          ) where parser = parseWithSimplify (a :: NonGreedy (GuardedAlt     SrcSpanInfo)) (fmap sGuardedAlt)
instance Parseable (NonGreedy S.GuardedAlts         ) where parser = parseWithSimplify (a :: NonGreedy (GuardedAlts    SrcSpanInfo)) (fmap sGuardedAlts)
instance Parseable (NonGreedy S.GuardedRhs          ) where parser = parseWithSimplify (a :: NonGreedy (GuardedRhs     SrcSpanInfo)) (fmap sGuardedRhs)
instance Parseable (NonGreedy S.ImportDecl          ) where parser = parseWithSimplify (a :: NonGreedy (ImportDecl     SrcSpanInfo)) (fmap sImportDecl)
instance Parseable (NonGreedy S.ImportSpec          ) where parser = parseWithSimplify (a :: NonGreedy (ImportSpec     SrcSpanInfo)) (fmap sImportSpec)
instance Parseable (NonGreedy (Bool, [S.ImportSpec])) where parser = parseWithSimplify (a :: NonGreedy (ImportSpecList SrcSpanInfo)) (fmap sImportSpecList)
instance Parseable (NonGreedy S.InstDecl            ) where parser = parseWithSimplify (a :: NonGreedy (InstDecl       SrcSpanInfo)) (fmap sInstDecl)
instance Parseable (NonGreedy S.IPBind              ) where parser = parseWithSimplify (a :: NonGreedy (IPBind         SrcSpanInfo)) (fmap sIPBind)
instance Parseable (NonGreedy S.IPName              ) where parser = parseWithSimplify (a :: NonGreedy (IPName         SrcSpanInfo)) (fmap sIPName)
instance Parseable (NonGreedy S.Kind                ) where parser = parseWithSimplify (a :: NonGreedy (Kind           SrcSpanInfo)) (fmap sKind)
instance Parseable (NonGreedy S.Literal             ) where parser = parseWithSimplify (a :: NonGreedy (Literal        SrcSpanInfo)) (fmap sLiteral)
instance Parseable (NonGreedy S.Module              ) where parser = parseWithSimplify (a :: NonGreedy (Module         SrcSpanInfo)) (fmap sModule)
instance Parseable (NonGreedy S.ModuleName          ) where parser = parseWithSimplify (a :: NonGreedy (ModuleName     SrcSpanInfo)) (fmap sModuleName)
instance Parseable (NonGreedy S.ModulePragma        ) where parser = parseWithSimplify (a :: NonGreedy (ModulePragma   SrcSpanInfo)) (fmap sModulePragma)
instance Parseable (NonGreedy S.Name                ) where parser = parseWithSimplify (a :: NonGreedy (Name           SrcSpanInfo)) (fmap sName)
instance Parseable (NonGreedy S.Op                  ) where parser = parseWithSimplify (a :: NonGreedy (Op             SrcSpanInfo)) (fmap sOp)
instance Parseable (NonGreedy S.Pat                 ) where parser = parseWithSimplify (a :: NonGreedy (Pat            SrcSpanInfo)) (fmap sPat)
instance Parseable (NonGreedy S.QName               ) where parser = parseWithSimplify (a :: NonGreedy (QName          SrcSpanInfo)) (fmap sQName)
instance Parseable (NonGreedy S.QOp                 ) where parser = parseWithSimplify (a :: NonGreedy (QOp            SrcSpanInfo)) (fmap sQOp)
instance Parseable (NonGreedy S.QualConDecl         ) where parser = parseWithSimplify (a :: NonGreedy (QualConDecl    SrcSpanInfo)) (fmap sQualConDecl)
instance Parseable (NonGreedy S.QualStmt            ) where parser = parseWithSimplify (a :: NonGreedy (QualStmt       SrcSpanInfo)) (fmap sQualStmt)
instance Parseable (NonGreedy S.Rhs                 ) where parser = parseWithSimplify (a :: NonGreedy (Rhs            SrcSpanInfo)) (fmap sRhs)
instance Parseable (NonGreedy S.Rule                ) where parser = parseWithSimplify (a :: NonGreedy (Rule           SrcSpanInfo)) (fmap sRule)
instance Parseable (NonGreedy S.RuleVar             ) where parser = parseWithSimplify (a :: NonGreedy (RuleVar        SrcSpanInfo)) (fmap sRuleVar)
instance Parseable (NonGreedy S.Safety              ) where parser = parseWithSimplify (a :: NonGreedy (Safety         SrcSpanInfo)) (fmap sSafety)
instance Parseable (NonGreedy S.Stmt                ) where parser = parseWithSimplify (a :: NonGreedy (Stmt           SrcSpanInfo)) (fmap sStmt)
instance Parseable (NonGreedy S.Type                ) where parser = parseWithSimplify (a :: NonGreedy (Type           SrcSpanInfo)) (fmap sType)
instance Parseable (NonGreedy S.TyVarBind           ) where parser = parseWithSimplify (a :: NonGreedy (TyVarBind      SrcSpanInfo)) (fmap sTyVarBind)
instance Parseable (NonGreedy S.XName               ) where parser = parseWithSimplify (a :: NonGreedy (XName          SrcSpanInfo)) (fmap sXName)

instance Parseable (NonGreedy [S.ClassDecl]           ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (ClassDecl    SrcSpanInfo))) (fmap (map sClassDecl    . unListOf))
instance Parseable (NonGreedy [S.CName]               ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (CName        SrcSpanInfo))) (fmap (map sCName        . unListOf))
instance Parseable (NonGreedy [S.Decl]                ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (Decl         SrcSpanInfo))) (fmap (map sDecl         . unListOf))
instance Parseable (NonGreedy [S.Exp]                 ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (Exp          SrcSpanInfo))) (fmap (map sExp          . unListOf))
instance Parseable (NonGreedy [([S.Name], S.BangType)]) where parser = parseWithSimplify (a :: NonGreedy (ListOf (FieldDecl    SrcSpanInfo))) (fmap (map sFieldDecl    . unListOf))
instance Parseable (NonGreedy [S.FunDep]              ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (FunDep       SrcSpanInfo))) (fmap (map sFunDep       . unListOf))
instance Parseable (NonGreedy [S.GadtDecl]            ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (GadtDecl     SrcSpanInfo))) (fmap (map sGadtDecl     . unListOf))
instance Parseable (NonGreedy [S.GuardedAlt]          ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (GuardedAlt   SrcSpanInfo))) (fmap (map sGuardedAlt   . unListOf))
instance Parseable (NonGreedy [S.GuardedRhs]          ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (GuardedRhs   SrcSpanInfo))) (fmap (map sGuardedRhs   . unListOf))
instance Parseable (NonGreedy [S.ImportDecl]          ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (ImportDecl   SrcSpanInfo))) (fmap (map sImportDecl   . unListOf))
instance Parseable (NonGreedy [S.InstDecl]            ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (InstDecl     SrcSpanInfo))) (fmap (map sInstDecl     . unListOf))
instance Parseable (NonGreedy [S.IPBind]              ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (IPBind       SrcSpanInfo))) (fmap (map sIPBind       . unListOf))
instance Parseable (NonGreedy [S.Name]                ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (Name         SrcSpanInfo))) (fmap (map sName         . unListOf))
instance Parseable (NonGreedy [S.Op]                  ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (Op           SrcSpanInfo))) (fmap (map sOp           . unListOf))
instance Parseable (NonGreedy [S.Pat]                 ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (Pat          SrcSpanInfo))) (fmap (map sPat          . unListOf))
instance Parseable (NonGreedy [S.QName]               ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (QName        SrcSpanInfo))) (fmap (map sQName        . unListOf))
instance Parseable (NonGreedy [S.QualConDecl]         ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (QualConDecl  SrcSpanInfo))) (fmap (map sQualConDecl  . unListOf))
instance Parseable (NonGreedy [S.QualStmt]            ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (QualStmt     SrcSpanInfo))) (fmap (map sQualStmt     . unListOf))
instance Parseable (NonGreedy [S.Rule]                ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (Rule         SrcSpanInfo))) (fmap (map sRule         . unListOf))
instance Parseable (NonGreedy [S.RuleVar]             ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (RuleVar      SrcSpanInfo))) (fmap (map sRuleVar      . unListOf))
instance Parseable (NonGreedy [S.Stmt]                ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (Stmt         SrcSpanInfo))) (fmap (map sStmt         . unListOf))
instance Parseable (NonGreedy [S.TyVarBind]           ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (TyVarBind    SrcSpanInfo))) (fmap (map sTyVarBind    . unListOf))

instance Parseable (NonGreedy [[S.QualStmt]]          ) where parser = parseWithSimplify (a :: NonGreedy (ListOf [QualStmt     SrcSpanInfo])) (fmap (map (map sQualStmt) . unListOf))

instance Parseable (NonGreedy S.FieldUpdate           ) where parser = parseWithSimplify (a :: NonGreedy (FieldUpdate SrcSpanInfo)) (fmap sFieldUpdate)
instance Parseable (NonGreedy S.PatField              ) where parser = parseWithSimplify (a :: NonGreedy (PatField    SrcSpanInfo)) (fmap sPatField)
instance Parseable (NonGreedy S.XAttr                 ) where parser = parseWithSimplify (a :: NonGreedy (XAttr       SrcSpanInfo)) (fmap sXAttr)
instance Parseable (NonGreedy S.PXAttr                ) where parser = parseWithSimplify (a :: NonGreedy (PXAttr      SrcSpanInfo)) (fmap sPXAttr)
instance Parseable (NonGreedy S.RPat                  ) where parser = parseWithSimplify (a :: NonGreedy (RPat        SrcSpanInfo)) (fmap sRPat)
instance Parseable (NonGreedy S.RPatOp                ) where parser = parseWithSimplify (a :: NonGreedy (RPatOp      SrcSpanInfo)) (fmap sRPatOp)
instance Parseable (NonGreedy S.Bracket               ) where parser = parseWithSimplify (a :: NonGreedy (Bracket     SrcSpanInfo)) (fmap sBracket)
instance Parseable (NonGreedy S.Splice                ) where parser = parseWithSimplify (a :: NonGreedy (Splice      SrcSpanInfo)) (fmap sSplice)
instance Parseable (NonGreedy S.Context               ) where parser = parseWithSimplify (a :: NonGreedy (Context     SrcSpanInfo)) (fmap sContext)

instance Parseable (NonGreedy [S.FieldUpdate]         ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (FieldUpdate SrcSpanInfo))) (fmap (map sFieldUpdate . unListOf))
instance Parseable (NonGreedy [S.PatField]            ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (PatField    SrcSpanInfo))) (fmap (map sPatField    . unListOf))
instance Parseable (NonGreedy [S.XAttr]               ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (XAttr       SrcSpanInfo))) (fmap (map sXAttr       . unListOf))
instance Parseable (NonGreedy [S.PXAttr]              ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (PXAttr      SrcSpanInfo))) (fmap (map sPXAttr      . unListOf))
instance Parseable (NonGreedy [S.RPat]                ) where parser = parseWithSimplify (a :: NonGreedy (ListOf (RPat        SrcSpanInfo))) (fmap (map sRPat        . unListOf))
-}

{-
instance Parseable (S.Name, [S.TyVarBind])  where parser = parseWithSimplify
instance Parseable (S.QName, [S.Type])      where parser = parseWithSimplify
instance Parseable [(S.QName, [S.Type])]    where parser = parseWithSimplify
instance Parseable S.Assoc                  where parser = parseWithSimplify
instance Parseable S.Asst                   where parser = parseWithSimplify
instance Parseable S.Match                  where parser = parseWithSimplify
instance Parseable S.SpecialCon             where parser = parseWithSimplify
instance Parseable S.WarningText            where parser = parseWithSimplify
instance Parseable [S.Asst]                 where parser = parseWithSimplify
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
getTopPragmas = fmap (fmap unNonGreedy) parse

-- | Type intended to be used with 'Parseable', with instances that implement a
--   non-greedy parse of the module name, including top-level pragmas.  This
--   means that a parse error that comes after the module header won't be
--   returned. If no module name is found (and no parse error occurs), then
--   \"Main\" is returned.  This is the same behavior that 'parseModule' has.
data PragmasAndModuleName = PragmasAndModuleName
    [S.ModulePragma]
    S.ModuleName
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable (NonGreedy PragmasAndModuleName) where
    parser fixs = do
        NonGreedy (A.PragmasAndModuleName (ps, _) mmn) <- parser fixs
        return $ NonGreedy $ PragmasAndModuleName
            (map sModulePragma (ps :: [A.ModulePragma SrcSpanInfo]))
            (maybe S.main_mod sModuleName mmn)

-- | Type intended to be used with 'Parseable', with instances that implement a
--   non-greedy parse of the module name, including top-level pragmas.  This
--   means that a parse error that comes after the module header won't be
--   returned. If no module head is found, then a default simple head like
--   \"module Main where\" is assumed. This is the same behavior that
--   'parseModule' has.
--
--   Note that the 'ParseMode' particularly matters for this due to the
--   'MagicHash' changing the lexing of identifiers to include \"#\".
data PragmasAndModuleHead = PragmasAndModuleHead
    [S.ModulePragma]
    (S.ModuleName, Maybe S.WarningText, Maybe [S.ExportSpec])
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable (NonGreedy PragmasAndModuleHead) where
    parser fixs = do
        NonGreedy (A.PragmasAndModuleHead (ps, _) mmh) <- parser fixs
        return $ NonGreedy $ PragmasAndModuleHead
            (map sModulePragma (ps :: [A.ModulePragma SrcSpanInfo]))
            (sModuleHead mmh)

-- | Type intended to be used with 'Parseable', with instances that implement a
--   non-greedy parse of the module head, including top-level pragmas, module
--   name, export list, and import list. This means that if a parse error that
--   comes after the imports won't be returned.  If no module head is found,
--   then a default simple head like \"module Main where\" is assumed. This is
--   the same behavior that 'parseModule' has.
--
--   Note that the 'ParseMode' particularly matters for this due to the
--   'MagicHash' changing the lexing of identifiers to include \"#\".
data ModuleHeadAndImports = ModuleHeadAndImports
    [S.ModulePragma]
    (S.ModuleName, Maybe S.WarningText, Maybe [S.ExportSpec])
    [S.ImportDecl]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

instance Parseable (NonGreedy ModuleHeadAndImports) where
    parser fixs = do
        NonGreedy (A.ModuleHeadAndImports (ps, _) mmh mimps) <- parser fixs
        return $ NonGreedy $ ModuleHeadAndImports
            (map sModulePragma (ps :: [A.ModulePragma SrcSpanInfo]))
            (sModuleHead mmh)
            (fromMaybe [] $ fmap (map sImportDecl . fst) mimps)

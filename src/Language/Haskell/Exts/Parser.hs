-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Parser
-- Copyright   :  (c) The GHC Team, 1997-2000
--                (c) Niklas Broberg, 2004-2012
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
                Parseable(..),
                ParseMode(..), defaultParseMode, ParseResult(..), fromParseResult,
                -- * Parsing of specific AST elements
                -- ** Modules
                parseModule, parseModuleWithMode, parseModuleWithComments,
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
                -- ** Option pragmas
                getTopPragmas
            ) where


import Language.Haskell.Exts.InternalParser ( ParseMode(..), defaultParseMode, ParseResult(..), fromParseResult )
import qualified Language.Haskell.Exts.InternalParser as P

import Language.Haskell.Exts.Annotated.Syntax
import qualified Language.Haskell.Exts.Syntax as S
import Language.Haskell.Exts.Annotated.Simplify

import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Comments

getTopPragmas :: String -> ParseResult [S.ModulePragma]
getTopPragmas = fmap (map sModulePragma) . P.getTopPragmas

-- | Class to reuse the parse function at many different types.
class Parseable ast where
  -- | Parse a string with default mode.
  parse :: String -> ParseResult ast
  -- | Parse a string with an explicit mode.
  parseWithMode :: ParseMode -> String -> ParseResult ast
  -- | Parse a string with an explicit mode, returning all comments along the AST
  parseWithComments :: ParseMode -> String -> ParseResult (ast, [Comment])


instance SrcInfo loc => Parseable (Module loc) where
  parse = fmap (fmap fromSrcInfo) . P.parseModule
  parseWithMode = (fmap (fmap fromSrcInfo) .) . P.parseModuleWithMode
  parseWithComments md s = P.parseModuleWithComments md s >>= \(r, cs) -> return (fmap fromSrcInfo r, cs)

instance SrcInfo loc => Parseable (Exp loc) where
  parse = fmap (fmap fromSrcInfo) . P.parseExp
  parseWithMode = (fmap (fmap fromSrcInfo) .) . P.parseExpWithMode
  parseWithComments md s = P.parseExpWithComments md s >>= \(r, cs) -> return (fmap fromSrcInfo r, cs)

instance SrcInfo loc => Parseable (Pat loc) where
  parse = fmap (fmap fromSrcInfo) . P.parsePat
  parseWithMode = (fmap (fmap fromSrcInfo) .) . P.parsePatWithMode
  parseWithComments md s = P.parsePatWithComments md s >>= \(r, cs) -> return (fmap fromSrcInfo r, cs)

instance SrcInfo loc => Parseable (Decl loc) where
  parse = fmap (fmap fromSrcInfo) . P.parseDecl
  parseWithMode = (fmap (fmap fromSrcInfo) .) . P.parseDeclWithMode
  parseWithComments md s = P.parseDeclWithComments md s >>= \(r, cs) -> return (fmap fromSrcInfo r, cs)

instance SrcInfo loc => Parseable (Type loc) where
  parse = fmap (fmap fromSrcInfo) . P.parseType
  parseWithMode = (fmap (fmap fromSrcInfo) .) . P.parseTypeWithMode
  parseWithComments md s = P.parseTypeWithComments md s >>= \(r, cs) -> return (fmap fromSrcInfo r, cs)

instance SrcInfo loc => Parseable (Stmt loc) where
  parse = fmap (fmap fromSrcInfo) . P.parseStmt
  parseWithMode = (fmap (fmap fromSrcInfo) .) . P.parseStmtWithMode
  parseWithComments md s = P.parseStmtWithComments md s >>= \(r, cs) -> return (fmap fromSrcInfo r, cs)


-- | Parse of a string, which should contain a complete Haskell module.
parseModule :: String -> ParseResult S.Module
parseModule = fmap sModule . P.parseModule

-- | Parse of a string containing a complete Haskell module, using an explicit mode.
parseModuleWithMode :: ParseMode -> String -> ParseResult S.Module
parseModuleWithMode = (fmap sModule .) . P.parseModuleWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit mode, retaining comments.
parseModuleWithComments :: ParseMode -> String -> ParseResult (S.Module, [Comment])
parseModuleWithComments = (fmap (\(mod, cs) -> (sModule mod, cs)) .) . P.parseModuleWithComments

-- | Parse of a string containing a Haskell expression.
parseExp :: String -> ParseResult S.Exp
parseExp = fmap sExp . P.parseExp

-- | Parse of a string containing a Haskell expression, using an explicit mode.
parseExpWithMode :: ParseMode -> String -> ParseResult S.Exp
parseExpWithMode = (fmap sExp .) . P.parseExpWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit mode, retaining comments.
parseExpWithComments :: ParseMode -> String -> ParseResult (S.Exp, [Comment])
parseExpWithComments = (fmap (\(e, cs) -> (sExp e, cs)) .) . P.parseExpWithComments

-- | Parse of a string containing a Haskell pattern.
parsePat :: String -> ParseResult S.Pat
parsePat = fmap sPat . P.parsePat

-- | Parse of a string containing a Haskell pattern, using an explicit mode.
parsePatWithMode :: ParseMode -> String -> ParseResult S.Pat
parsePatWithMode = (fmap sPat .) . P.parsePatWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit mode, retaining comments.
parsePatWithComments :: ParseMode -> String -> ParseResult (S.Pat, [Comment])
parsePatWithComments = (fmap (\(p, cs) -> (sPat p, cs)) .) . P.parsePatWithComments

-- | Parse of a string containing a Haskell top-level declaration.
parseDecl :: String -> ParseResult S.Decl
parseDecl = fmap sDecl . P.parseDecl

-- | Parse of a string containing a Haskell top-level declaration, using an explicit mode.
parseDeclWithMode :: ParseMode -> String -> ParseResult S.Decl
parseDeclWithMode = (fmap sDecl .) . P.parseDeclWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit mode, retaining comments.
parseDeclWithComments :: ParseMode -> String -> ParseResult (S.Decl, [Comment])
parseDeclWithComments = (fmap (\(decl, cs) -> (sDecl decl, cs)) .) . P.parseDeclWithComments

-- | Parse of a string containing a Haskell type.
parseType :: String -> ParseResult S.Type
parseType = fmap sType . P.parseType

-- | Parse of a string containing a Haskell type, using an explicit mode.
parseTypeWithMode :: ParseMode -> String -> ParseResult S.Type
parseTypeWithMode = (fmap sType .) . P.parseTypeWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit mode, retaining comments.
parseTypeWithComments :: ParseMode -> String -> ParseResult (S.Type, [Comment])
parseTypeWithComments = (fmap (\(t, cs) -> (sType t, cs)) .) . P.parseTypeWithComments

-- | Parse of a string containing a Haskell type.
parseStmt :: String -> ParseResult S.Stmt
parseStmt = fmap sStmt . P.parseStmt

-- | Parse of a string containing a Haskell type, using an explicit mode.
parseStmtWithMode :: ParseMode -> String -> ParseResult S.Stmt
parseStmtWithMode = (fmap sStmt .) . P.parseStmtWithMode

-- | Parse of a string containing a complete Haskell module, using an explicit mode, retaining comments.
parseStmtWithComments :: ParseMode -> String -> ParseResult (S.Stmt, [Comment])
parseStmtWithComments = (fmap (\(s, cs) -> (sStmt s, cs)) .) . P.parseStmtWithComments


instance Parseable S.Module where
  parse = parseModule
  parseWithMode = parseModuleWithMode
  parseWithComments = parseModuleWithComments

instance Parseable S.Exp where
  parse = parseExp
  parseWithMode = parseExpWithMode
  parseWithComments = parseExpWithComments

instance Parseable S.Pat where
  parse = parsePat
  parseWithMode = parsePatWithMode
  parseWithComments = parsePatWithComments

instance Parseable S.Decl where
  parse = parseDecl
  parseWithMode = parseDeclWithMode
  parseWithComments = parseDeclWithComments

instance Parseable S.Type where
  parse = parseType
  parseWithMode = parseTypeWithMode
  parseWithComments = parseTypeWithComments

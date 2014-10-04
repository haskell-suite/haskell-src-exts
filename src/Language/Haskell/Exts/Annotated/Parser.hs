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
    -- * Parsing of specific AST elements
    -- ** Modules
    , parseModule, parseModuleWithMode, parseModuleWithComments
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
    , ListOf(..), unListOf
    -- ** Option pragmas
    , getTopPragmas
    ) where

import Data.Data hiding (Fixity)
import Language.Haskell.Exts.Annotated.Fixity
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.InternalParser
import Language.Haskell.Exts.ParseMonad
import Language.Haskell.Exts.SrcLoc

instance Parseable (Decl   SrcSpanInfo) where parser = normalParser mparseDecl
instance Parseable (Exp    SrcSpanInfo) where parser = normalParser mparseExp
instance Parseable (Module SrcSpanInfo) where parser = normalParser mparseModule
instance Parseable (Pat    SrcSpanInfo) where parser = normalParser mparsePat
instance Parseable (Stmt   SrcSpanInfo) where parser = normalParser mparseStmt
instance Parseable (Type   SrcSpanInfo) where parser = normalParserNoFixity mparseType

normalParser :: AppFixity a => P (a SrcSpanInfo) -> Maybe [Fixity] -> P (a SrcSpanInfo)
normalParser p Nothing = p
normalParser p (Just fixs) = p >>= \ast -> applyFixities fixs ast `atSrcLoc` noLoc

normalParserNoFixity :: P (a SrcSpanInfo) -> Maybe [Fixity] -> P (a SrcSpanInfo)
normalParserNoFixity p _ = p

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

-- Non-greedy parsers (should use ng- prefixed parses exported by InternalParser)

-- | Non-greedy parse of a string starting with a series of top-level option pragmas.
getTopPragmas :: String -> ParseResult [ModulePragma SrcSpanInfo]
getTopPragmas = fmap (unListOf . unNonGreedy) . parse

instance Parseable (NonGreedy (ListOf (ModulePragma SrcSpanInfo))) where
  parser = nglistParserNoFixity ngparseModulePragmas

nglistParserNoFixity :: P ([a SrcSpanInfo], [SrcSpan], SrcSpanInfo) -> Maybe [Fixity] -> P (NonGreedy (ListOf (a SrcSpanInfo)))
nglistParserNoFixity f = fmap (NonGreedy . toListOf) . normalParserNoFixity f

-- | Instances of 'Parseable' for @NonGreedy a@ will only consume the input
--   until @a@ is fully parsed.  This means that parse errors that come later
--   in the input will be ignored.  It's also more efficient, as it's fully lazy
--   in the remainder of the input:
--
--   >>> parse (unlines ("module A where" : "main =" : repeat "blah")) :: ParseResult PragmasAndModuleHead
--   ParseOk (NonGreedy {unNonGreedy = PragmasAndModuleHead [] (ModuleName "A",Nothing,Nothing)})
--
--   (this example uses the simplified AST)
newtype NonGreedy a = NonGreedy { unNonGreedy :: a }
  deriving (Eq,Ord,Show,Typeable,Data)

instance Functor NonGreedy where
    fmap f (NonGreedy x) = NonGreedy (f x)

-- | @ListOf a@ stores lists of the AST type @a@, along with a 'SrcSpanInfo',
--   in order to provide 'Parseable' instances for lists.  These instances are
--   provided when the type is used as a list in the syntax, and the same
--   delimiters are used in all of its usages. Some exceptions are made:
data ListOf a = ListOf SrcSpanInfo [a]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)

unListOf :: ListOf a -> [a]
unListOf (ListOf _ xs) = xs

-- It's safe to forget about the previous SrcSpanInfo 'srcInfoPoints',
-- as long as they are created with (presently) are all created with
-- 'noInfoSpan' ('nIS'), '(<^^>)', or '(<++>)', all of which have
-- empty 'srcInfoPoints'. Ideally, the parsers would return better
-- types, but this works.
toListOf :: ([a], [SrcSpan], SrcSpanInfo) -> ListOf a
toListOf (xs, ss, l) = ListOf (infoSpan (srcInfoSpan l) ss) xs

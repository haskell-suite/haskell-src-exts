-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Annotated.Build
-- Copyright   :  (c) The GHC Team, 1997-2000,
--                (c) Niklas Broberg 2004
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains combinators to use when building
-- Haskell source trees programmatically, as opposed to
-- parsing them from a string. The contents here are quite
-- experimental and will likely receive a lot of attention
-- when the rest has stabilised.
--
-----------------------------------------------------------------------------

module Language.Haskell.Exts.Annotated.Build (

    -- * Syntax building functions
    name,       -- :: String -> Name
    sym,        -- :: String -> Name
    var,        -- :: Name -> Exp
    op,         -- :: Name -> QOp
    qvar,       -- :: Module -> Name -> Exp
    pvar,       -- :: Name -> Pat
    app,        -- :: Exp -> Exp -> Exp
    infixApp,   -- :: Exp -> QOp -> Exp -> Exp
    appFun,     -- :: Exp -> [Exp] -> Exp
    pApp,       -- :: Name -> [Pat] -> Pat
    tuple,      -- :: [Exp] -> Exp
    pTuple,     -- :: [Pat] -> Pat
    varTuple,   -- :: [Name] -> Exp
    pvarTuple,  -- :: [Name] -> Pat
    function,   -- :: String -> Exp
    strE,       -- :: String -> Exp
    charE,      -- :: Char -> Exp
    intE,       -- :: Integer -> Exp
    strP,       -- :: String -> Pat
    charP,      -- :: Char -> Pat
    intP,       -- :: Integer -> Pat
    doE,        -- :: [Stmt] -> Exp
    lamE,       -- :: SrcLoc -> [Pat] -> Exp -> Exp
    letE,       -- :: [Decl] -> Exp -> Exp
    caseE,      -- :: Exp -> [Alt] -> Exp
    alt,        -- :: SrcLoc -> Pat -> Exp -> Alt
    altGW,      -- :: SrcLoc -> Pat -> [Stmt] -> Exp -> Binds -> Alt
    listE,      -- :: [Exp] -> Exp
    eList,      -- :: Exp
    peList,     -- :: Pat
    paren,      -- :: Exp -> Exp
    pParen,     -- :: Pat -> Pat
    qualStmt,   -- :: Exp -> Stmt
    genStmt,    -- :: SrcLoc -> Pat -> Exp -> Stmt
    letStmt,    -- :: [Decl] -> Stmt
    binds,      -- :: [Decl] -> Binds
    noBinds,    -- :: Binds
    wildcard,   -- :: Pat
    genNames,   -- :: String -> Int -> [Name]

    -- * More advanced building
    sfun,           -- :: SrcLoc -> Name -> [Name] -> Rhs -> Binds -> Decl
    simpleFun,      -- :: SrcLoc -> Name -> Name -> Exp -> Decl
    patBind,        -- :: SrcLoc -> Pat -> Exp -> Decl
    patBindWhere,   -- :: SrcLoc -> Pat -> Exp -> [Decl] -> Decl
    nameBind,       -- :: SrcLoc -> Name -> Exp -> Decl
    metaFunction,   -- :: String -> [Exp] -> Exp
    metaConPat      -- :: String -> [Pat] -> Pat
  ) where

import Language.Haskell.Exts.Annotated.Syntax

-----------------------------------------------------------------------------
-- Help functions for Abstract syntax

-- | An identifier with the given string as its name.
--   The string should be a valid Haskell identifier.
name :: l -> String -> Name l
name = Ident

-- | A symbol identifier. The string should be a valid
--   Haskell symbol identifier.
sym :: l -> String -> Name l
sym = Symbol

-- | A local variable as expression.
var :: l -> Name l -> Exp l
var l = Var l . UnQual l

-- | Use the given identifier as an operator.
op :: l -> Name l -> QOp l
op l = QVarOp l . UnQual l

-- | A qualified variable as expression.
qvar :: l -> ModuleName l -> Name l -> Exp l
qvar l m = Var l . Qual l m

-- | A pattern variable.
pvar :: l -> Name l -> Pat l
pvar = PVar

-- | Application of expressions by juxtaposition.
app :: l -> Exp l -> Exp l -> Exp l
app = App

-- | Apply an operator infix.
infixApp :: l -> Exp l -> QOp l -> Exp l -> Exp l
infixApp = InfixApp

-- | Apply a function to a list of arguments.
appFun :: [l] -> Exp l -> [Exp l] -> Exp l
appFun _ f [] = f
appFun (l:ls) f (a:as) = appFun ls (app l f a) as

-- | A constructor pattern, with argument patterns.
pApp :: l -> Name l -> [Pat l] -> Pat l
pApp l n = PApp l (UnQual l n)

-- | A tuple expression.
tuple :: l -> [Exp l] -> Exp l
tuple l = Tuple l Boxed

-- | A tuple pattern.
pTuple :: l -> [Pat l] -> Pat l
pTuple l = PTuple l Boxed

-- | A tuple expression consisting of variables only.
varTuple :: l -> [Name l] -> Exp l
varTuple l ns = tuple l $ map (var l) ns

-- | A tuple pattern consisting of variables only.
pvarTuple :: l -> [Name l] -> Pat l
pvarTuple l ns = pTuple l $ map (pvar l) ns

-- | A function with a given name.
function :: l -> String -> Exp l
function l = var l . Ident l

-- | A literal string expression.
strE :: l -> String -> Exp l
strE l s = Lit l $ String l s s

-- | A literal character expression.
charE :: l -> Char -> Exp l
charE l c = Lit l $ Char l c [c]

-- | A literal integer expression.
intE :: l -> Integer -> Exp l
intE l i = Lit l $ Int l i (show i)

-- | A literal string pattern.
strP :: l -> String -> Pat l
strP l s = PLit l $ String l s s

-- | A literal character pattern.
charP :: l -> Char -> Pat l
charP l c = PLit l $ Char l c [c]

-- | A literal integer pattern.
intP :: l -> Integer -> Pat l
intP l i = PLit l $ Int l i (show i)

-- | A do block formed by the given statements.
--   The last statement in the list should be
--   a 'Qualifier' expression.
doE :: l -> [Stmt l] -> Exp l
doE = Do

-- | Lambda abstraction, given a list of argument
--   patterns and an expression body.
lamE :: l -> [Pat l] -> Exp l -> Exp l
lamE = Lambda

-- | A @let@ ... @in@ block.
letE :: l -> [Decl l] -> Exp l -> Exp l
letE l ds e = Let l (binds l ds) e

-- | A @case@ expression.
caseE :: l -> Exp l -> [Alt l] -> Exp l
caseE = Case

-- | An unguarded alternative in a @case@ expression.
alt :: l -> Pat l -> Exp l -> Alt l
alt l p e = Alt l p (unGAlt l e) Nothing

-- | An alternative with a single guard in a @case@ expression.
altGW :: l -> Pat l -> [Stmt l] -> Exp l -> Binds l -> Alt l
altGW l p gs e w = Alt l p (gAlt l gs e) (Just w)

-- | An unguarded righthand side of a @case@ alternative.
unGAlt :: l -> Exp l -> GuardedAlts l
unGAlt = UnGuardedAlt

-- | An list of guarded righthand sides for a @case@ alternative.
gAlts :: l -> [([Stmt l], Exp l)] -> GuardedAlts l
gAlts l as = GuardedAlts l $ map (\(gs,e) -> GuardedAlt l gs e) as

-- | A single guarded righthand side for a @case@ alternative.
gAlt :: l -> [Stmt l] -> Exp l -> GuardedAlts l
gAlt l gs e = gAlts l [(gs,e)]

-- | A list expression.
listE :: l -> [Exp l] -> Exp l
listE = List

-- | The empty list expression.
eList :: l -> Exp l
eList l = List l []

-- | The empty list pattern.
peList :: l -> Pat l
peList l = PList l []

-- | Put parentheses around an expression.
paren :: l -> Exp l -> Exp l
paren = Paren

-- | Put parentheses around a pattern.
pParen :: l -> Pat l -> Pat l
pParen = PParen

-- | A qualifier expression statement.
qualStmt :: l -> Exp l -> Stmt l
qualStmt = Qualifier

-- | A generator statement: /pat/ @<-@ /exp/
genStmt :: l -> Pat l -> Exp l -> Stmt l
genStmt = Generator

-- | A @let@ binding group as a statement.
letStmt :: l -> [Decl l] -> Stmt l
letStmt l ds = LetStmt l $ binds l ds

-- | Hoist a set of declarations to a binding group.
binds :: l -> [Decl l] -> Binds l
binds = BDecls

-- | An empty binding group.
noBinds :: l -> Binds l
noBinds l = binds l []

-- | The wildcard pattern: @_@
wildcard :: l -> Pat l
wildcard = PWildCard

-- | Generate k names by appending numbers 1 through k to a given string.
genNames :: l -> String -> Int -> [Name l]
genNames l s k = [ Ident l $ s ++ show i | i <- [1..k] ]

-------------------------------------------------------------------------------
-- Some more specialised help functions

-- | A function with a single clause
sfun :: l -> Name l -> [Name l] -> Rhs l -> Maybe (Binds l) -> Decl l
sfun l f pvs rhs mbs = FunBind l [Match l f (map (pvar l) pvs) rhs mbs]

-- | A function with a single clause, a single argument, no guards
-- and no where declarations
simpleFun :: l -> Name l -> Name l -> Exp l -> Decl l
simpleFun l f a e = let rhs = UnGuardedRhs l e
             in sfun l f [a] rhs Nothing

-- | A pattern bind where the pattern is a variable, and where
-- there are no guards and no 'where' clause.
patBind :: l -> Pat l -> Exp l -> Decl l
patBind l p e = let rhs = UnGuardedRhs l e
         in PatBind l p Nothing rhs Nothing

-- | A pattern bind where the pattern is a variable, and where
-- there are no guards, but with a 'where' clause.
patBindWhere :: l -> Pat l -> Exp l -> [Decl l] -> Decl l
patBindWhere l p e ds = let rhs = UnGuardedRhs l e
             in PatBind l p Nothing rhs (Just $ binds l ds)

-- | Bind an identifier to an expression.
nameBind :: l -> Name l -> Exp l -> Decl l
nameBind l n e = patBind l (pvar l n) e

-- | Apply function of a given name to a list of arguments.
metaFunction :: l -> String -> [Exp l] -> Exp l
metaFunction l s es = mf l s (reverse es)
  where mf l s []     = var l $ name l s
        mf l s (e:es) = app l (mf l s es) e

-- | Apply a constructor of a given name to a list of pattern
--   arguments, forming a constructor pattern.
metaConPat :: l -> String -> [Pat l] -> Pat l
metaConPat l s ps = pApp l (name l s) ps

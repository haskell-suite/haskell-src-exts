-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Build
-- Copyright   :  (c) The GHC Team, 1997-2000,
--                (c) Niklas Broberg 2004
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, d00nibro@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

-- This module badly needs Haddock documentation.
module Language.Haskell.Exts.Build (

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

import Language.Haskell.Exts.Syntax

-----------------------------------------------------------------------------
-- Help functions for Abstract syntax 

name :: String -> Name
name = Ident

sym :: String -> Name
sym = Symbol

var :: Name -> Exp
var = Var . UnQual

op :: Name -> QOp
op = QVarOp . UnQual

qvar :: ModuleName -> Name -> Exp
qvar m n = Var $ Qual m n

pvar :: Name -> Pat
pvar = PVar

app :: Exp -> Exp -> Exp
app = App

infixApp :: Exp -> QOp -> Exp -> Exp
infixApp = InfixApp

appFun :: Exp -> [Exp] -> Exp
appFun f [] = f
appFun f (a:as) = appFun (app f a) as

pApp :: Name -> [Pat] -> Pat
pApp n ps = PApp (UnQual n) ps

tuple :: [Exp] -> Exp
tuple = Tuple

pTuple :: [Pat] -> Pat
pTuple = PTuple

varTuple :: [Name] -> Exp
varTuple ns = tuple $ map var ns

pvarTuple :: [Name] -> Pat
pvarTuple ns = pTuple $ map pvar ns

function :: String -> Exp
function = var . Ident

strE :: String -> Exp
strE = Lit . String

charE :: Char -> Exp
charE = Lit . Char

intE :: Integer -> Exp
intE = Lit . Int

strP :: String -> Pat
strP = PLit . String

charP :: Char -> Pat
charP = PLit . Char

intP :: Integer -> Pat
intP = PLit . Int

doE :: [Stmt] -> Exp
doE = Do

lamE :: SrcLoc -> [Pat] -> Exp -> Exp
lamE = Lambda

letE :: [Decl] -> Exp -> Exp
letE ds e = Let (binds ds) e

caseE :: Exp -> [Alt] -> Exp
caseE = Case

alt :: SrcLoc -> Pat -> Exp -> Alt
alt s p e = Alt s p (UnGuardedAlt e) noBinds

altGW :: SrcLoc -> Pat -> [Stmt] -> Exp -> Binds -> Alt
altGW s p gs e w = Alt s p (gAlt s gs e) w

unGAlt :: Exp -> GuardedAlts
unGAlt = UnGuardedAlt

gAlts :: SrcLoc -> [([Stmt],Exp)] -> GuardedAlts
gAlts s as = GuardedAlts $ map (\(gs,e) -> GuardedAlt s gs e) as

gAlt :: SrcLoc -> [Stmt] -> Exp -> GuardedAlts
gAlt s gs e = gAlts s [(gs,e)]

listE :: [Exp] -> Exp
listE = List

eList :: Exp
eList = List []

peList :: Pat
peList = PList []

paren :: Exp -> Exp
paren = Paren

pParen :: Pat -> Pat
pParen = PParen

qualStmt :: Exp -> Stmt
qualStmt = Qualifier

genStmt :: SrcLoc -> Pat -> Exp -> Stmt
genStmt = Generator

letStmt :: [Decl] -> Stmt
letStmt ds = LetStmt $ binds ds

binds :: [Decl] -> Binds
binds = BDecls

noBinds :: Binds
noBinds = binds []

wildcard :: Pat
wildcard = PWildCard

genNames :: String -> Int -> [Name]
genNames s k = [ Ident $ s ++ show i | i <- [1..k] ]

-------------------------------------------------------------------------------
-- Some more specialised help functions 

-- | A function with a single "match"
sfun :: SrcLoc -> Name -> [Name] -> Rhs -> Binds -> Decl
sfun s f pvs rhs bs = FunBind [Match s f (map pvar pvs) Nothing rhs bs]

-- | A function with a single "match", a single argument, no guards
-- and no where declarations
simpleFun :: SrcLoc -> Name -> Name -> Exp -> Decl
simpleFun s f a e = let rhs = UnGuardedRhs e
             in sfun s f [a] rhs noBinds

-- | A pattern bind where the pattern is a variable, and where
-- there are no guards and no 'where' clause.
patBind :: SrcLoc -> Pat -> Exp -> Decl
patBind s p e = let rhs = UnGuardedRhs e
         in PatBind s p Nothing rhs noBinds

patBindWhere :: SrcLoc -> Pat -> Exp -> [Decl] -> Decl
patBindWhere s p e ds = let rhs = UnGuardedRhs e
             in PatBind s p Nothing rhs (binds ds)

nameBind :: SrcLoc -> Name -> Exp -> Decl
nameBind s n e = patBind s (pvar n) e

-- meta-level functions, i.e. functions that represent functions, 
-- and that take arguments representing arguments... whew!
metaFunction :: String -> [Exp] -> Exp
metaFunction s es = mf s (reverse es)
  where mf s []     = var $ name s
        mf s (e:es) = app (mf s es) e


metaConPat :: String -> [Pat] -> Pat
metaConPat s ps = pApp (name s) ps

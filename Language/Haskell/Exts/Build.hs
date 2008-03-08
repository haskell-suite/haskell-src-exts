-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Build
-- Original    :  Language.Haskell.Syntax
-- Copyright   :  (c) The GHC Team, 1997-2000,
--                (c) Niklas Broberg 2004
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, d00nibro@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Language.Haskell.Exts.Build (

    -- * Syntax building functions
    name,		-- :: String -> HsName
    sym,		-- :: String -> HsName
    var,		-- :: HsName -> HsExp
    op,			-- :: HsName -> HsQOp
    qvar,		-- :: Module -> HsName -> HsExp
    pvar,		-- :: HsName -> HsPat
    app,		-- :: HsExp -> HsExp -> HsExp
    infixApp,		-- :: HsExp -> HsQOp -> HsExp -> HsExp
    appFun,		-- :: HsExp -> [HsExp] -> HsExp
    pApp,		-- :: HsName -> [HsPat] -> HsPat
    tuple,		-- :: [HsExp] -> HsExp
    pTuple,		-- :: [HsPat] -> HsPat
    varTuple,		-- :: [HsName] -> HsExp
    pvarTuple,		-- :: [HsName] -> HsPat
    function,		-- :: String -> HsExp
    strE,		-- :: String -> HsExp
    charE,		-- :: Char -> HsExp
    intE,		-- :: Integer -> HsExp
    strP,		-- :: String -> HsPat
    charP,		-- :: Char -> HsPat
    intP,		-- :: Integer -> HsPat
    doE,		-- :: [HsStmt] -> HsExp
    lamE,		-- :: SrcLoc -> [HsPat] -> HsExp -> HsExp
    letE,		-- :: [HsDecl] -> HsExp -> HsExp
    caseE,		-- :: HsExp -> [HsAlt] -> HsExp
    alt,		-- :: SrcLoc -> HsPat -> HsExp -> HsAlt
    altGW,		-- :: SrcLoc -> HsPat -> [HsStmt] -> HsExp -> HsBinds -> HsAlt
    listE,		-- :: [HsExp] -> HsExp
    eList,		-- :: HsExp
    peList,		-- :: HsPat
    paren,		-- :: HsExp -> HsExp
    pParen,		-- :: HsPat -> HsPat
    qualStmt,		-- :: HsExp -> HsStmt
    genStmt,		-- :: SrcLoc -> HsPat -> HsExp -> HsStmt
    letStmt,		-- :: [HsDecl] -> HsStmt
    binds,		-- :: [HsDecl] -> HsBinds
    noBinds,		-- :: HsBinds
    wildcard,		-- :: HsPat
    genNames,		-- :: String -> Int -> [HsName]
    
    -- * More advanced building
    sfun,		-- :: SrcLoc -> HsName -> [HsName] -> HsRhs -> HsBinds -> HsDecl
    simpleFun,		-- :: SrcLoc -> HsName -> HsName -> HsExp -> HsDecl
    patBind,		-- :: SrcLoc -> HsPat -> HsExp -> HsDecl
    patBindWhere,	-- :: SrcLoc -> HsPat -> HsExp -> [HsDecl] -> HsDecl
    nameBind,		-- :: SrcLoc -> HsName -> HsExp -> HsDecl
    metaFunction,	-- :: String -> [HsExp] -> HsExp
    metaConPat		-- :: String -> [HsPat] -> HsPat
  ) where

import Language.Haskell.Exts.Syntax

-----------------------------------------------------------------------------
-- Help functions for Abstract syntax 

name :: String -> HsName
name = HsIdent

sym :: String -> HsName
sym = HsSymbol

var :: HsName -> HsExp
var = HsVar . UnQual

op :: HsName -> HsQOp
op = HsQVarOp . UnQual

qvar :: Module -> HsName -> HsExp
qvar m n = HsVar $ Qual m n

pvar :: HsName -> HsPat
pvar = HsPVar

app :: HsExp -> HsExp -> HsExp
app = HsApp

infixApp :: HsExp -> HsQOp -> HsExp -> HsExp
infixApp = HsInfixApp

appFun :: HsExp -> [HsExp] -> HsExp
appFun f [] = f
appFun f (a:as) = appFun (app f a) as

pApp :: HsName -> [HsPat] -> HsPat
pApp n ps = HsPApp (UnQual n) ps

tuple :: [HsExp] -> HsExp
tuple = HsTuple

pTuple :: [HsPat] -> HsPat
pTuple = HsPTuple

varTuple :: [HsName] -> HsExp
varTuple ns = tuple $ map var ns

pvarTuple :: [HsName] -> HsPat
pvarTuple ns = pTuple $ map pvar ns

function :: String -> HsExp
function = var . HsIdent

strE :: String -> HsExp
strE = HsLit . HsString

charE :: Char -> HsExp
charE = HsLit . HsChar

intE :: Integer -> HsExp
intE = HsLit . HsInt

strP :: String -> HsPat
strP = HsPLit . HsString

charP :: Char -> HsPat
charP = HsPLit . HsChar

intP :: Integer -> HsPat
intP = HsPLit . HsInt

doE :: [HsStmt] -> HsExp
doE = HsDo

lamE :: SrcLoc -> [HsPat] -> HsExp -> HsExp
lamE = HsLambda

letE :: [HsDecl] -> HsExp -> HsExp
letE ds e = HsLet (binds ds) e

caseE :: HsExp -> [HsAlt] -> HsExp
caseE = HsCase

alt :: SrcLoc -> HsPat -> HsExp -> HsAlt
alt s p e = HsAlt s p (HsUnGuardedAlt e) noBinds

altGW :: SrcLoc -> HsPat -> [HsStmt] -> HsExp -> HsBinds -> HsAlt
altGW s p gs e w = HsAlt s p (gAlt s gs e) w

unGAlt :: HsExp -> HsGuardedAlts
unGAlt = HsUnGuardedAlt

gAlts :: SrcLoc -> [([HsStmt],HsExp)] -> HsGuardedAlts
gAlts s as = HsGuardedAlts $ map (\(gs,e) -> HsGuardedAlt s gs e) as

gAlt :: SrcLoc -> [HsStmt] -> HsExp -> HsGuardedAlts
gAlt s gs e = gAlts s [(gs,e)]

listE :: [HsExp] -> HsExp
listE = HsList

eList :: HsExp
eList = HsList []

peList :: HsPat
peList = HsPList []

paren :: HsExp -> HsExp
paren = HsParen

pParen :: HsPat -> HsPat
pParen = HsPParen

qualStmt :: HsExp -> HsStmt
qualStmt = HsQualifier

genStmt :: SrcLoc -> HsPat -> HsExp -> HsStmt
genStmt = HsGenerator

letStmt :: [HsDecl] -> HsStmt
letStmt ds = HsLetStmt $ binds ds

binds :: [HsDecl] -> HsBinds
binds = HsBDecls

noBinds :: HsBinds
noBinds = binds []

wildcard :: HsPat
wildcard = HsPWildCard

genNames :: String -> Int -> [HsName]
genNames s k = [ HsIdent $ s ++ show i | i <- [1..k] ]

-------------------------------------------------------------------------------
-- Some more specialised help functions 

-- | A function with a single "match"
sfun :: SrcLoc -> HsName -> [HsName] -> HsRhs -> HsBinds -> HsDecl
sfun s f pvs rhs bs = HsFunBind [HsMatch s f (map pvar pvs) rhs bs]

-- | A function with a single "match", a single argument, no guards
-- and no where declarations
simpleFun :: SrcLoc -> HsName -> HsName -> HsExp -> HsDecl
simpleFun s f a e = let rhs = HsUnGuardedRhs e
		     in sfun s f [a] rhs noBinds

-- | A pattern bind where the pattern is a variable, and where
-- there are no guards and no 'where' clause.
patBind :: SrcLoc -> HsPat -> HsExp -> HsDecl
patBind s p e = let rhs = HsUnGuardedRhs e
		 in HsPatBind s p rhs noBinds

patBindWhere :: SrcLoc -> HsPat -> HsExp -> [HsDecl] -> HsDecl
patBindWhere s p e ds = let rhs = HsUnGuardedRhs e
			 in HsPatBind s p rhs (binds ds)

nameBind :: SrcLoc -> HsName -> HsExp -> HsDecl
nameBind s n e = patBind s (pvar n) e

-- meta-level functions, i.e. functions that represent functions, 
-- and that take arguments representing arguments... whew!
metaFunction :: String -> [HsExp] -> HsExp
metaFunction s es = mf s (reverse es)
  where mf s []     = var $ name s
	mf s (e:es) = app (mf s es) e


metaConPat :: String -> [HsPat] -> HsPat
metaConPat s ps = pApp (name s) ps

-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.ParseUtils
-- Original    :  Language.Haskell.ParseUtils
-- Copyright   :  (c) Niklas Broberg 2004,
--        (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, d00nibro@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for the Haskell-exts parser.
--
-----------------------------------------------------------------------------

module Language.Haskell.Exts.ParseUtils (
      splitTyConApp     -- HsType -> P (HsName,[HsType])
    , mkRecConstrOrUpdate   -- HsExp -> [HsFieldUpdate] -> P HsExp
    , checkPrec         -- Integer -> P Int
    , checkContext      -- HsType -> P HsContext
    , checkAssertion    -- HsType -> P HsAsst
    , checkDataHeader   -- HsType -> P (HsContext,HsName,[HsName])
    , checkClassHeader  -- HsType -> P (HsContext,HsName,[HsName])
    , checkInstHeader   -- HsType -> P (HsContext,HsQName,[HsType])
    , checkPattern      -- HsExp -> P HsPat
    , checkExpr         -- HsExp -> P HsExp
    , checkStmt         -- HsStmt -> P HsStmt
    , checkValDef       -- SrcLoc -> HsExp -> HsRhs -> [HsDecl] -> P HsDecl
    , checkClassBody    -- [HsClassDecl] -> P [HsClassDecl]
    , checkInstBody     -- [HsInstDecl] -> P [HsInstDecl]
    , checkUnQual       -- HsQName -> P HsName
    , checkRevDecls     -- [HsDecl] -> P [HsDecl]
    , checkRevClsDecls  -- [HsClassDecl] -> P [HsClassDecl]
    , checkRevInstDecls -- [HsInstDecl] -> P [HsInstDecl]
    , checkDataOrNew    -- DataOrNew -> [HsDecl] -> P ()
    , checkSimpleType   -- HsType -> P ()
    , getGConName       -- HsExp -> P HsQName
    , mkHsTyForall      -- Maybe [HsName] -> HsContext -> HsType -> HsType 
    -- HaRP
    , checkRPattern     -- HsExp -> P HsRPat
    -- Hsx
    , checkEqNames      -- HsXName -> HsXName -> P HsXName
    , mkPageModule      -- HsExp -> P HsModule
    , mkPage        -- HsModule -> SrcLoc -> HsExp -> P HsModule
    , mkDVar        -- [String] -> String
    , mkDVarExpr        -- [String] -> HsExp
    ) where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.ParseMonad
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Build

import Data.List (intersperse)

splitTyConApp :: HsType -> P (HsName,[HsType])
splitTyConApp t0 = split t0 []
 where
    split :: HsType -> [HsType] -> P (HsName,[HsType])
    split (HsTyApp t u) ts = split t (u:ts)
    split (HsTyCon (UnQual t)) ts = return (t,ts)
    split (HsTyInfix a op b) ts = split (HsTyCon op) (a:b:ts)
    split _ _ = fail "Illegal data/newtype declaration"

-----------------------------------------------------------------------------
-- Various Syntactic Checks

checkContext :: HsType -> P HsContext
checkContext (HsTyTuple Boxed ts) =
    mapM checkAssertion ts
checkContext t = do
    c <- checkAssertion t
    return [c]

-- Changed for multi-parameter type classes.
-- Further changed for implicit parameters.

checkAssertion :: HsType -> P HsAsst
checkAssertion (HsTyPred p@(HsIParam _ _)) = return p
checkAssertion (HsTyPred p@(HsEqualP _ _)) = return p
checkAssertion t = checkAssertion' [] t
    where   checkAssertion' ts (HsTyCon c) = return $ HsClassA c ts
            checkAssertion' ts (HsTyApp a t) = checkAssertion' (t:ts) a
            checkAssertion' ts (HsTyInfix a op b) = checkAssertion' (a:b:ts) (HsTyCon op)
            checkAssertion' _ _ = fail "Illegal class assertion"


checkDataHeader :: HsType -> P (HsContext,HsName,[HsName])
checkDataHeader (HsTyForall Nothing cs t) = do
    (c,ts) <- checkSimple "data/newtype" t []
    return (cs,c,ts)
checkDataHeader t = do
    (c,ts) <- checkSimple "data/newtype" t []
    return ([],c,ts)

checkClassHeader :: HsType -> P (HsContext,HsName,[HsName])
checkClassHeader (HsTyForall Nothing cs t) = do
    (c,ts) <- checkSimple "class" t []
    return (cs,c,ts)
checkClassHeader t = do
    (c,ts) <- checkSimple "class" t []
    return ([],c,ts)

checkSimple :: String -> HsType -> [HsName] -> P (HsName,[HsName])
checkSimple kw (HsTyApp l (HsTyVar a)) xs = checkSimple kw l (a:xs)
checkSimple _  (HsTyInfix (HsTyVar a) (UnQual t) (HsTyVar b)) xs = return (t,a:b:xs)
checkSimple _kw (HsTyCon (UnQual t))   xs = return (t,xs)
checkSimple kw _ _ = fail ("Illegal " ++ kw ++ " declaration")

checkInstHeader :: HsType -> P (HsContext,HsQName,[HsType])
checkInstHeader (HsTyForall Nothing cs t) = do
    (c,ts) <- checkInsts t []
    return (cs,c,ts)
checkInstHeader t = do
    (c,ts) <- checkInsts t []
    return ([],c,ts)
    

checkInsts :: HsType -> [HsType] -> P ((HsQName,[HsType]))
checkInsts (HsTyApp l t) ts = checkInsts l (t:ts)
checkInsts (HsTyCon c)   ts = return (c,ts)
checkInsts _ _ = fail "Illegal instance declaration"

{-
checkInst :: HsType -> P ()
checkInst (HsTyApp l _) = checkInst l
checkInst (HsTyVar _)   = fail "Illegal instance declaration"
checkInst _             = return ()
-}

-----------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: HsExp -> P HsPat
checkPattern e = checkPat e []

checkPat :: HsExp -> [HsPat] -> P HsPat
checkPat (HsCon c) args = return (HsPApp c args)
checkPat (HsApp f x) args = do
    x <- checkPat x []
    checkPat f (x:args)
checkPat e [] = case e of
    HsVar (UnQual x)   -> return (HsPVar x)
    HsLit l            -> return (HsPLit l)
    HsInfixApp l op r  -> do
                  l <- checkPat l []
                  r <- checkPat r []
                  case op of
                    HsQConOp c -> return (HsPInfixApp l c r)
                    _ -> patFail ""
    HsTuple es         -> do
                  ps <- mapM (\e -> checkPat e []) es
                  return (HsPTuple ps)
    HsList es      -> do
                  ps <- mapM checkRPattern es
                  return $ if all isStdPat ps
                            then HsPList $ map stripRP ps
                            else HsPRPat $ map fixRPOpPrec ps
            where isStdPat :: HsRPat -> Bool
                  isStdPat (HsRPPat _) = True
                  isStdPat (HsRPAs _ p) = isStdPat p
                  isStdPat _           = False
                  stripRP :: HsRPat -> HsPat
                  stripRP (HsRPPat  p) = p
                  stripRP (HsRPAs n p) = HsPAsPat n (stripRP p)
                  stripRP _           = error "cannot strip RP wrapper if not all patterns are base"

    HsParen e      -> do
                  p <- checkPat e []
                  return (HsPParen p)
    HsAsPat n e    -> do
                  p <- checkPat e []
                  return (HsPAsPat n p)
    HsWildCard     -> return HsPWildCard
    HsIrrPat e     -> do
                  p <- checkPat e []
                  return (HsPIrrPat p)
    HsRecConstr c fs   -> do
                  fs <- mapM checkPatField fs
                  return (HsPRec c fs)
    HsNegApp (HsLit l) -> return (HsPNeg (HsPLit l))
    HsExpTypeSig s e t -> do
                  p <- checkPat e []
                  return (HsPatTypeSig s p t)
    
    -- Hsx
    HsXTag s n attrs mattr cs -> do
                  pattrs <- mapM checkPAttr attrs
                  pcs    <- mapM (\c -> checkPat c []) cs
                  mpattr <- maybe (return Nothing) 
                              (\e -> do p <- checkPat e []
                                        return $ Just p) 
                              mattr
                  let cps = mkChildrenPat pcs
                  return $ HsPXTag s n pattrs mpattr cps
    HsXETag s n attrs mattr -> do
                  pattrs <- mapM checkPAttr attrs
                  mpattr <- maybe (return Nothing) 
                              (\e -> do p <- checkPat e []
                                        return $ Just p) 
                              mattr
                  return $ HsPXETag s n pattrs mpattr
    HsXPcdata pcdata   -> return $ HsPXPcdata pcdata
    HsXExpTag e -> do
            p <- checkPat e []
            return $ HsPXPatTag p
    HsXRPats es -> do
            rps <- mapM checkRPattern es
            return (HsPXRPats $ map fixRPOpPrec rps)
    e -> patFail $ show e

checkPat e _ = patFail $ show e

checkPatField :: HsFieldUpdate -> P HsPatField
checkPatField (HsFieldUpdate n e) = do
    p <- checkPat e []
    return (HsPFieldPat n p)

checkPAttr :: HsXAttr -> P HsPXAttr
checkPAttr (HsXAttr n v) = do p <- checkPat v []
                              return $ HsPXAttr n p

patFail :: String -> P a
patFail s = fail $ "Parse error in pattern: " ++ s

checkRPattern :: HsExp -> P HsRPat
checkRPattern e = case e of
    HsSeqRP es -> do 
        rps <- mapM checkRPattern es
        return $ HsRPSeq rps
    HsPostOp e op -> do
        rpop <- checkRPatOp op
        rp   <- checkRPattern e
        return $ HsRPOp rp rpop
    HsGuardRP e gs -> do
        rp <- checkPattern e
        return $ HsRPGuard rp gs
    HsEitherRP e1 e2 -> do
        rp1 <- checkRPattern e1
        rp2 <- checkRPattern e2
        return $ HsRPEither rp1 rp2
    HsCAsRP n e -> do
        rp <- checkRPattern e
        return $ HsRPCAs n rp
    HsAsPat n e  -> do
        rp <- checkRPattern e
        return $ HsRPAs n rp
    HsParen e -> do
        rp <- checkRPattern e
        return $ HsRPParen rp
    _          -> do
        p <- checkPattern e
        return $ HsRPPat p

checkRPatOp :: HsQOp -> P HsRPatOp
checkRPatOp o@(HsQVarOp (UnQual (HsSymbol sym))) =
    case sym of
     "*"  -> return HsRPStar
     "*!" -> return HsRPStarG
     "+"  -> return HsRPPlus
     "+!" -> return HsRPPlusG
     "?"  -> return HsRPOpt
     "?!" -> return HsRPOptG
     _    -> rpOpFail o
checkRPatOp o = rpOpFail o

rpOpFail sym = fail $ "Unrecognized regular pattern operator: " ++ show sym

fixRPOpPrec :: HsRPat -> HsRPat
fixRPOpPrec rp = case rp of
    HsRPOp rp rpop      -> fPrecOp rp (flip HsRPOp rpop)
    HsRPEither rp1 rp2  -> HsRPEither (fixRPOpPrec rp1) (fixRPOpPrec rp2)
    HsRPSeq rps         -> HsRPSeq $ map fixRPOpPrec rps
    HsRPCAs n rp        -> HsRPCAs n $ fixRPOpPrec rp
    HsRPAs n rp         -> HsRPAs n $ fixRPOpPrec rp
    HsRPParen rp        -> HsRPParen $ fixRPOpPrec rp
    _                   -> rp

  where fPrecOp :: HsRPat -> (HsRPat -> HsRPat) -> HsRPat
        fPrecOp (HsRPOp rp rpop) f = fPrecOp rp (f . flip HsRPOp rpop)
        fPrecOp (HsRPCAs n rp) f = fPrecAs rp f (HsRPCAs n)
        fPrecOp (HsRPAs  n rp) f = fPrecAs rp f (HsRPAs  n)
        fPrecOp rp f = f $ fixRPOpPrec rp
        fPrecAs :: HsRPat -> (HsRPat -> HsRPat) -> (HsRPat -> HsRPat) -> HsRPat
        fPrecAs (HsRPCAs n rp) f g = fPrecAs rp f (g . HsRPCAs n)
        fPrecAs (HsRPAs  n rp) f g = fPrecAs rp f (g . HsRPAs  n)
        fPrecAs rp f g = g . f $ fixRPOpPrec rp


mkChildrenPat :: [HsPat] -> [HsPat]
mkChildrenPat ps = mkCPAux ps []
  where mkCPAux :: [HsPat] -> [HsPat] -> [HsPat]
        mkCPAux [] qs = reverse qs
        mkCPAux (p:ps) qs = case p of
            (HsPRPat rps) -> [mkCRP ps (reverse rps ++ map HsRPPat qs)]
            _             -> mkCPAux ps (p:qs)
        
        mkCRP :: [HsPat] -> [HsRPat] -> HsPat
        mkCRP [] rps = HsPXRPats $ reverse rps
        mkCRP (p:ps) rps = case p of
            (HsPXRPats rqs) -> mkCRP ps (reverse rqs ++ rps)
            _               -> mkCRP ps (HsRPPat p : rps)

-----------------------------------------------------------------------------
-- Check Expression Syntax

checkExpr :: HsExp -> P HsExp
checkExpr e = case e of
    HsVar _               -> return e
    HsIPVar _             -> return e
    HsCon _               -> return e
    HsLit _               -> return e
    HsInfixApp e1 op e2   -> check2Exprs e1 e2 (flip HsInfixApp op)
    HsApp e1 e2           -> check2Exprs e1 e2 HsApp
    HsNegApp e            -> check1Expr e HsNegApp
    HsLambda loc ps e     -> check1Expr e (HsLambda loc ps)
    HsLet bs e            -> check1Expr e (HsLet bs)
    HsDLet bs e           -> check1Expr e (HsDLet bs)
    HsWith e bs           -> check1Expr e (flip HsWith bs)
    HsIf e1 e2 e3         -> check3Exprs e1 e2 e3 HsIf
    HsCase e alts         -> do
                     alts <- mapM checkAlt alts
                     e <- checkExpr e
                     return (HsCase e alts)
    HsDo stmts        -> do
                     stmts <- mapM checkStmt stmts
                     return (HsDo stmts)
    HsMDo stmts       -> do
                     stmts <- mapM checkStmt stmts
                     return (HsMDo stmts)
    HsTuple es        -> checkManyExprs es HsTuple
    HsList es         -> checkManyExprs es HsList
    -- Since we don't parse things as left sections, we need to mangle them into that.
    HsParen e         -> case e of
                          HsPostOp e1 op -> check1Expr e1 (flip HsLeftSection op)
                          _              -> check1Expr e HsParen
    --HsLeftSection e op    -> check1Expr e (flip HsLeftSection op)
    HsRightSection op e   -> check1Expr e (HsRightSection op)
    HsRecConstr c fields      -> do
                     fields <- mapM checkField fields
                     return (HsRecConstr c fields)
    HsRecUpdate e fields      -> do
                     fields <- mapM checkField fields
                     e <- checkExpr e
                     return (HsRecUpdate e fields)
    HsEnumFrom e          -> check1Expr e HsEnumFrom
    HsEnumFromTo e1 e2    -> check2Exprs e1 e2 HsEnumFromTo
    HsEnumFromThen e1 e2      -> check2Exprs e1 e2 HsEnumFromThen
    HsEnumFromThenTo e1 e2 e3 -> check3Exprs e1 e2 e3 HsEnumFromThenTo
    HsListComp e stmts        -> do
                     stmts <- mapM checkStmt stmts
                     e <- checkExpr e
                     return (HsListComp e stmts)
    HsExpTypeSig loc e ty     -> do
                     e <- checkExpr e
                     return (HsExpTypeSig loc e ty)
    
    --Template Haskell
--    HsReifyExp _          -> return e
    HsBracketExp _        -> return e
    HsSpliceExp _         -> return e
    HsTypQuote _          -> return e
    HsVarQuote _          -> return e
    
    -- Hsx
    HsXTag s n attrs mattr cs -> do attrs <- mapM checkAttr attrs
                                    cs <- mapM checkExpr cs
                                    mattr <- maybe (return Nothing) 
                                              (\e -> checkExpr e >>= return . Just) 
                                              mattr                 
                                    return $ HsXTag s n attrs mattr cs
    HsXETag s n attrs mattr   -> do attrs <- mapM checkAttr attrs
                                    mattr <- maybe (return Nothing) 
                                              (\e -> checkExpr e >>= return . Just) 
                                              mattr                 
                                    return $ HsXETag s n attrs mattr 
    HsXPcdata _       -> return e
    HsXExpTag e       -> do e <- checkExpr e
                            return $ HsXExpTag e
    _             -> fail $ "Parse error in expression: " ++ show e

checkAttr :: HsXAttr -> P HsXAttr
checkAttr (HsXAttr n v) = do v <- checkExpr v
                             return $ HsXAttr n v

-- type signature for polymorphic recursion!!
check1Expr :: HsExp -> (HsExp -> a) -> P a
check1Expr e1 f = do
    e1 <- checkExpr e1
    return (f e1)

check2Exprs :: HsExp -> HsExp -> (HsExp -> HsExp -> a) -> P a
check2Exprs e1 e2 f = do
    e1 <- checkExpr e1
    e2 <- checkExpr e2
    return (f e1 e2)

check3Exprs :: HsExp -> HsExp -> HsExp -> (HsExp -> HsExp -> HsExp -> a) -> P a
check3Exprs e1 e2 e3 f = do
    e1 <- checkExpr e1
    e2 <- checkExpr e2
    e3 <- checkExpr e3
    return (f e1 e2 e3)

checkManyExprs :: [HsExp] -> ([HsExp] -> a) -> P a
checkManyExprs es f = do
    es <- mapM checkExpr es
    return (f es)

checkAlt :: HsAlt -> P HsAlt
checkAlt (HsAlt loc p galts bs) = do
    galts <- checkGAlts galts
    return (HsAlt loc p galts bs)

checkGAlts :: HsGuardedAlts -> P HsGuardedAlts
checkGAlts (HsUnGuardedAlt e) = check1Expr e HsUnGuardedAlt
checkGAlts (HsGuardedAlts galts) = do
    galts <- mapM checkGAlt galts
    return (HsGuardedAlts galts)

checkGAlt :: HsGuardedAlt -> P HsGuardedAlt
checkGAlt (HsGuardedAlt loc g e) = check1Expr e (HsGuardedAlt loc g)

checkStmt :: HsStmt -> P HsStmt
checkStmt (HsGenerator loc p e) = check1Expr e (HsGenerator loc p)
checkStmt (HsQualifier e) = check1Expr e HsQualifier
checkStmt s@(HsLetStmt _) = return s

checkField :: HsFieldUpdate -> P HsFieldUpdate
checkField (HsFieldUpdate n e) = check1Expr e (HsFieldUpdate n)

getGConName :: HsExp -> P HsQName
getGConName (HsCon n) = return n
getGConName (HsList []) = return list_cons_name
getGConName _ = fail "Expression in reification is not a name"

-----------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef :: SrcLoc -> HsExp -> HsRhs -> HsBinds -> P HsDecl
checkValDef srcloc lhs rhs whereBinds =
    case isFunLhs lhs [] of
     Just (f,es) -> do
            ps <- mapM checkPattern es
            return (HsFunBind [HsMatch srcloc f ps rhs whereBinds])
     Nothing     -> do
            lhs <- checkPattern lhs
            return (HsPatBind srcloc lhs rhs whereBinds)

-- A variable binding is parsed as an HsPatBind.

isFunLhs :: HsExp -> [HsExp] -> Maybe (HsName, [HsExp])
isFunLhs (HsInfixApp l (HsQVarOp (UnQual op)) r) es = Just (op, l:r:es)
isFunLhs (HsApp (HsVar (UnQual f)) e) es = Just (f, e:es)
isFunLhs (HsApp (HsParen f) e) es = isFunLhs f (e:es)
isFunLhs (HsApp f e) es = isFunLhs f (e:es)
isFunLhs _ _ = Nothing

-----------------------------------------------------------------------------
-- In a class or instance body, a pattern binding must be of a variable.

checkClassBody :: [HsClassDecl] -> P [HsClassDecl]
checkClassBody decls = do
    mapM_ checkClassMethodDef decls
    return decls
  where checkClassMethodDef (HsClsDecl decl) = checkMethodDef decl
        checkClassMethodDef _ = return ()

checkInstBody :: [HsInstDecl] -> P [HsInstDecl]
checkInstBody decls = do
    mapM_ checkInstMethodDef decls
    return decls
  where checkInstMethodDef (HsInsDecl decl) = checkMethodDef decl
        checkInstMethodDef _ = return ()

checkMethodDef :: HsDecl -> P ()
checkMethodDef (HsPatBind _ (HsPVar _) _ _) = return ()
checkMethodDef (HsPatBind loc _ _ _) =
    fail "illegal method definition" `atSrcLoc` loc
checkMethodDef _ = return ()

-----------------------------------------------------------------------------
-- Check that an identifier or symbol is unqualified.
-- For occasions when doing this in the grammar would cause conflicts.

checkUnQual :: HsQName -> P HsName
checkUnQual (Qual _ _) = fail "Illegal qualified name"
checkUnQual (UnQual n) = return n
checkUnQual (Special _) = fail "Illegal special name"

-----------------------------------------------------------------------------
-- Check that two xml tag names are equal
-- Could use Eq directly, but I am not sure whether <dom:name>...</name> 
-- would be valid, in that case Eq won't work. TODO

checkEqNames :: HsXName -> HsXName -> P HsXName
checkEqNames n@(HsXName n1) (HsXName n2) 
    | n1 == n2  = return n
    | otherwise = fail "names in matching xml tags are not equal"
checkEqNames n@(HsXDomName d1 n1) (HsXDomName d2 n2)
    | n1 == n2 && d1 == d2 = return n
    | otherwise = fail "names in matching xml tags are not equal"
checkEqNames _ _ = fail "names in matching xml tags are not equal"


-----------------------------------------------------------------------------
-- Miscellaneous utilities

checkPrec :: Integer -> P Int
checkPrec i | 0 <= i && i <= 9 = return (fromInteger i)
checkPrec i | otherwise        = fail ("Illegal precedence " ++ show i)

mkRecConstrOrUpdate :: HsExp -> [HsFieldUpdate] -> P HsExp
mkRecConstrOrUpdate (HsCon c) fs       = return (HsRecConstr c fs)
mkRecConstrOrUpdate e         fs@(_:_) = return (HsRecUpdate e fs)
mkRecConstrOrUpdate _         _        = fail "Empty record update"

-----------------------------------------------------------------------------
-- Reverse a list of declarations, merging adjacent HsFunBinds of the
-- same name and checking that their arities match.

checkRevDecls :: [HsDecl] -> P [HsDecl]
checkRevDecls = mergeFunBinds []
    where
    mergeFunBinds revDs [] = return revDs
    mergeFunBinds revDs (HsFunBind ms1@(HsMatch _ name ps _ _:_):ds1) =
        mergeMatches ms1 ds1
        where
        arity = length ps
        mergeMatches ms' (HsFunBind ms@(HsMatch loc name' ps' _ _:_):ds)
            | name' == name =
            if length ps' /= arity
            then fail ("arity mismatch for '" ++ prettyPrint name ++ "'")
                 `atSrcLoc` loc
            else mergeMatches (ms++ms') ds
        mergeMatches ms' ds = mergeFunBinds (HsFunBind ms':revDs) ds
    mergeFunBinds revDs (d:ds) = mergeFunBinds (d:revDs) ds

checkRevClsDecls :: [HsClassDecl] -> P [HsClassDecl]
checkRevClsDecls = mergeClsFunBinds []
    where
    mergeClsFunBinds revDs [] = return revDs
    mergeClsFunBinds revDs (HsClsDecl (HsFunBind ms1@(HsMatch _ name ps _ _:_)):ds1) =
        mergeMatches ms1 ds1
        where
        arity = length ps
        mergeMatches ms' (HsClsDecl (HsFunBind ms@(HsMatch loc name' ps' _ _:_)):ds)
            | name' == name =
            if length ps' /= arity
            then fail ("arity mismatch for '" ++ prettyPrint name ++ "'")
                 `atSrcLoc` loc
            else mergeMatches (ms++ms') ds
        mergeMatches ms' ds = mergeClsFunBinds (HsClsDecl (HsFunBind ms'):revDs) ds
    mergeClsFunBinds revDs (d:ds) = mergeClsFunBinds (d:revDs) ds

checkRevInstDecls :: [HsInstDecl] -> P [HsInstDecl]
checkRevInstDecls = mergeInstFunBinds []
    where
    mergeInstFunBinds revDs [] = return revDs
    mergeInstFunBinds revDs (HsInsDecl (HsFunBind ms1@(HsMatch _ name ps _ _:_)):ds1) =
        mergeMatches ms1 ds1
        where
        arity = length ps
        mergeMatches ms' (HsInsDecl (HsFunBind ms@(HsMatch loc name' ps' _ _:_)):ds)
            | name' == name =
            if length ps' /= arity
            then fail ("arity mismatch for '" ++ prettyPrint name ++ "'")
                 `atSrcLoc` loc
            else mergeMatches (ms++ms') ds
        mergeMatches ms' ds = mergeInstFunBinds (HsInsDecl (HsFunBind ms'):revDs) ds
    mergeInstFunBinds revDs (d:ds) = mergeInstFunBinds (d:revDs) ds

----------------------------------------------------------------
-- Check that newtype declarations have
-- the right number (1) of constructors

checkDataOrNew :: DataOrNew -> [a] -> P ()
checkDataOrNew NewType [x] = return ()
checkDataOrNew DataType _  = return ()
checkDataOrNew _        _  = fail "newtype declaration must have exactly one constructor."

checkSimpleType :: HsType -> P (HsName, [HsName])
checkSimpleType t = checkSimple "test" t []

---------------------------------------
-- Converting a complete page

pageFun :: SrcLoc -> HsExp -> HsDecl
pageFun loc e = HsPatBind loc namePat rhs (HsBDecls [])
    where namePat = HsPVar $ HsIdent "page"
          rhs = HsUnGuardedRhs e

mkPage :: HsModule -> SrcLoc -> HsExp -> P HsModule
mkPage (HsModule src md exps imps decls) loc xml = do
    let page = pageFun loc xml
    return $ HsModule src md exps imps (decls ++ [page])
    
mkPageModule :: HsExp -> P HsModule
mkPageModule xml = do 
    do loc <- case xml of 
           HsXTag l _ _ _ _ -> return l
           HsXETag l _ _ _  -> return l
           _ -> fail "Will not happen since mkPageModule is only called on XML expressions"
       mod <- getModuleName
       return $ (HsModule 
              loc
              (Module mod)
              (Just [HsEVar $ UnQual $ HsIdent "page"])
              []
              [pageFun loc xml])

---------------------------------------
-- Handle dash-identifiers

mkDVar :: [String] -> String
mkDVar = concat . intersperse "-"

mkDVarExpr :: [String] -> HsExp
mkDVarExpr = foldl1 (\x y -> infixApp x (op $ sym "-") y) . map (var . name)

---------------------------------------
-- Combine adjacent for-alls. 
--
-- A valid type must have one for-all at the top of the type, or of the fn arg types

mkHsTyForall :: Maybe [HsTyVarBind] -> HsContext -> HsType -> HsType
mkHsTyForall mtvs []   ty = mk_forall_ty mtvs ty
mkHsTyForall mtvs ctxt ty = HsTyForall mtvs ctxt ty

-- mk_forall_ty makes a pure for-all type (no context)
mk_forall_ty (Just []) ty             = ty  -- Explicit for-all with no tyvars
mk_forall_ty mtvs1     (HsTyForall mtvs2 ctxt ty) = mkHsTyForall (mtvs1 `plus` mtvs2) ctxt ty
mk_forall_ty mtvs1     ty             = HsTyForall mtvs1 [] ty

mtvs1       `plus` Nothing     = mtvs1
Nothing     `plus` mtvs2       = mtvs2 
(Just tvs1) `plus` (Just tvs2) = Just (tvs1 ++ tvs2)

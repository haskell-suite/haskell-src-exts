{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.ParseUtils
-- Copyright   :  (c) Niklas Broberg 2004,
--                (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Utilities for the Haskell-exts parser.
--
-----------------------------------------------------------------------------

module Language.Haskell.Exts.ParseUtils (
      splitTyConApp         -- PType -> P (Name,[Type])
    , checkEnabled          -- (Show e, Enabled e) => e -> P ()
    , checkPatternGuards    -- [Stmt] -> P ()
    , mkRecConstrOrUpdate   -- PExp -> [PFieldUpdate] -> P Exp
    , checkPrec             -- Integer -> P Int
    , checkPContext         -- PType -> P PContext
    , checkContext          -- PContext -> P Context
    , checkAssertion        -- PType -> P PAsst
    , checkDataHeader       -- PType -> P (Context,Name,[TyVarBind])
    , checkClassHeader      -- PType -> P (Context,Name,[TyVarBind])
    , checkInstHeader       -- PType -> P (Context,QName,[Type])
    , checkDeriving         -- [PType] -> P [Deriving]
    , checkPattern          -- PExp -> P Pat
    , checkExpr             -- PExp -> P Exp
    , checkType             -- PType -> P Type
    , checkValDef           -- SrcLoc -> PExp -> Maybe Type -> Rhs -> Binds -> P Decl
    , checkClassBody        -- [ClassDecl] -> P [ClassDecl]
    , checkInstBody         -- [InstDecl] -> P [InstDecl]
    , checkUnQual           -- QName -> P Name
    , checkRevDecls         -- [Decl] -> P [Decl]
    , checkRevClsDecls      -- [ClassDecl] -> P [ClassDecl]
    , checkRevInstDecls     -- [InstDecl] -> P [InstDecl]
    , checkDataOrNew        -- DataOrNew -> [a] -> P ()
    , checkSimpleType       -- PType -> P (Name, [TyVarBind])
    , checkSigVar           -- PExp -> P Name
    , getGConName           -- S.Exp -> P QName
    , mkTyForall            -- Maybe [TyVarBind] -> PContext -> PType -> PType
    -- HaRP
    , checkRPattern         -- PExp -> P RPat
    -- Hsx
    , checkEqNames          -- XName -> XName -> P XName
    , mkPageModule          -- [OptionPragma] -> Exp -> P Module
    , mkPage                -- Module -> SrcLoc -> Exp -> P Module
    , mkDVar                -- [String] -> String
    , mkDVarExpr            -- [String] -> PExp
    -- Pragmas
    , checkRuleExpr         -- PExp -> P Exp
    , readTool              -- Maybe String -> Maybe Tool

    -- Parsed expressions and types
    , PExp(..), PFieldUpdate(..), ParseXAttr(..), PType(..), PContext, PAsst(..)
    , p_unit_con            -- PExp
    , p_tuple_con           -- Boxed -> Int -> PExp
    , p_unboxed_singleton_con   -- PExp
    ) where

import Language.Haskell.Exts.Syntax hiding ( Type(..), Asst(..), Exp(..), FieldUpdate(..), XAttr(..) )
import qualified Language.Haskell.Exts.Syntax as S ( Type(..), Asst(..), Exp(..), FieldUpdate(..), XAttr(..) )
import Language.Haskell.Exts.ParseMonad
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.ExtScheme

import Data.List (intersperse)
import Control.Monad (when)

splitTyConApp :: PType -> P (Name,[S.Type])
splitTyConApp t0 = do
            (n, pts) <- split t0 []
            ts <- mapM checkType pts
            return (n,ts)
 where
    split :: PType -> [PType] -> P (Name,[PType])
    split (TyApp t u) ts = split t (u:ts)
    split (TyCon (UnQual t)) ts = return (t,ts)
    split (TyInfix a op b) ts = split (TyCon op) (a:b:ts)
    split _ _ = fail "Illegal data/newtype declaration"

-----------------------------------------------------------------------------
-- Checking for extensions

checkEnabled :: (Show e, Enabled e) => e  -> P ()
checkEnabled e = do
    exts <- getExtensions
    if isEnabled e exts
     then return ()
     else fail $ show e ++ " is not enabled"

checkPatternGuards :: [Stmt] -> P ()
checkPatternGuards [Qualifier _] = return ()
checkPatternGuards _ = checkEnabled PatternGuards

-----------------------------------------------------------------------------
-- Checking contexts

-- Check that a context is syntactically correct. Takes care of
-- checking for MPTCs, TypeOperators, TypeFamilies (for eq constraints)
-- and ImplicitParameters, but leaves checking of the class assertion
-- parameters for later.
checkPContext :: PType -> P PContext
checkPContext (TyTuple Boxed ts) =
    mapM checkAssertion ts
checkPContext (TyCon (Special UnitCon)) =
    return []
checkPContext (TyParen t) = do
    c <- checkAssertion t
    return [c]
checkPContext t = do
    c <- checkAssertion t
    return [c]

-- Check a single assertion according to the above, still leaving
-- the class assertion parameters for later.
checkAssertion :: PType -> P PAsst
-- We cannot even get here unless ImplicitParameters is enabled.
checkAssertion (TyPred p@(IParam _ _)) = return p
-- We cannot even get here unless TypeFamilies is enabled.
checkAssertion (TyPred p@(EqualP _ _)) = return p
checkAssertion t = checkAssertion' [] t
    where   -- class assertions must have at least one argument
            checkAssertion' ts@(_:xs) (TyCon c) = do
                when (not $ null xs) $ checkEnabled MultiParamTypeClasses
                when (isSymbol c)    $ checkEnabled TypeOperators
                return $ ClassA c ts
            checkAssertion' ts (TyApp a t) = do
                -- no check on t at this stage
                checkAssertion' (t:ts) a
            checkAssertion' ts (TyInfix a op b) =
                -- infix operators require TypeOperators
                checkEnabled TypeOperators >> checkAssertion' (a:b:ts) (TyCon op)
            checkAssertion' ts (TyParen t) =
                checkAssertion' ts t
            checkAssertion' _ _ = fail "Illegal class assertion"

isSymbol :: QName -> Bool
isSymbol (UnQual (Symbol _)) = True
isSymbol (Qual _ (Symbol _)) = True
isSymbol _                   = False


-- Checks simple contexts for class and instance
-- headers. If FlexibleContexts is enabled then
-- anything goes, otherwise only tyvars are allowed.
checkSContext :: PContext -> P Context
checkSContext = mapM (checkAsst True)

-- Checks ordinary contexts for sigtypes and data type
-- declarations. If FlexibleContexts is enabled then
-- anything goes, otherwise only tyvars OR tyvars
-- applied to types are allowed.
checkContext :: PContext -> P Context
checkContext = mapM (checkAsst False)

checkAsst :: Bool -> PAsst -> P S.Asst
checkAsst isSimple asst =
    case asst of
      ClassA qn pts -> do
                ts <- mapM (checkAsstParam isSimple) pts
                return $ S.ClassA qn ts
      InfixA a op b -> do
                [a,b] <- mapM (checkAsstParam isSimple) [a,b]
                return $ S.InfixA a op b
      IParam ipn pt -> do
                t <- checkType pt
                return $ S.IParam ipn t
      EqualP pa pb  -> do
                a <- checkType pa
                b <- checkType pb
                return $ S.EqualP a b

checkAsstParam :: Bool -> PType -> P S.Type
checkAsstParam isSimple t = do
        exts <- getExtensions
        if FlexibleContexts `elem` exts
         then checkType t
         else case t of
                TyVar n     -> return $ S.TyVar n
                TyApp pf pt | not isSimple    -> do
                        f <- checkAsstParam isSimple pf
                        t <- checkType pt
                        return $ S.TyApp f t
                _       -> fail "Malformed context: FlexibleContexts not enabled"

-----------------------------------------------------------------------------
-- Checking Headers


checkDataHeader :: PType -> P (Context,Name,[TyVarBind])
checkDataHeader (TyForall Nothing cs t) = do
    (c,ts) <- checkSimple "data/newtype" t []
    cs <- checkContext cs
    return (cs,c,ts)
checkDataHeader t = do
    (c,ts) <- checkSimple "data/newtype" t []
    return ([],c,ts)

checkClassHeader :: PType -> P (Context,Name,[TyVarBind])
checkClassHeader (TyForall Nothing cs t) = do
    (c,ts) <- checkSimple "class" t []
    cs <- checkSContext cs
    return (cs,c,ts)
checkClassHeader t = do
    (c,ts) <- checkSimple "class" t []
    return ([],c,ts)

checkSimple :: String -> PType -> [TyVarBind] -> P (Name,[TyVarBind])
checkSimple kw (TyApp l t) xs | isTyVarBind t = checkSimple kw l (toTyVarBind t : xs)
checkSimple _  (TyInfix t1 (UnQual t) t2) xs
    | isTyVarBind t1 && isTyVarBind t2 =
       checkEnabled TypeOperators >> return (t, toTyVarBind t1 : toTyVarBind t2 : xs)
checkSimple _kw (TyCon (UnQual t))   xs = do
    case t of
      Symbol _ -> checkEnabled TypeOperators
      _ -> return ()
    return (t,xs)
checkSimple kw _ _ = fail ("Illegal " ++ kw ++ " declaration")

isTyVarBind :: PType -> Bool
isTyVarBind (TyVar _) = True
isTyVarBind (TyKind (TyVar _) _) = True
isTyVarBind _ = False

toTyVarBind :: PType -> TyVarBind
toTyVarBind (TyVar n) = UnkindedVar n
toTyVarBind (TyKind (TyVar n) k) = KindedVar n k

checkInstHeader :: PType -> P (Context,QName,[S.Type])
checkInstHeader (TyForall Nothing cs t) = do
    (c,ts) <- checkInsts t []
    cs <- checkSContext cs
    return (cs,c,ts)
checkInstHeader t = do
    (c,ts) <- checkInsts t []
    return ([],c,ts)


checkInsts :: PType -> [PType] -> P (QName,[S.Type])
checkInsts (TyApp l t) ts = checkInsts l (t:ts)
checkInsts (TyCon c)   ts = do
    when (isSymbol c) $ checkEnabled TypeOperators
    ts <- checkTypes ts
    return (c,ts)
checkInsts (TyInfix a op b) [] = do
    checkEnabled TypeOperators
    ts <- checkTypes [a,b]
    return (op,ts)
checkInsts (TyParen t) [] = checkInsts t []
checkInsts _ _ = fail "Illegal instance declaration"

checkDeriving :: [PType] -> P [Deriving]
checkDeriving = mapM (flip checkInsts [])

-----------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: PExp -> P Pat
checkPattern e = checkPat e []

checkPat :: PExp -> [Pat] -> P Pat
checkPat (Con c) args = return (PApp c args)
checkPat (App f x) args = do
    x <- checkPat x []
    checkPat f (x:args)
checkPat (InfixApp l op r) args
    | op == (QVarOp (UnQual (Symbol "!"))) = do
        -- We must have BangPatterns on
        checkEnabled BangPatterns
        let (e,es) = splitBang r []
        ps <- mapM checkPattern (BangPat e:es)
        checkPat l (ps++args)
checkPat e [] = case e of
    Var (UnQual x)   -> return (PVar x)
    Lit l            -> return (PLit l)
    InfixApp l op r  ->
        case op of
            QConOp c -> do
                    l <- checkPat l []
                    r <- checkPat r []
                    return (PInfixApp l c r)
            QVarOp (UnQual (Symbol "+")) -> do
                    case (l,r) of
                        (Var (UnQual n@(Ident _)), Lit (Int k)) -> return (PNPlusK n k)
                        _ -> patFail ""
{-            QVarOp (UnQual (Symbol "!")) -> do
                    -- We must have BangPatterns on
                    checkEnabled BangPatterns
                    let (e,es) = splitBang r []
                    ps <- mapM checkPattern (BangPat e:es)
                    checkPat l ps -}
            _ -> patFail ""
    Tuple es         -> do
                  ps <- mapM (\e -> checkPat e []) es
                  return (PTuple ps)
    List es      -> do
                  ps <- mapM checkRPattern es
                  if all isStdPat ps
                    then return . PList $ map stripRP ps
                    -- we don't allow truly regular patterns unless the extension is enabled
                    else checkEnabled RegularPatterns >> return (PRPat $ map fixRPOpPrec ps)
            where isStdPat :: RPat -> Bool
                  isStdPat (RPPat _) = True
                  isStdPat (RPAs _ p) = isStdPat p
                  isStdPat (RPParen p) = isStdPat p
                  isStdPat _           = False
                  stripRP :: RPat -> Pat
                  stripRP (RPPat  p) = p
                  stripRP (RPAs n p) = PAsPat n (stripRP p)
                  stripRP (RPParen p) = PParen (stripRP p)
                  stripRP _           = error "cannot strip RP wrapper if not all patterns are base"

    Paren e      -> do
                  p <- checkPat e []
                  return (PParen p)
    AsPat n e    -> do
                  p <- checkPat e []
                  return (PAsPat n p)
    WildCard     -> return PWildCard
    IrrPat e     -> do
                  p <- checkPat e []
                  return (PIrrPat p)
    ViewPat e p  -> do
                  e <- checkExpr e
                  p <- checkPat p []
                  return (PViewPat e p)
    RecConstr c fs   -> do
                  fs <- mapM checkPatField fs
                  return (PRec c fs)
    NegApp (Lit l) -> return (PNeg (PLit l))
    ExpTypeSig s e t -> do
                  -- patterns cannot have signatures unless ScopedTypeVariables is enabled.
                  checkEnabled ScopedTypeVariables
                  p <- checkPat e []
                  return (PatTypeSig s p t)

    -- Hsx
    XTag s n attrs mattr cs -> do
                  pattrs <- mapM checkPAttr attrs
                  pcs    <- mapM (\c -> checkPat c []) cs
                  mpattr <- maybe (return Nothing)
                              (\e -> do p <- checkPat e []
                                        return $ Just p)
                              mattr
                  let cps = mkChildrenPat pcs
                  return $ PXTag s n pattrs mpattr cps
    XETag s n attrs mattr -> do
                  pattrs <- mapM checkPAttr attrs
                  mpattr <- maybe (return Nothing)
                              (\e -> do p <- checkPat e []
                                        return $ Just p)
                              mattr
                  return $ PXETag s n pattrs mpattr
    XPcdata pcdata   -> return $ PXPcdata pcdata
    XExpTag e -> do
            p <- checkPat e []
            return $ PXPatTag p
    XRPats es -> do
            rps <- mapM checkRPattern es
            return (PXRPats $ map fixRPOpPrec rps)

    -- Generics
    ExplTypeArg qn t -> return $ PExplTypeArg qn t

    -- QuasiQuotation
    QuasiQuote n q -> return $ PQuasiQuote n q

    -- BangPatterns
    BangPat e -> do
        p <- checkPat e []
        return $ PBangPat p

    PreOp (QVarOp (UnQual (Symbol "!"))) e -> do
        checkEnabled BangPatterns
        p <- checkPat e []
        return $ PBangPat p

    e -> patFail $ show e

checkPat e _ = patFail $ show e

splitBang :: PExp -> [PExp] -> (PExp, [PExp])
splitBang (App f x) es = splitBang f (x:es)
splitBang e es = (e, es)

checkPatField :: PFieldUpdate -> P PatField
checkPatField (FieldUpdate n e) = do
    p <- checkPat e []
    return (PFieldPat n p)
checkPatField (FieldPun n) = return (PFieldPun n)
checkPatField (FieldWildcard) = return PFieldWildcard

checkPAttr :: ParseXAttr -> P PXAttr
checkPAttr (XAttr n v) = do p <- checkPat v []
                            return $ PXAttr n p

patFail :: String -> P a
patFail s = fail $ "Parse error in pattern: " ++ s

checkRPattern :: PExp -> P RPat
checkRPattern e = case e of
    SeqRP es -> do
        rps <- mapM checkRPattern es
        return $ RPSeq rps
    PostOp e op -> do
        rpop <- checkRPatOp op
        rp   <- checkRPattern e
        return $ RPOp rp rpop
    GuardRP e gs -> do
        rp <- checkPattern e
        return $ RPGuard rp gs
    EitherRP e1 e2 -> do
        rp1 <- checkRPattern e1
        rp2 <- checkRPattern e2
        return $ RPEither rp1 rp2
    CAsRP n e -> do
        rp <- checkRPattern e
        return $ RPCAs n rp
    AsPat n e  -> do
        rp <- checkRPattern e
        return $ RPAs n rp
    Paren e -> do
        rp <- checkRPattern e
        return $ RPParen rp
    _          -> do
        p <- checkPattern e
        return $ RPPat p

checkRPatOp :: QOp -> P RPatOp
checkRPatOp o@(QVarOp (UnQual (Symbol sym))) =
    case sym of
     "*"  -> return RPStar
     "*!" -> return RPStarG
     "+"  -> return RPPlus
     "+!" -> return RPPlusG
     "?"  -> return RPOpt
     "?!" -> return RPOptG
     _    -> rpOpFail o
checkRPatOp o = rpOpFail o

rpOpFail sym = fail $ "Unrecognized regular pattern operator: " ++ show sym

fixRPOpPrec :: RPat -> RPat
fixRPOpPrec rp = case rp of
    RPOp rp rpop      -> fPrecOp rp (flip RPOp rpop)
    RPEither rp1 rp2  -> RPEither (fixRPOpPrec rp1) (fixRPOpPrec rp2)
    RPSeq rps         -> RPSeq $ map fixRPOpPrec rps
    RPCAs n rp        -> RPCAs n $ fixRPOpPrec rp
    RPAs n rp         -> RPAs n $ fixRPOpPrec rp
    RPParen rp        -> RPParen $ fixRPOpPrec rp
    _                   -> rp

  where fPrecOp :: RPat -> (RPat -> RPat) -> RPat
        fPrecOp (RPOp rp rpop) f = fPrecOp rp (f . flip RPOp rpop)
        fPrecOp (RPCAs n rp) f = fPrecAs rp f (RPCAs n)
        fPrecOp (RPAs  n rp) f = fPrecAs rp f (RPAs  n)
        fPrecOp rp f = f $ fixRPOpPrec rp
        fPrecAs :: RPat -> (RPat -> RPat) -> (RPat -> RPat) -> RPat
        fPrecAs (RPCAs n rp) f g = fPrecAs rp f (g . RPCAs n)
        fPrecAs (RPAs  n rp) f g = fPrecAs rp f (g . RPAs  n)
        fPrecAs rp f g = g . f $ fixRPOpPrec rp


mkChildrenPat :: [Pat] -> [Pat]
mkChildrenPat ps = mkCPAux ps []
  where mkCPAux :: [Pat] -> [Pat] -> [Pat]
        mkCPAux [] qs = reverse qs
        mkCPAux (p:ps) qs = case p of
            (PRPat rps) -> [mkCRP ps (reverse rps ++ map RPPat qs)]
            _             -> mkCPAux ps (p:qs)

        mkCRP :: [Pat] -> [RPat] -> Pat
        mkCRP [] rps = PXRPats $ reverse rps
        mkCRP (p:ps) rps = case p of
            (PXRPats rqs) -> mkCRP ps (reverse rqs ++ rps)
            _               -> mkCRP ps (RPPat p : rps)

-----------------------------------------------------------------------------
-- Check Expression Syntax

checkExpr :: PExp -> P S.Exp
checkExpr e = case e of
    Var v               -> return $ S.Var v
    IPVar v             -> return $ S.IPVar v
    Con c               -> return $ S.Con c
    Lit l               -> return $ S.Lit l
    InfixApp e1 op e2   -> check2Exprs e1 e2 (flip S.InfixApp op)
    App e1 e2           -> check2Exprs e1 e2 S.App
    NegApp e            -> check1Expr e S.NegApp
    Lambda loc ps e     -> check1Expr e (S.Lambda loc ps)
    Let bs e            -> check1Expr e (S.Let bs)
    If e1 e2 e3         -> check3Exprs e1 e2 e3 S.If
    Case e alts         -> do
                     e <- checkExpr e
                     return (S.Case e alts)
    Do stmts        -> return (S.Do stmts)
    MDo stmts       -> return (S.MDo stmts)
    Tuple es        -> checkManyExprs es S.Tuple
    List es         -> checkManyExprs es S.List
    -- Since we don't parse things as left sections, we need to mangle them into that.
    Paren e         -> case e of
                          PostOp e1 op -> check1Expr e1 (flip S.LeftSection op)
                          PreOp  op e2 -> check1Expr e2 (S.RightSection op)
                          _            -> check1Expr e S.Paren
--    RightSection op e   -> check1Expr e (S.RightSection op)
    RecConstr c fields      -> do
                     fields <- mapM checkField fields
                     return (S.RecConstr c fields)
    RecUpdate e fields      -> do
                     fields <- mapM checkField fields
                     e <- checkExpr e
                     return (S.RecUpdate e fields)
    EnumFrom e          -> check1Expr e S.EnumFrom
    EnumFromTo e1 e2    -> check2Exprs e1 e2 S.EnumFromTo
    EnumFromThen e1 e2      -> check2Exprs e1 e2 S.EnumFromThen
    EnumFromThenTo e1 e2 e3 -> check3Exprs e1 e2 e3 S.EnumFromThenTo
    -- a parallel list comprehension, which could be just a simple one
    ParComp e qualss        -> do
                     e <- checkExpr e
                     case qualss of
                      [quals] -> return (S.ListComp e quals)
                      _       -> return (S.ParComp e qualss)
    ExpTypeSig loc e ty     -> do
                     e <- checkExpr e
                     return (S.ExpTypeSig loc e ty)

    --Template Haskell
    BracketExp e        -> return $ S.BracketExp e
    SpliceExp e         -> return $ S.SpliceExp e
    TypQuote q          -> return $ S.TypQuote q
    VarQuote q          -> return $ S.VarQuote q
    QuasiQuote n q      -> return $ S.QuasiQuote n q

    -- Hsx
    XTag s n attrs mattr cs -> do attrs <- mapM checkAttr attrs
                                  cs <- mapM checkExpr cs
                                  mattr <- maybe (return Nothing)
                                              (\e -> checkExpr e >>= return . Just)
                                              mattr
                                  return $ S.XTag s n attrs mattr cs
    XETag s n attrs mattr   -> do attrs <- mapM checkAttr attrs
                                  mattr <- maybe (return Nothing)
                                              (\e -> checkExpr e >>= return . Just)
                                              mattr
                                  return $ S.XETag s n attrs mattr
    XPcdata p       -> return $ S.XPcdata p
    XExpTag e       -> do e <- checkExpr e
                          return $ S.XExpTag e
    -- Pragmas
    CorePragma s e  -> check1Expr e (S.CorePragma s)
    SCCPragma  s e  -> check1Expr e (S.SCCPragma s)
    GenPragma s xx yy e -> check1Expr e (S.GenPragma s xx yy)
--    UnknownExpPragma n s -> return $ S.UnknownExpPragma n s

    -- Arrows
    Proc p e        -> do e <- checkExpr e
                          return $ S.Proc p e
    LeftArrApp e1 e2      -> check2Exprs e1 e2 S.LeftArrApp
    RightArrApp e1 e2     -> check2Exprs e1 e2 S.RightArrApp
    LeftArrHighApp e1 e2  -> check2Exprs e1 e2 S.LeftArrHighApp
    RightArrHighApp e1 e2 -> check2Exprs e1 e2 S.RightArrHighApp

    _             -> fail $ "Parse error in expression: " ++ show e

checkAttr :: ParseXAttr -> P S.XAttr
checkAttr (XAttr n v) = do v <- checkExpr v
                           return $ S.XAttr n v

-- type signature for polymorphic recursion!!
check1Expr :: PExp -> (S.Exp -> a) -> P a
check1Expr e1 f = do
    e1 <- checkExpr e1
    return (f e1)

check2Exprs :: PExp -> PExp -> (S.Exp -> S.Exp -> a) -> P a
check2Exprs e1 e2 f = do
    e1 <- checkExpr e1
    e2 <- checkExpr e2
    return (f e1 e2)

check3Exprs :: PExp -> PExp -> PExp -> (S.Exp -> S.Exp -> S.Exp -> a) -> P a
check3Exprs e1 e2 e3 f = do
    e1 <- checkExpr e1
    e2 <- checkExpr e2
    e3 <- checkExpr e3
    return (f e1 e2 e3)

checkManyExprs :: [PExp] -> ([S.Exp] -> a) -> P a
checkManyExprs es f = do
    es <- mapM checkExpr es
    return (f es)


checkRuleExpr :: PExp -> P S.Exp
checkRuleExpr = checkExpr

readTool :: Maybe String -> Maybe Tool
readTool = fmap readC
 where readC str = case str of
        "GHC" -> GHC
        "HUGS" -> HUGS
        "NHC98" -> NHC98
        "YHC" -> YHC
        "HADDOCK" -> HADDOCK
        _ -> UnknownTool str

{-
checkAlt :: Alt -> P Alt
checkAlt (Alt loc p galts bs) = do
    galts <- checkGAlts galts
    return (Alt loc p galts bs)

checkGAlts :: GuardedAlts -> P GuardedAlts
checkGAlts (UnGuardedAlt e) = check1Expr e UnGuardedAlt
checkGAlts (GuardedAlts galts) = do
    galts <- mapM checkGAlt galts
    return (GuardedAlts galts)

checkGAlt :: GuardedAlt -> P GuardedAlt
checkGAlt (GuardedAlt loc g e) = check1Expr e (GuardedAlt loc g)

checkStmt :: Stmt -> P Stmt
checkStmt (Generator loc p e) = check1Expr e (Generator loc p)
checkStmt (Qualifier e) = check1Expr e Qualifier
checkStmt s@(LetStmt _) = return s
-}
checkField :: PFieldUpdate -> P S.FieldUpdate
checkField (FieldUpdate n e) = check1Expr e (S.FieldUpdate n)
checkField (FieldPun n) = return $ S.FieldPun n
checkField (FieldWildcard) = return S.FieldWildcard

getGConName :: S.Exp -> P QName
getGConName (S.Con n) = return n
getGConName (S.List []) = return list_cons_name
getGConName _ = fail "Expression in reification is not a name"

-----------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef :: SrcLoc -> PExp -> Maybe S.Type -> Rhs -> Binds -> P Decl
checkValDef srcloc lhs optsig rhs whereBinds = do
    mlhs <- isFunLhs lhs []
    case mlhs of
     Just (f,es) -> do
            ps <- mapM checkPattern es
            case optsig of -- only pattern bindings can have signatures
                Nothing -> return (FunBind [Match srcloc f ps optsig rhs whereBinds])
                Just _  -> fail "Cannot give an explicit type signature to a function binding"
     Nothing     -> do
            lhs <- checkPattern lhs
            return (PatBind srcloc lhs optsig rhs whereBinds)

-- A variable binding is parsed as an PatBind.

isFunLhs :: PExp -> [PExp] -> P (Maybe (Name, [PExp]))
isFunLhs (InfixApp l (QVarOp (UnQual op)) r) es
    | op == (Symbol "!") = do
        exts <- getExtensions
        if BangPatterns `elem` exts
         then let (b,bs) = splitBang r []
               in isFunLhs l (BangPat b : bs ++ es)
         else return $ Just (op, l:r:es) -- It's actually a definition of the operator !
    | otherwise = return $ Just (op, l:r:es)
isFunLhs (App (Var (UnQual f)) e) es = return $ Just (f, e:es)
--isFunLhs (App (Paren f) e) es = isFunLhs f (e:es)
isFunLhs (App f e) es = isFunLhs f (e:es)
isFunLhs (Var (UnQual f)) es@(_:_) = return $ Just (f, es)
isFunLhs (Paren f) es@(_:_) = isFunLhs f es
isFunLhs _ _ = return Nothing

-- Separating between signature declarations and value definitions in
-- a post-processing step

checkSigVar :: PExp -> P Name
checkSigVar (Var (UnQual n)) = return n
checkSigVar e = fail $ "Left-hand side of type signature is not a variable: " ++ show e

-----------------------------------------------------------------------------
-- In a class or instance body, a pattern binding must be of a variable.

checkClassBody :: [ClassDecl] -> P [ClassDecl]
checkClassBody decls = do
    mapM_ checkClassMethodDef decls
    return decls
  where checkClassMethodDef (ClsDecl decl) = checkMethodDef decl
        checkClassMethodDef _ = return ()

checkInstBody :: [InstDecl] -> P [InstDecl]
checkInstBody decls = do
    mapM_ checkInstMethodDef decls
    return decls
  where checkInstMethodDef (InsDecl decl) = checkMethodDef decl
        checkInstMethodDef _ = return ()

checkMethodDef :: Decl -> P ()
checkMethodDef (PatBind _ (PVar _) _ _ _) = return ()
checkMethodDef (PatBind loc _ _ _ _) =
    fail "illegal method definition" `atSrcLoc` loc
checkMethodDef _ = return ()

-----------------------------------------------------------------------------
-- Check that an identifier or symbol is unqualified.
-- For occasions when doing this in the grammar would cause conflicts.

checkUnQual :: QName -> P Name
checkUnQual (Qual _ _) = fail "Illegal qualified name"
checkUnQual (UnQual n) = return n
checkUnQual (Special _) = fail "Illegal special name"

-----------------------------------------------------------------------------
-- Check that two xml tag names are equal
-- Could use Eq directly, but I am not sure whether <dom:name>...</name>
-- would be valid, in that case Eq won't work. TODO

checkEqNames :: XName -> XName -> P XName
checkEqNames n@(XName n1) (XName n2)
    | n1 == n2  = return n
    | otherwise = fail "names in matching xml tags are not equal"
checkEqNames n@(XDomName d1 n1) (XDomName d2 n2)
    | n1 == n2 && d1 == d2 = return n
    | otherwise = fail "names in matching xml tags are not equal"
checkEqNames _ _ = fail "names in matching xml tags are not equal"


-----------------------------------------------------------------------------
-- Miscellaneous utilities

checkPrec :: Integer -> P Int
checkPrec i | 0 <= i && i <= 9 = return (fromInteger i)
checkPrec i | otherwise        = fail ("Illegal precedence " ++ show i)

mkRecConstrOrUpdate :: PExp -> [PFieldUpdate] -> P PExp
mkRecConstrOrUpdate (Con c) fs       = return (RecConstr c fs)
mkRecConstrOrUpdate e       fs@(_:_) = return (RecUpdate e fs)
mkRecConstrOrUpdate _       _        = fail "Empty record update"

-----------------------------------------------------------------------------
-- Reverse a list of declarations, merging adjacent FunBinds of the
-- same name and checking that their arities match.

checkRevDecls :: [Decl] -> P [Decl]
checkRevDecls = mergeFunBinds []
    where
    mergeFunBinds revDs [] = return revDs
    mergeFunBinds revDs (FunBind ms1@(Match _ name ps _ _ _:_):ds1) =
        mergeMatches ms1 ds1
        where
        arity = length ps
        mergeMatches ms' (FunBind ms@(Match loc name' ps' _ _ _:_):ds)
            | name' == name =
            if length ps' /= arity
            then fail ("arity mismatch for '" ++ prettyPrint name ++ "'")
                 `atSrcLoc` loc
            else mergeMatches (ms++ms') ds
        mergeMatches ms' ds = mergeFunBinds (FunBind ms':revDs) ds
    mergeFunBinds revDs (d:ds) = mergeFunBinds (d:revDs) ds

checkRevClsDecls :: [ClassDecl] -> P [ClassDecl]
checkRevClsDecls = mergeClsFunBinds []
    where
    mergeClsFunBinds revDs [] = return revDs
    mergeClsFunBinds revDs (ClsDecl (FunBind ms1@(Match _ name ps _ _ _:_)):ds1) =
        mergeMatches ms1 ds1
        where
        arity = length ps
        mergeMatches ms' (ClsDecl (FunBind ms@(Match loc name' ps' _ _ _:_)):ds)
            | name' == name =
            if length ps' /= arity
            then fail ("arity mismatch for '" ++ prettyPrint name ++ "'")
                 `atSrcLoc` loc
            else mergeMatches (ms++ms') ds
        mergeMatches ms' ds = mergeClsFunBinds (ClsDecl (FunBind ms'):revDs) ds
    mergeClsFunBinds revDs (d:ds) = mergeClsFunBinds (d:revDs) ds

checkRevInstDecls :: [InstDecl] -> P [InstDecl]
checkRevInstDecls = mergeInstFunBinds []
    where
    mergeInstFunBinds revDs [] = return revDs
    mergeInstFunBinds revDs (InsDecl (FunBind ms1@(Match _ name ps _ _ _:_)):ds1) =
        mergeMatches ms1 ds1
        where
        arity = length ps
        mergeMatches ms' (InsDecl (FunBind ms@(Match loc name' ps' _ _ _:_)):ds)
            | name' == name =
            if length ps' /= arity
            then fail ("arity mismatch for '" ++ prettyPrint name ++ "'")
                 `atSrcLoc` loc
            else mergeMatches (ms++ms') ds
        mergeMatches ms' ds = mergeInstFunBinds (InsDecl (FunBind ms'):revDs) ds
    mergeInstFunBinds revDs (d:ds) = mergeInstFunBinds (d:revDs) ds

----------------------------------------------------------------
-- Check that newtype declarations have
-- the right number (1) of constructors

checkDataOrNew :: DataOrNew -> [a] -> P ()
checkDataOrNew NewType [x] = return ()
checkDataOrNew DataType _  = return ()
checkDataOrNew _        _  = fail "newtype declaration must have exactly one constructor."

checkSimpleType :: PType -> P (Name, [TyVarBind])
checkSimpleType t = checkSimple "test" t []

---------------------------------------
-- Check actual types

checkType :: PType -> P S.Type
checkType t = checkT t False

checkT :: PType -> Bool -> P S.Type
checkT t simple = case t of
    TyForall tvs@Nothing cs pt    -> do
            when (simple) $ checkEnabled (Any [Rank2Types, RankNTypes, PolymorphicComponents, LiberalTypeSynonyms, ScopedTypeVariables])
            ctxt <- checkContext cs
            check1Type pt (S.TyForall Nothing ctxt)
    TyForall tvs cs pt -> do
            checkEnabled (Any [Rank2Types, RankNTypes, PolymorphicComponents, LiberalTypeSynonyms, ScopedTypeVariables])
            ctxt <- checkContext cs
            check1Type pt (S.TyForall tvs ctxt)
    TyFun   at rt   -> check2Types at rt S.TyFun
    TyTuple b pts   -> checkTypes pts >>= return . S.TyTuple b
    TyList  pt      -> check1Type pt S.TyList
    TyApp   ft at   -> check2Types ft at S.TyApp
    TyVar   n       -> return $ S.TyVar n
    TyCon   n       -> do
            when (isSymbol n) $ checkEnabled TypeOperators
            return $ S.TyCon n
    TyParen pt      -> check1Type pt S.TyParen
    -- TyPred  cannot be a valid type
    -- Here we know that t will be used as an actual type (and not a data constructor)
    -- so we can check that TypeOperators are enabled.
    TyInfix at op bt -> checkEnabled TypeOperators >> check2Types at bt (flip S.TyInfix op)
    TyKind  pt k    -> check1Type pt (flip S.TyKind k)

check1Type :: PType -> (S.Type -> S.Type) -> P S.Type
check1Type pt f = checkT pt True >>= return . f

check2Types :: PType -> PType -> (S.Type -> S.Type -> S.Type) -> P S.Type
check2Types at bt f = checkT at True >>= \a -> checkT bt True >>= \b -> return (f a b)

checkTypes :: [PType] -> P [S.Type]
checkTypes = mapM (flip checkT True)

---------------------------------------
-- Converting a complete page

pageFun :: SrcLoc -> S.Exp -> Decl
pageFun loc e = PatBind loc namePat Nothing rhs (BDecls [])
    where namePat = PVar $ Ident "page"
          rhs = UnGuardedRhs e

mkPage :: Module -> SrcLoc -> S.Exp -> P Module
mkPage (Module src md os warn exps imps decls) loc xml = do
    let page = pageFun loc xml
    return $ Module src md os warn exps imps (decls ++ [page])

mkPageModule :: [OptionPragma] -> S.Exp -> P Module
mkPageModule os xml = do
    do loc <- case xml of
           S.XTag l _ _ _ _ -> return l
           S.XETag l _ _ _  -> return l
           _ -> fail "Will not happen since mkPageModule is only called on XML expressions"
       mod <- getModuleName
       return $ (Module
              loc
              (ModuleName mod)
              os
              Nothing
              (Just [EVar $ UnQual $ Ident "page"])
              []
              [pageFun loc xml])

---------------------------------------
-- Handle dash-identifiers

mkDVar :: [String] -> String
mkDVar = concat . intersperse "-"

mkDVarExpr :: [String] -> PExp
mkDVarExpr = foldl1 (\x y -> InfixApp x (op $ sym "-") y) . map (Var . UnQual . name)

---------------------------------------
-- Combine adjacent for-alls.
--
-- A valid type must have one for-all at the top of the type, or of the fn arg types

mkTyForall :: Maybe [TyVarBind] -> PContext -> PType -> PType
mkTyForall mtvs []   ty = mk_forall_ty mtvs ty
mkTyForall mtvs ctxt ty = TyForall mtvs ctxt ty

-- mk_forall_ty makes a pure for-all type (no context)
mk_forall_ty (Just []) ty             = ty  -- Explicit for-all with no tyvars
mk_forall_ty mtvs1     (TyForall mtvs2 ctxt ty) = mkTyForall (mtvs1 `plus` mtvs2) ctxt ty
mk_forall_ty mtvs1     ty             = TyForall mtvs1 [] ty

mtvs1       `plus` Nothing     = mtvs1
Nothing     `plus` mtvs2       = mtvs2
(Just tvs1) `plus` (Just tvs2) = Just (tvs1 ++ tvs2)

---------------------------------------
-- Expressions as we parse them (and patters, and regular patterns)

data PExp
    = Var QName                 -- ^ variable
    | IPVar IPName              -- ^ implicit parameter variable
    | Con QName                 -- ^ data constructor
    | Lit Literal               -- ^ literal constant
    | InfixApp PExp QOp PExp    -- ^ infix application
    | App PExp PExp             -- ^ ordinary application
    | NegApp PExp               -- ^ negation expression @-@ /exp/
    | Lambda SrcLoc [Pat] PExp  -- ^ lambda expression
    | Let Binds PExp            -- ^ local declarations with @let@
    | If PExp PExp PExp         -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    | Case PExp [Alt]           -- ^ @case@ /exp/ @of@ /alts/
    | Do [Stmt]                 -- ^ @do@-expression:
                                    -- the last statement in the list
                                    -- should be an expression.
    | MDo [Stmt]                -- ^ @mdo@-expression
    | Tuple [PExp]              -- ^ tuple expression
    | List [PExp]               -- ^ list expression
    | Paren PExp                -- ^ parenthesized expression
--     RightSection QOp PExp     -- ^ right section @(@/qop/ /exp/@)@
    | RecConstr QName [PFieldUpdate]
                                -- ^ record construction expression
    | RecUpdate PExp [PFieldUpdate]
                                -- ^ record update expression
    | EnumFrom PExp             -- ^ unbounded arithmetic sequence,
                                    -- incrementing by 1
    | EnumFromTo PExp PExp      -- ^ bounded arithmetic sequence,
                                    -- incrementing by 1
    | EnumFromThen PExp PExp    -- ^ unbounded arithmetic sequence,
                                    -- with first two elements given
    | EnumFromThenTo PExp PExp PExp
                                -- ^ bounded arithmetic sequence,
                                    -- with first two elements given
    | ParComp  PExp [[QualStmt]]    -- ^ parallel list comprehension
    | ExpTypeSig SrcLoc PExp S.Type
                                -- ^ expression type signature
    | AsPat Name PExp           -- ^ patterns only
    | WildCard                  -- ^ patterns only
    | IrrPat PExp               -- ^ patterns only

-- Post-ops for parsing left sections and regular patterns. Not to be left in the final tree.
    | PostOp PExp QOp           -- ^ post-ops
    | PreOp QOp PExp            -- ^ pre-ops

-- View patterns
    | ViewPat PExp PExp         -- ^ patterns only

-- HaRP
    | SeqRP [PExp]              -- ^ regular patterns only
    | GuardRP PExp [Stmt]       -- ^ regular patterns only
    | EitherRP PExp PExp        -- ^ regular patterns only
    | CAsRP Name PExp           -- ^ regular patterns only

-- Template Haskell
    | VarQuote QName            -- ^ 'x
    | TypQuote QName            -- ^ ''T
    | BracketExp Bracket
    | SpliceExp Splice
    | QuasiQuote String String  -- ^ [$...|...]

-- Hsx
    | XTag SrcLoc XName [ParseXAttr] (Maybe PExp) [PExp]
    | XETag SrcLoc XName [ParseXAttr] (Maybe PExp)
    | XPcdata String
    | XExpTag PExp
    | XRPats [PExp]

-- Pragmas
    | CorePragma        String  PExp
    | SCCPragma         String  PExp
    | GenPragma         String (Int, Int) (Int, Int) PExp
--    | UnknownExpPragma  String String

-- Generics
    | ExplTypeArg QName S.Type    -- ^ f {| Int |} x = ...

-- Bang Patterns
    | BangPat PExp              -- ^ f !a = ...

-- Arrows
    | Proc Pat PExp
    | LeftArrApp      PExp PExp
    | RightArrApp     PExp PExp
    | LeftArrHighApp  PExp PExp
    | RightArrHighApp PExp PExp
  deriving (Eq,Show)

data PFieldUpdate
    = FieldUpdate QName PExp
    | FieldPun Name
    | FieldWildcard
  deriving (Eq,Show)

data ParseXAttr = XAttr XName PExp
  deriving (Eq,Show)

p_unit_con :: PExp
p_unit_con          = Con unit_con_name

p_tuple_con :: Boxed -> Int -> PExp
p_tuple_con b i       = Con (tuple_con_name b i)

p_unboxed_singleton_con :: PExp
p_unboxed_singleton_con = Con unboxed_singleton_con_name

type PContext = [PAsst]

data PType
     = TyForall
        (Maybe [TyVarBind])
        PContext
        PType
     | TyFun   PType PType        -- ^ function type
     | TyTuple Boxed [PType]      -- ^ tuple type, possibly boxed
     | TyList  PType              -- ^ list syntax, e.g. [a], as opposed to [] a
     | TyApp   PType PType        -- ^ application of a type constructor
     | TyVar   Name               -- ^ type variable
     | TyCon   QName              -- ^ named type or type constructor
     | TyParen PType              -- ^ type surrounded by parentheses
     | TyPred  PAsst              -- ^ assertion of an implicit parameter
     | TyInfix PType QName PType  -- ^ infix type constructor
     | TyKind  PType Kind         -- ^ type with explicit kind signature
  deriving (Eq, Show)

data PAsst = ClassA QName [PType]
           | InfixA PType QName PType
           | IParam IPName  PType
           | EqualP PType   PType
  deriving (Eq, Show)

unit_tycon, fun_tycon, list_tycon, unboxed_singleton_tycon :: PType
unit_tycon        = TyCon unit_tycon_name
fun_tycon         = TyCon fun_tycon_name
list_tycon        = TyCon list_tycon_name
unboxed_singleton_tycon = TyCon unboxed_singleton_tycon_name

tuple_tycon :: Boxed -> Int -> PType
tuple_tycon b i         = TyCon (tuple_tycon_name b i)

{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Annotated.ParseUtils
-- Copyright   :  (c) Niklas Broberg 2004-2009,
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

module Language.Haskell.Exts.Annotated.ParseUtils (
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
    , checkPageModule
    , checkHybridModule
    , mkDVar                -- [String] -> String
    -- Pragmas
    , checkRuleExpr         -- PExp -> P Exp
    , readTool              -- Maybe String -> Maybe Tool

    -- Parsed expressions and types
    , PExp(..), PFieldUpdate(..), ParseXAttr(..), PType(..), PContext, PAsst(..)
    , p_unit_con            -- PExp
    , p_tuple_con           -- Boxed -> Int -> PExp
    , p_unboxed_singleton_con   -- PExp
    ) where

import Language.Haskell.Exts.Annotated.Syntax hiding ( Type(..), Asst(..), Exp(..), FieldUpdate(..), XAttr(..), Context(..) )
import qualified Language.Haskell.Exts.Annotated.Syntax as S ( Type(..), Asst(..), Exp(..), FieldUpdate(..), XAttr(..), Context(..) )
import Language.Haskell.Exts.Annotated.ParseMonad
import Language.Haskell.Exts.Annotated.Pretty
import Language.Haskell.Exts.Annotated.Build

import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.ExtScheme

import Data.List (intersperse)
import Data.Maybe (fromJust)
import Control.Monad (when,liftM)

--- import Debug.Trace (trace)

type L = SrcSpanInfo
type S = SrcSpan

splitTyConApp :: PType L -> P (Name L, [S.Type L])
splitTyConApp t0 = do
            (n, pts) <- split t0 []
            ts <- mapM checkType pts
            return (n,ts)
 where
    split :: PType L -> [PType L] -> P (Name L, [PType L])
    split (TyApp _ t u) ts = split t (u:ts)
    split (TyCon _ (UnQual _ t)) ts = return (t,ts)
    split (TyInfix l a op b) ts = split (TyCon l op) (a:b:ts)
    split _ _ = fail "Illegal data/newtype declaration"

-----------------------------------------------------------------------------
-- Checking for extensions

checkEnabled :: (Show e, Enabled e) => e  -> P ()
checkEnabled e = do
    exts <- getExtensions
    if isEnabled e exts
     then return ()
     else fail $ show e ++ " is not enabled"

checkPatternGuards :: [Stmt L] -> P ()
checkPatternGuards [Qualifier _ _] = return ()
checkPatternGuards _ = checkEnabled PatternGuards

-----------------------------------------------------------------------------
-- Checking contexts

-- Check that a context is syntactically correct. Takes care of
-- checking for MPTCs, TypeOperators, TypeFamilies (for eq constraints)
-- and ImplicitParameters, but leaves checking of the class assertion
-- parameters for later.
checkPContext :: PType L -> P (PContext L)
checkPContext (TyTuple l Boxed ts) =
    mapM checkAssertion ts >>= return . CxTuple l
checkPContext (TyCon l (Special _ (UnitCon _))) =
    return $ CxEmpty l
checkPContext (TyParen l t) = do
    c <- checkPContext t
    return $ CxParen l c
checkPContext t = do
    c <- checkAssertion t
    return $ CxSingle (ann c) c

------------------------------------------------------------------------------------------------------------------- WORKING HERE

-- Check a single assertion according to the above, still leaving
-- the class assertion parameters for later.
checkAssertion :: PType L -> P (PAsst L)
-- We cannot even get here unless ImplicitParameters is enabled.
checkAssertion (TyPred _ p@(IParam _ _ _)) = return p
-- We cannot even get here unless TypeFamilies is enabled.
checkAssertion (TyPred _ p@(EqualP _ _ _)) = return p
checkAssertion t = checkAssertion' id [] t
    where   -- class assertions must have at least one argument
            checkAssertion' fl ts@(_:xs) (TyCon l c) = do
                when (not $ null xs) $ checkEnabled MultiParamTypeClasses
                when (isSymbol c)    $ checkEnabled TypeOperators
                return $ ClassA (fl l) c ts
            checkAssertion' fl ts (TyApp l a t) = do
                -- no check on t at this stage
                checkAssertion' (const (fl l)) (t:ts) a
            checkAssertion' fl ts (TyInfix l a op b) = do
                -- infix operators require TypeOperators
                checkEnabled TypeOperators
                return $ InfixA (fl l) a op b
            checkAssertion' fl ts (TyParen l t) =
                checkAssertion' (const (fl l)) ts t
            checkAssertion' _ _ _ = fail "Illegal class assertion"

isSymbol :: QName L -> Bool
isSymbol (UnQual _ (Symbol _ _)) = True
isSymbol (Qual _ _ (Symbol _ _)) = True
isSymbol _                       = False


-- Checks simple contexts for class and instance
-- headers. If FlexibleContexts is enabled then
-- anything goes, otherwise only tyvars are allowed.
checkSContext :: Maybe (PContext L) -> P (Maybe (S.Context L))
checkSContext (Just ctxt) = case ctxt of
    CxEmpty l -> return $ Just $ S.CxEmpty l
    CxSingle l a -> checkAsst True a >>= return . Just . S.CxSingle l
    CxTuple l as -> mapM (checkAsst True) as >>= return . Just . S.CxTuple l
    CxParen l cx -> checkSContext (Just cx) >>= return . fmap (S.CxParen l)
checkSContext _ = return Nothing

-- Checks ordinary contexts for sigtypes and data type
-- declarations. If FlexibleContexts is enabled then
-- anything goes, otherwise only tyvars OR tyvars
-- applied to types are allowed.
checkContext :: Maybe (PContext L) -> P (Maybe (S.Context L))
checkContext (Just ctxt) = case ctxt of
    CxEmpty l -> return $ Just $ S.CxEmpty l
    CxSingle l a -> checkAsst False a >>= return . Just . S.CxSingle l
    CxTuple l as -> mapM (checkAsst False) as >>= return . Just . S.CxTuple l
    CxParen l cx -> checkSContext (Just cx) >>= return . fmap (S.CxParen l)
checkContext _ = return Nothing

checkAsst :: Bool -> PAsst L -> P (S.Asst L)
checkAsst isSimple asst =
    case asst of
      ClassA l qn pts -> do
                ts <- mapM (checkAsstParam isSimple) pts
                return $ S.ClassA l qn ts
      InfixA l a op b -> do
                [a,b] <- mapM (checkAsstParam isSimple) [a,b]
                return $ S.InfixA l a op b
      IParam l ipn pt -> do
                t <- checkType pt
                return $ S.IParam l ipn t
      EqualP l pa pb  -> do
                a <- checkType pa
                b <- checkType pb
                return $ S.EqualP l a b

checkAsstParam :: Bool -> PType L -> P (S.Type L)
checkAsstParam isSimple t = do
        exts <- getExtensions
        if FlexibleContexts `elem` exts
         then checkType t
         else case t of
                TyVar l n     -> return $ S.TyVar l n
                TyApp l pf pt | not isSimple    -> do
                        f <- checkAsstParam isSimple pf
                        t <- checkType pt
                        return $ S.TyApp l f t
                _       -> fail "Malformed context: FlexibleContexts not enabled"

-----------------------------------------------------------------------------
-- Checking Headers


checkDataHeader :: PType L -> P (Maybe (S.Context L), DeclHead L)
checkDataHeader (TyForall _ Nothing cs t) = do
    dh <- checkSimple "data/newtype" t []
    cs <- checkContext cs
    return (cs,dh)
checkDataHeader t = do
    dh <- checkSimple "data/newtype" t []
    return (Nothing,dh)

checkClassHeader :: PType L -> P (Maybe (S.Context L), DeclHead L)
checkClassHeader (TyForall _ Nothing cs t) = do
    dh <- checkSimple "class" t []
    cs <- checkSContext cs
    return (cs,dh)
checkClassHeader t = do
    dh <- checkSimple "class" t []
    return (Nothing,dh)

checkSimple :: String -> PType L -> [TyVarBind L] -> P (DeclHead L)
checkSimple kw (TyApp _ l t) xs | isTyVarBind t = checkSimple kw l (toTyVarBind t : xs)
checkSimple _  (TyInfix l t1 (UnQual _ t) t2) []
    | isTyVarBind t1 && isTyVarBind t2 =
       checkEnabled TypeOperators >> return (DHInfix l (toTyVarBind t1) t (toTyVarBind t2))
checkSimple _kw (TyCon l (UnQual _ t))   xs = do
    case t of
      Symbol _ _ -> checkEnabled TypeOperators
      _ -> return ()
    return (DHead l t xs)
checkSimple kw (TyParen l t) xs = do
    dh <- checkSimple kw t xs
    return (DHParen l dh)
checkSimple kw _ _ = fail ("Illegal " ++ kw ++ " declaration")

isTyVarBind :: PType L -> Bool
isTyVarBind (TyVar _ _) = True
isTyVarBind (TyKind _ (TyVar _ _) _) = True
isTyVarBind _ = False

toTyVarBind :: PType L -> TyVarBind L
toTyVarBind (TyVar l n) = UnkindedVar l n
toTyVarBind (TyKind l (TyVar _ n) k) = KindedVar l n k

checkInstHeader :: PType L -> P (Maybe (S.Context L), InstHead L)
checkInstHeader (TyForall _ Nothing cs t) = do
    ih <- checkInsts t []
    cs <- checkSContext cs
    return (cs, ih)
checkInstHeader t = do
    ih <- checkInsts t []
    return (Nothing, ih)


checkInsts :: PType L -> [PType L] -> P (InstHead L)
checkInsts (TyApp _ l t) ts = checkInsts l (t:ts)
checkInsts (TyCon l c)   ts = do
    when (isSymbol c) $ checkEnabled TypeOperators
    ts <- checkTypes ts
    return $ IHead l c ts
checkInsts (TyInfix l a op b) [] = do
    checkEnabled TypeOperators
    [ta,tb] <- checkTypes [a,b]
    return $ IHInfix l ta op tb
checkInsts (TyParen l t) [] = checkInsts t [] >>= return . IHParen l
checkInsts _ _ = fail "Illegal instance declaration"

checkDeriving :: [PType L] -> P [InstHead L]
checkDeriving = mapM (flip checkInsts [])

-----------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: PExp L -> P (Pat L)
checkPattern e = checkPat e []

checkPat :: PExp L -> [Pat L] -> P (Pat L)
checkPat (Con l c) args = return (PApp l c args)
checkPat (App l f x) args = do
    x <- checkPat x []
    checkPat f (x:args)
checkPat (InfixApp _ l op r) args
    | op =~= (QVarOp () (UnQual () (Symbol () "!"))) = do
        -- We must have BangPatterns on
        checkEnabled BangPatterns
        let (e,es) = splitBang r []
        ps <- mapM checkPattern (BangPat (ann op) e:es)
        checkPat l (ps++args)
checkPat e [] = case e of
    Var l (UnQual _ x)   -> return (PVar l x)
    Lit l lit            -> return (PLit l lit)
    InfixApp loc l op r  ->
        case op of
            QConOp _ c -> do
                    l <- checkPat l []
                    r <- checkPat r []
                    return (PInfixApp loc l c r)
            QVarOp ppos (UnQual _ (Symbol _ "+")) -> do
                    case (l,r) of
                        (Var _ (UnQual _ n@(Ident _ _)), Lit _ (Int kpos k _)) -> do
                            let pp = srcInfoSpan ppos
                                kp = srcInfoSpan kpos
                            return (PNPlusK (loc <** [pp,kp]) n k)
                        _ -> patFail ""
            _ -> patFail ""
    TupleSection l mes    ->
            if not (any ((=~=) Nothing) mes)
             then do ps <- mapM (\e -> checkPat e []) (map fromJust mes)
                     return (PTuple l ps)
             else fail "Illegal tuple section in pattern"

    List l es      -> do
                  ps <- mapM checkRPattern es
                  if all isStdPat ps
                    then return . PList l $ map stripRP ps
                    -- we don't allow truly regular patterns unless the extension is enabled
                    else checkEnabled RegularPatterns >> return (PRPat l $ map fixRPOpPrec ps)
            where isStdPat :: RPat L -> Bool
                  isStdPat (RPPat _ _) = True
                  isStdPat (RPAs _ _ p) = isStdPat p
                  isStdPat (RPParen _ p) = isStdPat p
                  isStdPat _           = False
                  stripRP :: RPat L -> Pat L
                  stripRP (RPPat  _ p) = p
                  stripRP (RPAs l n p) = PAsPat l n (stripRP p)
                  stripRP (RPParen l p) = PParen l (stripRP p)
                  stripRP _           = error "cannot strip RP wrapper if not all patterns are base"

    Paren l e      -> do
                  p <- checkPat e []
                  return (PParen l p)
    AsPat l n e    -> do
                  p <- checkPat e []
                  return (PAsPat l n p)
    WildCard l   -> return (PWildCard l)
    IrrPat l e   -> do
                  p <- checkPat e []
                  return (PIrrPat l p)
    ViewPat l e p  -> do
                  e <- checkExpr e
                  p <- checkPat p []
                  return (PViewPat l e p)
    RecConstr l c fs   -> do
                  fs <- mapM checkPatField fs
                  return (PRec l c fs)
    NegApp l1 (Lit l2 lit) -> return (PNeg l1 (PLit l2 lit))
    ExpTypeSig l e t -> do
                  -- patterns cannot have signatures unless ScopedTypeVariables is enabled.
                  checkEnabled ScopedTypeVariables
                  p <- checkPat e []
                  return (PatTypeSig l p t)

    -- Hsx
    XTag l n attrs mattr cs -> do
                  pattrs <- mapM checkPAttr attrs
                  pcs    <- mapM (\c -> checkPat c []) cs
                  mpattr <- maybe (return Nothing)
                              (\e -> do p <- checkPat e []
                                        return $ Just p)
                              mattr
                  let cps = mkChildrenPat pcs
                  return $ PXTag l n pattrs mpattr cps
    XETag l n attrs mattr -> do
                  pattrs <- mapM checkPAttr attrs
                  mpattr <- maybe (return Nothing)
                              (\e -> do p <- checkPat e []
                                        return $ Just p)
                              mattr
                  return $ PXETag l n pattrs mpattr
    XPcdata l pcdata   -> return $ PXPcdata l pcdata
    XExpTag l e -> do
            p <- checkPat e []
            return $ PXPatTag l p
    XRPats l es -> do
            rps <- mapM checkRPattern es
            return (PXRPats l $ map fixRPOpPrec rps)

    -- Generics
    ExplTypeArg l qn t -> return $ PExplTypeArg l qn t

    -- QuasiQuotation
    QuasiQuote l n q -> return $ PQuasiQuote l n q

    -- BangPatterns
    BangPat l e -> do
        p <- checkPat e []
        return $ PBangPat l p

    PreOp l (QVarOp _ (UnQual _ (Symbol _ "!"))) e -> do
        checkEnabled BangPatterns
        p <- checkPat e []
        return $ PBangPat l p

    e -> patFail $ show e

checkPat e _ = patFail $ show e

splitBang :: PExp L -> [PExp L] -> (PExp L, [PExp L])
splitBang (App _ f x) es = splitBang f (x:es)
splitBang e es = (e, es)

checkPatField :: PFieldUpdate L -> P (PatField L)
checkPatField (FieldUpdate l n e) = do
    p <- checkPat e []
    return (PFieldPat l n p)
checkPatField (FieldPun l n) = return (PFieldPun l n)
checkPatField (FieldWildcard l) = return (PFieldWildcard l)

checkPAttr :: ParseXAttr L -> P (PXAttr L)
checkPAttr (XAttr l n v) = do p <- checkPat v []
                              return $ PXAttr l n p

patFail :: String -> P a
patFail s = fail $ "Parse error in pattern: " ++ s

checkRPattern :: PExp L -> P (RPat L)
checkRPattern e = case e of
    SeqRP l es -> do
        rps <- mapM checkRPattern es
        return $ RPSeq l rps
    PostOp l e op -> do
        rpop <- checkRPatOp op
        rp   <- checkRPattern e
        return $ RPOp l rp rpop
    GuardRP l e gs -> do
        rp <- checkPattern e
        return $ RPGuard l rp gs
    EitherRP l e1 e2 -> do
        rp1 <- checkRPattern e1
        rp2 <- checkRPattern e2
        return $ RPEither l rp1 rp2
    CAsRP l n e -> do
        rp <- checkRPattern e
        return $ RPCAs l n rp
    AsPat l n e  -> do
        rp <- checkRPattern e
        return $ RPAs l n rp
    Paren l e -> do
        rp <- checkRPattern e
        return $ RPParen l rp
    _          -> do
        p <- checkPattern e
        return $ RPPat (ann p) p

checkRPatOp :: QOp L -> P (RPatOp L)
checkRPatOp o@(QVarOp l (UnQual _ (Symbol _ sym))) =
    case sym of
     "*"  -> return $ RPStar l
     "*!" -> return $ RPStarG l
     "+"  -> return $ RPPlus l
     "+!" -> return $ RPPlusG l
     "?"  -> return $ RPOpt l
     "?!" -> return $ RPOptG l
     _    -> rpOpFail o
checkRPatOp o = rpOpFail o

rpOpFail sym = fail $ "Unrecognized regular pattern operator: " ++ show sym

fixRPOpPrec :: RPat L -> RPat L
fixRPOpPrec rp = case rp of
    RPOp l rp rpop      -> fPrecOp rp (flip (RPOp l) rpop)
    RPEither l rp1 rp2  -> RPEither l (fixRPOpPrec rp1) (fixRPOpPrec rp2)
    RPSeq l rps         -> RPSeq l $ map fixRPOpPrec rps
    RPCAs l n rp        -> RPCAs l n $ fixRPOpPrec rp
    RPAs l n rp         -> RPAs l n $ fixRPOpPrec rp
    RPParen l rp        -> RPParen l $ fixRPOpPrec rp
    _                   -> rp

  where fPrecOp :: RPat L -> (RPat L -> RPat L) -> RPat L
        fPrecOp (RPOp l rp rpop) f = fPrecOp rp (f . flip (RPOp l) rpop)
        fPrecOp (RPCAs l n rp) f = fPrecAs rp f (RPCAs l n)
        fPrecOp (RPAs  l n rp) f = fPrecAs rp f (RPAs  l n)
        fPrecOp rp f = f $ fixRPOpPrec rp
        fPrecAs :: RPat L -> (RPat L -> RPat L) -> (RPat L -> RPat L) -> RPat L
        fPrecAs (RPCAs l n rp) f g = fPrecAs rp f (g . RPCAs l n)
        fPrecAs (RPAs  l n rp) f g = fPrecAs rp f (g . RPAs  l n)
        fPrecAs rp f g = g . f $ fixRPOpPrec rp


mkChildrenPat :: [Pat L] -> [Pat L]
mkChildrenPat ps = mkCPAux ps []
  where mkCPAux :: [Pat L] -> [Pat L] -> [Pat L]
        mkCPAux [] qs = reverse qs
        mkCPAux (p:ps) qs = case p of
            (PRPat l rps) -> [mkCRP l ps (reverse rps ++ map (\q -> RPPat (ann q) q) qs)]
            _             -> mkCPAux ps (p:qs)

        mkCRP :: L -> [Pat L] -> [RPat L] -> Pat L
        mkCRP l [] rps = PXRPats l $ reverse rps
        mkCRP _ (p:ps) rps = case p of
            (PXRPats l rqs) -> mkCRP l ps (reverse rqs ++ rps)
            _               -> mkCRP (ann p) ps (RPPat (ann p) p : rps)

-----------------------------------------------------------------------------
-- Check Expression Syntax

checkExpr :: PExp L -> P (S.Exp L)
checkExpr e = case e of
    Var l v               -> return $ S.Var l v
    IPVar l v             -> return $ S.IPVar l v
    Con l c               -> return $ S.Con l c
    Lit l lit             -> return $ S.Lit l lit
    InfixApp l e1 op e2   -> check2Exprs e1 e2 (flip (S.InfixApp l) op)
    App l e1 e2           -> check2Exprs e1 e2 (S.App l)
    NegApp l e            -> check1Expr e (S.NegApp l)
    Lambda loc ps e       -> check1Expr e (S.Lambda loc ps)
    Let l bs e            -> check1Expr e (S.Let l bs)
    If l e1 e2 e3         -> check3Exprs e1 e2 e3 (S.If l)
    Case l e alts         -> do
                     e <- checkExpr e
                     return (S.Case l e alts)
    Do l stmts            -> checkDo stmts >> return (S.Do l stmts)
    MDo l stmts           -> checkDo stmts >> return (S.MDo l stmts)
    TupleSection l mes -> if not (any ((=~=) Nothing) mes)
                           then checkManyExprs (map fromJust mes) (S.Tuple l)
                           else do checkEnabled TupleSections
                                   mes' <- mapM mCheckExpr mes
                                   return $ S.TupleSection l mes'


    List l es         -> checkManyExprs es (S.List l)
    -- Since we don't parse things as left or right sections, we need to mangle them into that.
    Paren l e         -> case e of
                          PostOp _ e1 op -> check1Expr e1 (flip (S.LeftSection l) op)
                          PreOp  _ op e2 -> check1Expr e2 (S.RightSection l op)
                          _            -> check1Expr e (S.Paren l)
    RecConstr l c fields      -> do
                     fields <- mapM checkField fields
                     return (S.RecConstr l c fields)
    RecUpdate l e fields      -> do
                     fields <- mapM checkField fields
                     e <- checkExpr e
                     return (S.RecUpdate l e fields)
    EnumFrom l e          -> check1Expr e (S.EnumFrom l)
    EnumFromTo l e1 e2    -> check2Exprs e1 e2 (S.EnumFromTo l)
    EnumFromThen l e1 e2      -> check2Exprs e1 e2 (S.EnumFromThen l)
    EnumFromThenTo l e1 e2 e3 -> check3Exprs e1 e2 e3 (S.EnumFromThenTo l)
    -- a parallel list comprehension, which could be just a simple one
    ParComp l e qualss        -> do
                     e <- checkExpr e
                     case qualss of
                      [quals] -> return (S.ListComp l e quals)
                      _       -> return (S.ParComp l e qualss)
    ExpTypeSig loc e ty     -> do
                     e <- checkExpr e
                     return (S.ExpTypeSig loc e ty)

    --Template Haskell
    BracketExp l e        -> return $ S.BracketExp l e
    SpliceExp l e         -> return $ S.SpliceExp l e
    TypQuote l q          -> return $ S.TypQuote l q
    VarQuote l q          -> return $ S.VarQuote l q
    QuasiQuote l n q      -> return $ S.QuasiQuote l n q

    -- Hsx
    XTag l n attrs mattr cs -> do attrs <- mapM checkAttr attrs
                                  cs <- mapM checkExpr cs
                                  mattr <- maybe (return Nothing)
                                              (\e -> checkExpr e >>= return . Just)
                                              mattr
                                  return $ S.XTag l n attrs mattr cs
    XETag l n attrs mattr   -> do attrs <- mapM checkAttr attrs
                                  mattr <- maybe (return Nothing)
                                              (\e -> checkExpr e >>= return . Just)
                                              mattr
                                  return $ S.XETag l n attrs mattr
    XPcdata l p       -> return $ S.XPcdata l p
    XExpTag l e       -> do e <- checkExpr e
                            return $ S.XExpTag l e
    -- Pragmas
    CorePragma l s e  -> check1Expr e (S.CorePragma l s)
    SCCPragma  l s e  -> check1Expr e (S.SCCPragma l s)
    GenPragma l s xx yy e -> check1Expr e (S.GenPragma l s xx yy)
--    UnknownExpPragma n s -> return $ S.UnknownExpPragma n s

    -- Arrows
    Proc l p e        -> do e <- checkExpr e
                            return $ S.Proc l p e
    LeftArrApp l e1 e2      -> check2Exprs e1 e2 (S.LeftArrApp l)
    RightArrApp l e1 e2     -> check2Exprs e1 e2 (S.RightArrApp l)
    LeftArrHighApp l e1 e2  -> check2Exprs e1 e2 (S.LeftArrHighApp l)
    RightArrHighApp l e1 e2 -> check2Exprs e1 e2 (S.RightArrHighApp l)

    _             -> fail $ "Parse error in expression: " ++ show e

checkAttr :: ParseXAttr L -> P (S.XAttr L)
checkAttr (XAttr l n v) = do v <- checkExpr v
                             return $ S.XAttr l n v

checkDo [] = error "Parse error: Last statement in a do-block must be an expression"
checkDo [Qualifier _ _] = return ()
checkDo (_:xs) = checkDo xs

-- type signature for polymorphic recursion!!
check1Expr :: PExp L -> (S.Exp L -> a) -> P a
check1Expr e1 f = do
    e1 <- checkExpr e1
    return (f e1)

check2Exprs :: PExp L -> PExp L -> (S.Exp L -> S.Exp L -> a) -> P a
check2Exprs e1 e2 f = do
    e1 <- checkExpr e1
    e2 <- checkExpr e2
    return (f e1 e2)

check3Exprs :: PExp L -> PExp L -> PExp L -> (S.Exp L -> S.Exp L -> S.Exp L -> a) -> P a
check3Exprs e1 e2 e3 f = do
    e1 <- checkExpr e1
    e2 <- checkExpr e2
    e3 <- checkExpr e3
    return (f e1 e2 e3)

checkManyExprs :: [PExp L] -> ([S.Exp L] -> a) -> P a
checkManyExprs es f = do
    es <- mapM checkExpr es
    return (f es)

mCheckExpr :: Maybe (PExp L) -> P (Maybe (S.Exp L))
mCheckExpr Nothing = return Nothing
mCheckExpr (Just e) = checkExpr e >>= return . Just

checkRuleExpr :: PExp L -> P (S.Exp L)
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

checkField :: PFieldUpdate L -> P (S.FieldUpdate L)
checkField (FieldUpdate l n e) = check1Expr e (S.FieldUpdate l n)
checkField (FieldPun l n) = return $ S.FieldPun l n
checkField (FieldWildcard l) = return $ S.FieldWildcard l

getGConName :: S.Exp L -> P (QName L)
getGConName (S.Con _ n) = return n
getGConName (S.List l []) = return (list_cons_name l)
getGConName _ = fail "Expression in reification is not a name"

-----------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef :: L -> PExp L -> Maybe (S.Type L) -> Rhs L -> Maybe (Binds L) -> P (Decl L)
checkValDef l lhs optsig rhs whereBinds = do
    mlhs <- isFunLhs lhs []
    case mlhs of
     Just (f,es,b) -> do
            ps <- mapM checkPattern es
            case optsig of -- only pattern bindings can have signatures
                Nothing -> return (FunBind l $
                            if b then [Match l f ps rhs whereBinds]
                                 else let [a,b] = ps in [InfixMatch l a f b rhs whereBinds])
                Just _  -> fail "Cannot give an explicit type signature to a function binding"
     Nothing     -> do
            lhs <- checkPattern lhs
            return (PatBind l lhs optsig rhs whereBinds)

-- A variable binding is parsed as a PatBind.

isFunLhs :: PExp L -> [PExp L] -> P (Maybe (Name L, [PExp L], Bool))
isFunLhs (InfixApp _ l (QVarOp loc (UnQual _ op)) r) es
    | op =~= (Symbol () "!") = do
        exts <- getExtensions
        if BangPatterns `elem` exts
         then let (b,bs) = splitBang r []
               in isFunLhs l (BangPat loc b : bs ++ es)
         else return $ Just (op, l:r:es, False) -- It's actually a definition of the operator !
    | otherwise = return $ Just (op, l:r:es, False)
isFunLhs (App _ (Var _ (UnQual _ f)) e) es = return $ Just (f, e:es, True)
isFunLhs (App _ f e) es = isFunLhs f (e:es)
isFunLhs (Var _ (UnQual _ f)) es@(_:_) = return $ Just (f, es, True)
isFunLhs (Paren _ f) es@(_:_) = isFunLhs f es
isFunLhs _ _ = return Nothing

-- Separating between signature declarations and value definitions in
-- a post-processing step

checkSigVar :: PExp L -> P (Name L)
checkSigVar (Var _ (UnQual _ n)) = return n
checkSigVar e = fail $ "Left-hand side of type signature is not a variable: " ++ show e

-----------------------------------------------------------------------------
-- In a class or instance body, a pattern binding must be of a variable.

checkClassBody :: [ClassDecl L] -> P [ClassDecl L]
checkClassBody decls = do
    mapM_ checkClassMethodDef decls
    return decls
  where checkClassMethodDef (ClsDecl _ decl) = checkMethodDef decl
        checkClassMethodDef _ = return ()

checkInstBody :: [InstDecl L] -> P [InstDecl L]
checkInstBody decls = do
    mapM_ checkInstMethodDef decls
    return decls
  where checkInstMethodDef (InsDecl _ decl) = checkMethodDef decl
        checkInstMethodDef _ = return ()

checkMethodDef :: Decl L -> P ()
checkMethodDef (PatBind _ (PVar _ _) _ _ _) = return ()
checkMethodDef (PatBind loc _ _ _ _) =
    fail "illegal method definition" -- `atSrcLoc` loc
checkMethodDef _ = return ()

-----------------------------------------------------------------------------
-- Check that an identifier or symbol is unqualified.
-- For occasions when doing this in the grammar would cause conflicts.

checkUnQual :: QName L -> P (Name L)
checkUnQual (Qual  _ _ _) = fail "Illegal qualified name"
checkUnQual (UnQual  _ n) = return n
checkUnQual (Special _ _) = fail "Illegal special name"

-----------------------------------------------------------------------------
-- Check that two xml tag names are equal
-- Could use Eq directly, but I am not sure whether <dom:name>...</name>
-- would be valid, in that case Eq won't work. TODO

checkEqNames :: XName L -> XName L -> P (XName L)
checkEqNames n@(XName _ n1) (XName _ n2)
    | n1 == n2  = return n
    | otherwise = fail "names in matching xml tags are not equal"
checkEqNames n@(XDomName _ d1 n1) (XDomName _ d2 n2)
    | n1 == n2 && d1 == d2 = return n
    | otherwise = fail "names in matching xml tags are not equal"
checkEqNames _ _ = fail "names in matching xml tags are not equal"


-----------------------------------------------------------------------------
-- Miscellaneous utilities

checkPrec :: Integer -> P Int
checkPrec i | 0 <= i && i <= 9 = return (fromInteger i)
checkPrec i | otherwise        = fail ("Illegal precedence " ++ show i)

mkRecConstrOrUpdate :: PExp L -> [PFieldUpdate L] -> P (PExp L)
mkRecConstrOrUpdate (Con l c) fs       = return (RecConstr l c fs)
mkRecConstrOrUpdate e         fs@(_:_) = return (RecUpdate (ann e) e fs)
mkRecConstrOrUpdate _         _        = fail "Empty record update"

-----------------------------------------------------------------------------
-- Reverse a list of declarations, merging adjacent FunBinds of the
-- same name and checking that their arities match.

checkRevDecls :: [Decl L] -> P [Decl L]
checkRevDecls = mergeFunBinds []
    where
    mergeFunBinds revDs [] = return revDs
    mergeFunBinds revDs (FunBind l ms1@(Match _ name ps _ _:_):ds1) =
        mergeMatches ms1 ds1 l
        where
        arity = length ps
        mergeMatches ms' (FunBind _ ms@(Match loc name' ps' _ _:_):ds) l
            | name' =~= name =
            if length ps' /= arity
            then fail ("arity mismatch for '" ++ prettyPrint name ++ "'")
                    -- `atSrcLoc` loc
            else mergeMatches (ms++ms') ds (loc <++> l)
        mergeMatches ms' ds l = mergeFunBinds (FunBind l ms':revDs) ds
    mergeFunBinds revDs (d:ds) = mergeFunBinds (d:revDs) ds

checkRevClsDecls :: [ClassDecl L] -> P [ClassDecl L]
checkRevClsDecls = mergeClsFunBinds []
    where
    mergeClsFunBinds revDs [] = return revDs
    mergeClsFunBinds revDs (ClsDecl l (FunBind _ ms1@(Match _ name ps _ _:_)):ds1) =
        mergeMatches ms1 ds1 l
        where
        arity = length ps
        mergeMatches ms' (ClsDecl _ (FunBind _ ms@(Match loc name' ps' _ _:_)):ds) l
            | name' =~= name =
            if length ps' /= arity
            then fail ("arity mismatch for '" ++ prettyPrint name ++ "'")
                    -- `atSrcLoc` loc
            else mergeMatches (ms++ms') ds (loc <++> l)
        mergeMatches ms' ds l = mergeClsFunBinds (ClsDecl l (FunBind l ms'):revDs) ds
    mergeClsFunBinds revDs (d:ds) = mergeClsFunBinds (d:revDs) ds

checkRevInstDecls :: [InstDecl L] -> P [InstDecl L]
checkRevInstDecls = mergeInstFunBinds []
    where
    mergeInstFunBinds :: [InstDecl L] -> [InstDecl L] -> P [InstDecl L]
    mergeInstFunBinds revDs [] = return revDs
    mergeInstFunBinds revDs (InsDecl l (FunBind _ ms1@(Match _ name ps _ _:_)):ds1) =
        mergeMatches ms1 ds1 l
        where
        arity = length ps
        mergeMatches ms' (InsDecl _ (FunBind _ ms@(Match loc name' ps' _ _:_)):ds) l
            | name' =~= name =
            if length ps' /= arity
            then fail ("arity mismatch for '" ++ prettyPrint name ++ "'")
                    -- `atSrcLoc` loc
            else mergeMatches (ms++ms') ds (loc <++> l)
        mergeMatches ms' ds l = mergeInstFunBinds (InsDecl l (FunBind l ms'):revDs) ds
    mergeInstFunBinds revDs (d:ds) = mergeInstFunBinds (d:revDs) ds

----------------------------------------------------------------
-- Check that newtype declarations have
-- the right number (1) of constructors

checkDataOrNew :: DataOrNew L -> [a] -> P ()
checkDataOrNew (NewType _) [x] = return ()
checkDataOrNew (DataType _) _  = return ()
checkDataOrNew _        _  = fail "newtype declaration must have exactly one constructor."

checkSimpleType :: PType L -> P (DeclHead L)
checkSimpleType t = checkSimple "test" t []

---------------------------------------
-- Check actual types

checkType :: PType L -> P (S.Type L)
checkType t = checkT t False

checkT :: PType L -> Bool -> P (S.Type L)
checkT t simple = case t of
    TyForall l tvs@Nothing cs pt    -> do
            when (simple) $ checkEnabled ExplicitForall
            ctxt <- checkContext cs
            check1Type pt (S.TyForall l Nothing ctxt)
    TyForall l tvs cs pt -> do
            checkEnabled ExplicitForall
            ctxt <- checkContext cs
            check1Type pt (S.TyForall l tvs ctxt)
    TyFun   l at rt   -> check2Types at rt (S.TyFun l)
    TyTuple l b pts   -> checkTypes pts >>= return . S.TyTuple l b
    TyList  l pt      -> check1Type pt (S.TyList l)
    TyApp   l ft at   -> check2Types ft at (S.TyApp l)
    TyVar   l n       -> return $ S.TyVar l n
    TyCon   l n       -> do
            when (isSymbol n) $ checkEnabled TypeOperators
            return $ S.TyCon l n
    TyParen l pt      -> check1Type pt (S.TyParen l)
    -- TyPred  cannot be a valid type
    -- Here we know that t will be used as an actual type (and not a data constructor)
    -- so we can check that TypeOperators are enabled.
    TyInfix l at op bt -> checkEnabled TypeOperators >> check2Types at bt (flip (S.TyInfix l) op)
    TyKind  l pt k    -> check1Type pt (flip (S.TyKind l) k)

check1Type :: PType L -> (S.Type L -> S.Type L) -> P (S.Type L)
check1Type pt f = checkT pt True >>= return . f

check2Types :: PType L -> PType L -> (S.Type L -> S.Type L -> S.Type L) -> P (S.Type L)
check2Types at bt f = checkT at True >>= \a -> checkT bt True >>= \b -> return (f a b)

checkTypes :: [PType L] -> P [S.Type L]
checkTypes = mapM (flip checkT True)

---------------------------------------
-- Converting a complete page

checkPageModule :: PExp L -> ([OptionPragma L],[S],L) -> P (Module L)
checkPageModule xml (os,ss,inf) = do
    mod <- getModuleName
    xml <- checkExpr xml
    case xml of
        S.XTag  l xn ats mattr cs -> return $ XmlPage (inf<++>l<**(srcInfoPoints l ++ ss)) (ModuleName l mod) os xn ats mattr cs
        S.XETag l xn ats mattr    -> return $ XmlPage (inf<++>l<**(srcInfoPoints l ++ ss)) (ModuleName l mod) os xn ats mattr []

checkHybridModule :: PExp L -> Module L -> S -> S -> P (Module L)
checkHybridModule xml (Module inf mh os is ds) s1 s2 = do
    xml <- checkExpr xml
    case xml of
        S.XTag  l xn ats mattr cs -> return $ XmlHybrid (inf<++>l<**(s1 : srcInfoPoints inf ++ s2 : srcInfoPoints l))
                                                mh os is ds xn ats mattr cs
        S.XETag l xn ats mattr    -> return $ XmlHybrid (inf<++>l<**(s1 : srcInfoPoints inf ++ s2 : srcInfoPoints l))
                                                mh os is ds xn ats mattr []

{-
pageFun :: L -> S.Exp L -> Decl L
pageFun loc e = PatBind loc namePat Nothing rhs Nothing
    where namePat = PVar loc $ Ident loc "page"
          rhs = UnGuardedRhs (ann e) e

mkPage :: Module L -> L -> S.Exp L -> P (Module L)
mkPage (Module src mmh os imps decls) loc xml = do
    let page = pageFun loc xml
    return $ Module src mmh os imps (decls ++ [page])

mkPageModule :: [OptionPragma L] -> S.Exp L -> P (Module L)
mkPageModule os xml = do
    do loc <- case xml of
           S.XTag l _ _ _ _ -> return l
           S.XETag l _ _ _  -> return l
           _ -> fail "Will not happen since mkPageModule is only called on XML expressions"
       mod <- getModuleName
       return $ (Module
              loc
              (ModuleName loc mod)
              os
              Nothing
              (Just (ExportSpecList loc [EVar loc $ UnQual loc $ Ident loc "page"]))
              []
              [pageFun loc xml])
-}
---------------------------------------
-- Handle dash-identifiers

mkDVar :: [String] -> String
mkDVar = concat . intersperse "-"

--mkDVarExpr :: L -> [String] -> PExp L
--mkDVarExpr l = foldl1 (\x y -> InfixApp l x (op $ sym "-") y) . map (Var l . UnQual l . name)

---------------------------------------
-- Combine adjacent for-alls. NO!
--
-- A valid type must have one for-all at the top of the type, or of the fn arg types

mkTyForall :: L -> Maybe [TyVarBind L] -> Maybe (PContext L) -> PType L -> PType L
--mkTyForall l mtvs (PContext _ [])   ty = mk_forall_ty l mtvs ty
mkTyForall l mtvs ctxt ty = TyForall l mtvs ctxt ty

{-- mk_forall_ty makes a pure for-all type (no context)
mk_forall_ty l (Just []) ty             = ty  -- Explicit for-all with no tyvars
mk_forall_ty l mtvs1     (TyForall _ mtvs2 ctxt ty) = mkTyForall l (mtvs1 `plus` mtvs2) ctxt ty
mk_forall_ty l mtvs1     ty             = TyForall l mtvs1 (PContext l []) ty

mtvs1       `plus` Nothing     = mtvs1
Nothing     `plus` mtvs2       = mtvs2
(Just tvs1) `plus` (Just tvs2) = Just (tvs1 ++ tvs2)
-}
---------------------------------------
-- Expressions as we parse them (and patters, and regular patterns)

data PExp l
    = Var l (QName l)                 -- ^ variable
    | IPVar l (IPName l)              -- ^ implicit parameter variable
    | Con l (QName l)                 -- ^ data constructor
    | Lit l (Literal l)               -- ^ literal constant
    | InfixApp l (PExp l) (QOp l) (PExp l)    -- ^ infix application
    | App l (PExp l) (PExp l)             -- ^ ordinary application
    | NegApp l (PExp l)               -- ^ negation expression @-@ /exp/
    | Lambda l [Pat l] (PExp l) -- ^ lambda expression
    | Let l (Binds l) (PExp l)           -- ^ local declarations with @let@
    | If l (PExp l) (PExp l) (PExp l)         -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    | Case l (PExp l) [Alt l]           -- ^ @case@ /exp/ @of@ /alts/
    | Do l [Stmt l]                 -- ^ @do@-expression:
                                    -- the last statement in the list
                                    -- should be an expression.
    | MDo l [Stmt l]                -- ^ @mdo@-expression
--    | Tuple [PExp]              -- ^ tuple expression
    | TupleSection l [Maybe (PExp l)] -- ^ tuple section expression, e.g. @(,,3)@
    | List l [PExp l]               -- ^ list expression
    | Paren l (PExp l)                -- ^ parenthesized expression
--     RightSection QOp PExp     -- ^ right section @(@/qop/ /exp/@)@
    | RecConstr l (QName l) [PFieldUpdate l]
                                -- ^ record construction expression
    | RecUpdate l (PExp l) [PFieldUpdate l]
                                -- ^ record update expression
    | EnumFrom l (PExp l)             -- ^ unbounded arithmetic sequence,
                                    -- incrementing by 1
    | EnumFromTo l (PExp l) (PExp l)      -- ^ bounded arithmetic sequence,
                                    -- incrementing by 1
    | EnumFromThen l (PExp l) (PExp l)   -- ^ unbounded arithmetic sequence,
                                    -- with first two elements given
    | EnumFromThenTo l (PExp l) (PExp l) (PExp l)
                                -- ^ bounded arithmetic sequence,
                                    -- with first two elements given
    | ParComp l (PExp l) [[QualStmt l]]    -- ^ parallel list comprehension
    | ExpTypeSig l (PExp l) (S.Type l)
                                -- ^ expression type signature
    | AsPat l (Name l) (PExp l)           -- ^ patterns only
    | WildCard l                 -- ^ patterns only
    | IrrPat l (PExp l)               -- ^ patterns only

-- Post-ops for parsing left sections and regular patterns. Not to be left in the final tree.
    | PostOp l (PExp l) (QOp l)          -- ^ post-ops
    | PreOp l (QOp l) (PExp l)            -- ^ pre-ops

-- View patterns
    | ViewPat l (PExp l) (PExp l)         -- ^ patterns only

-- HaRP
    | SeqRP l [PExp l]              -- ^ regular patterns only
    | GuardRP l (PExp l) [Stmt l]       -- ^ regular patterns only
    | EitherRP l (PExp l) (PExp l)        -- ^ regular patterns only
    | CAsRP l (Name l) (PExp l)           -- ^ regular patterns only

-- Template Haskell
    | VarQuote l (QName l)            -- ^ 'x
    | TypQuote l (QName l)            -- ^ ''T
    | BracketExp l (Bracket l)
    | SpliceExp l (Splice l)
    | QuasiQuote l String String  -- ^ [$...|...]

-- Hsx
    | XTag  l (XName l) [ParseXAttr l] (Maybe (PExp l)) [PExp l]
    | XETag l (XName l) [ParseXAttr l] (Maybe (PExp l))
    | XPcdata l String
    | XExpTag l (PExp l)
    | XRPats l [PExp l]

-- Pragmas
    | CorePragma l      String  (PExp l)
    | SCCPragma  l      String  (PExp l)
    | GenPragma  l      String (Int, Int) (Int, Int) (PExp l)
--    | UnknownExpPragma  String String

-- Generics
    | ExplTypeArg l (QName l) (S.Type l)   -- ^ f {| Int |} x = ...

-- Bang Patterns
    | BangPat l (PExp l)              -- ^ f !a = ...

-- Arrows
    | Proc l (Pat l) (PExp l)
    | LeftArrApp      l (PExp l) (PExp l)
    | RightArrApp     l (PExp l) (PExp l)
    | LeftArrHighApp  l (PExp l) (PExp l)
    | RightArrHighApp l (PExp l) (PExp l)
   deriving (Eq,Show)

data PFieldUpdate l
    = FieldUpdate l (QName l) (PExp l)
    | FieldPun l (Name l)
    | FieldWildcard l
  deriving (Eq,Show)

data ParseXAttr l = XAttr l (XName l) (PExp l)
  deriving (Eq,Show)

instance Annotated PExp where
    ann e = case e of
        Var l qn        -> l
        IPVar l ipn     -> l
        Con l qn        -> l
        Lit l lit       -> l
        InfixApp l e1 qop e2    -> l
        App l e1 e2     -> l
        NegApp l e      -> l
        Lambda l ps e   -> l
        Let l bs e      -> l
        If l ec et ee   -> l
        Case l e alts   -> l
        Do l ss         -> l
        MDo l ss        -> l
        TupleSection l mes  -> l
        List l es       -> l
        Paren l e       -> l
        RecConstr l qn fups     -> l
        RecUpdate l e  fups     -> l
        EnumFrom l e            -> l
        EnumFromTo l ef et      -> l
        EnumFromThen l ef et    -> l
        EnumFromThenTo l ef eth eto -> l
        ParComp  l e qsss       -> l
        ExpTypeSig l e t        -> l
        AsPat l n e             -> l
        WildCard l              -> l
        IrrPat l e              -> l
        PostOp l e op           -> l
        PreOp l op e            -> l
        ViewPat l e1 e2         -> l
        SeqRP l es              -> l
        GuardRP l e ss          -> l
        EitherRP l e1 e2        -> l
        CAsRP l n e             -> l

        VarQuote l qn           -> l
        TypQuote l qn           -> l
        BracketExp l br         -> l
        SpliceExp l sp          -> l
        QuasiQuote l sn se      -> l

        XTag  l xn xas me es    -> l
        XETag l xn xas me       -> l
        XPcdata l s             -> l
        XExpTag l e             -> l
        XRPats l es             -> l

        CorePragma l s e   -> l
        SCCPragma  l s e   -> l
        GenPragma  l s n12 n34 e -> l

        ExplTypeArg l qn t      -> l
        BangPat l e             -> l

        Proc            l p e   -> l
        LeftArrApp      l e1 e2 -> l
        RightArrApp     l e1 e2 -> l
        LeftArrHighApp  l e1 e2 -> l
        RightArrHighApp l e1 e2 -> l

    amap f e = case e of
        Var l qn                -> Var   (f l) qn
        IPVar l ipn             -> IPVar (f l) ipn
        Con l qn                -> Con   (f l) qn
        Lit l lit               -> Lit   (f l) lit
        InfixApp l e1 qop e2    -> InfixApp (f l) e1 qop e2
        App l e1 e2             -> App (f l) e1 e2
        NegApp l e              -> NegApp (f l) e
        Lambda l ps e           -> Lambda (f l) ps e
        Let l bs e              -> Let (f l) bs e
        If l ec et ee           -> If (f l) ec et ee
        Case l e alts           -> Case (f l) e alts
        Do l ss                 -> Do (f l) ss
        MDo l ss                -> MDo (f l) ss
        TupleSection l mes      -> TupleSection (f l) mes
        List l es               -> List (f l) es
        Paren l e               -> Paren (f l) e
        RecConstr l qn fups     -> RecConstr (f l) qn fups
        RecUpdate l e  fups     -> RecUpdate (f l) e  fups
        EnumFrom l e            -> EnumFrom (f l) e
        EnumFromTo l ef et      -> EnumFromTo (f l) ef et
        EnumFromThen l ef et    -> EnumFromThen (f l) ef et
        EnumFromThenTo l ef eth eto -> EnumFromThenTo (f l) ef eth eto
        ParComp  l e qsss       -> ParComp  (f l) e qsss
        ExpTypeSig l e t        -> ExpTypeSig (f l) e t

        AsPat l n e             -> AsPat (f l) n e
        WildCard l              -> WildCard (f l)
        IrrPat l e              -> IrrPat (f l) e
        PostOp l e op           -> PostOp (f l) e op
        PreOp l op e            -> PreOp (f l) op e
        ViewPat l e1 e2         -> ViewPat (f l) e1 e2
        SeqRP l es              -> SeqRP (f l) es
        GuardRP l e ss          -> GuardRP (f l) e ss
        EitherRP l e1 e2        -> EitherRP (f l) e1 e2
        CAsRP l n e             -> CAsRP (f l) n e
        ExplTypeArg l n t       -> ExplTypeArg (f l) n t
        BangPat l e             -> BangPat (f l) e

        VarQuote l qn           -> VarQuote (f l) qn
        TypQuote l qn           -> TypQuote (f l) qn
        BracketExp l br         -> BracketExp (f l) br
        SpliceExp l sp          -> SpliceExp (f l) sp
        QuasiQuote l sn se      -> QuasiQuote (f l) sn se

        XTag  l xn xas me es    -> XTag  (f l) xn xas me es
        XETag l xn xas me       -> XETag (f l) xn xas me
        XPcdata l s             -> XPcdata (f l) s
        XExpTag l e             -> XExpTag (f l) e

        CorePragma l s e        -> CorePragma (f l) s e
        SCCPragma  l s e        -> SCCPragma  (f l) s e
        GenPragma  l s n12 n34 e -> GenPragma  (f l) s n12 n34 e

        Proc            l p e   -> Proc            (f l) p e
        LeftArrApp      l e1 e2 -> LeftArrApp      (f l) e1 e2
        RightArrApp     l e1 e2 -> RightArrApp     (f l) e1 e2
        LeftArrHighApp  l e1 e2 -> LeftArrHighApp  (f l) e1 e2
        RightArrHighApp l e1 e2 -> RightArrHighApp (f l) e1 e2

instance Functor PExp where
      fmap f e = case e of
          Var l qn                -> Var   (f l) (fmap f qn)
          IPVar l ipn             -> IPVar (f l) (fmap f ipn)
          Con l qn                -> Con   (f l) (fmap f qn)
          Lit l lit               -> Lit   (f l) (fmap f lit)
          InfixApp l e1 qop e2    -> InfixApp (f l) (fmap f e1) (fmap f qop) (fmap f e2)
          App l e1 e2             -> App (f l) (fmap f e1) (fmap f e2)
          NegApp l e              -> NegApp (f l) (fmap f e)
          Lambda l ps e           -> Lambda (f l) (map (fmap f) ps) (fmap f e)
          Let l bs e              -> Let (f l) (fmap f bs) (fmap f e)
          If l ec et ee           -> If (f l) (fmap f ec) (fmap f et) (fmap f ee)
          Case l e alts           -> Case (f l) (fmap f e) (map (fmap f) alts)
          Do l ss                 -> Do (f l) (map (fmap f) ss)
          MDo l ss                -> MDo (f l) (map (fmap f) ss)
          TupleSection l mes      -> TupleSection (f l) (map (fmap (fmap f)) mes)
          List l es               -> List (f l) (map (fmap f) es)
          Paren l e               -> Paren (f l) (fmap f e)
          RecConstr l qn fups     -> RecConstr (f l) (fmap f qn) (map (fmap f) fups)
          RecUpdate l e  fups     -> RecUpdate (f l) (fmap f e)  (map (fmap f) fups)
          EnumFrom l e            -> EnumFrom (f l) (fmap f e)
          EnumFromTo l ef et      -> EnumFromTo (f l) (fmap f ef) (fmap f et)
          EnumFromThen l ef et    -> EnumFromThen (f l) (fmap f ef) (fmap f et)
          EnumFromThenTo l ef eth eto -> EnumFromThenTo (f l) (fmap f ef) (fmap f eth) (fmap f eto)
          ParComp  l e qsss       -> ParComp  (f l) (fmap f e) (map (map (fmap f)) qsss)
          ExpTypeSig l e t        -> ExpTypeSig (f l) (fmap f e) (fmap f t)

          AsPat l n e             -> AsPat (f l) (fmap f n) (fmap f e)
          WildCard l              -> WildCard (f l)
          IrrPat l e              -> IrrPat (f l) (fmap f e)
          PostOp l e op           -> PostOp (f l) (fmap f e) (fmap f op)
          PreOp l op e            -> PreOp (f l) (fmap f op) (fmap f e)
          ViewPat l e1 e2         -> ViewPat (f l) (fmap f e1) (fmap f e2)
          SeqRP l es              -> SeqRP (f l) (map (fmap f) es)
          GuardRP l e ss          -> GuardRP (f l) (fmap f e) (map (fmap f) ss)
          EitherRP l e1 e2        -> EitherRP (f l) (fmap f e1) (fmap f e2)
          CAsRP l n e             -> CAsRP (f l) (fmap f n) (fmap f e)
          ExplTypeArg l n t       -> ExplTypeArg (f l) (fmap f n) (fmap f t)
          BangPat l e             -> BangPat (f l) (fmap f e)

          VarQuote l qn           -> VarQuote (f l) (fmap f qn)
          TypQuote l qn           -> TypQuote (f l) (fmap f qn)
          BracketExp l br         -> BracketExp (f l) (fmap f br)
          SpliceExp l sp          -> SpliceExp (f l) (fmap f sp)
          QuasiQuote l sn se      -> QuasiQuote (f l) sn se

          XTag  l xn xas me es    -> XTag  (f l) (fmap f xn) (map (fmap f) xas) (fmap (fmap f) me) (map (fmap f) es)
          XETag l xn xas me       -> XETag (f l) (fmap f xn) (map (fmap f) xas) (fmap (fmap f) me)
          XPcdata l s             -> XPcdata (f l) s
          XExpTag l e             -> XExpTag (f l) (fmap f e)

          CorePragma l s e        -> CorePragma (f l) s (fmap f e)
          SCCPragma  l s e        -> SCCPragma  (f l) s (fmap f e)
          GenPragma  l s n12 n34 e -> GenPragma  (f l) s n12 n34 (fmap f e)

          Proc            l p e   -> Proc            (f l) (fmap f p) (fmap f e)
          LeftArrApp      l e1 e2 -> LeftArrApp      (f l) (fmap f e1) (fmap f e2)
          RightArrApp     l e1 e2 -> RightArrApp     (f l) (fmap f e1) (fmap f e2)
          LeftArrHighApp  l e1 e2 -> LeftArrHighApp  (f l) (fmap f e1) (fmap f e2)
          RightArrHighApp l e1 e2 -> RightArrHighApp (f l) (fmap f e1) (fmap f e2)



instance Functor PFieldUpdate where
    fmap f (FieldUpdate l qn e) = FieldUpdate (f l) (fmap f qn) (fmap f e)
    fmap f (FieldPun l n)       = FieldPun (f l) (fmap f n)
    fmap f (FieldWildcard l)    = FieldWildcard (f l)

instance Annotated PFieldUpdate where
    ann (FieldUpdate l qn e) = l
    ann (FieldPun l n)       = l
    ann (FieldWildcard l)    = l
    amap f (FieldUpdate l qn e) = FieldUpdate (f l) qn e
    amap f (FieldPun l n)       = FieldPun (f l) n
    amap f (FieldWildcard l)    = FieldWildcard (f l)

instance Functor ParseXAttr where
    fmap f (XAttr l xn e) = XAttr (f l) (fmap f xn) (fmap f e)

instance Annotated ParseXAttr where
    ann (XAttr l _ _) = l
    amap f (XAttr l xn e) = XAttr (f l) xn e

p_unit_con :: l -> PExp l
p_unit_con l         = Con l (unit_con_name l)

p_tuple_con :: l -> Boxed -> Int -> PExp l
p_tuple_con l b i       = Con l (tuple_con_name l b i)

p_unboxed_singleton_con :: l -> PExp l
p_unboxed_singleton_con l = Con l (unboxed_singleton_con_name l)

data PContext l
    = CxSingle l (PAsst l)
    | CxTuple  l [PAsst l]
    | CxParen  l (PContext l)
    | CxEmpty  l
 deriving (Eq, Show)

instance Functor PContext where
  fmap f (CxSingle l asst) = CxSingle (f l) (fmap f asst)
  fmap f (CxTuple l assts) = CxTuple (f l) (map (fmap f) assts)
  fmap f (CxParen l ctxt)  = CxParen (f l) (fmap f ctxt)
  fmap f (CxEmpty l)       = CxEmpty (f l)

instance Annotated PContext where
  ann (CxSingle l asst ) = l
  ann (CxTuple  l assts) = l
  ann (CxParen  l ctxt ) = l
  ann (CxEmpty  l)       = l
  amap f (CxSingle l asst ) = CxSingle (f l) asst
  amap f (CxTuple  l assts) = CxTuple  (f l) assts
  amap f (CxParen  l ctxt ) = CxParen  (f l) ctxt
  amap f (CxEmpty l) = CxEmpty (f l)

data PType l
     = TyForall l
        (Maybe [TyVarBind l])
        (Maybe (PContext l))
        (PType l)
     | TyFun   l (PType l) (PType l)            -- ^ function type
     | TyTuple l Boxed     [PType l]            -- ^ tuple type, possibly boxed
     | TyList  l (PType l)                      -- ^ list syntax, e.g. [a], as opposed to [] a
     | TyApp   l (PType l) (PType l)            -- ^ application of a type constructor
     | TyVar   l (Name l)                       -- ^ type variable
     | TyCon   l (QName l)                      -- ^ named type or type constructor
     | TyParen l (PType l)                      -- ^ type surrounded by parentheses
     | TyPred  l (PAsst l)                      -- ^ assertion of an implicit parameter
     | TyInfix l (PType l) (QName l) (PType l)  -- ^ infix type constructor
     | TyKind  l (PType l) (Kind l)             -- ^ type with explicit kind signature
  deriving (Eq, Show)

instance Functor PType where
    fmap f t = case t of
      TyForall l mtvs mcx t         -> TyForall (f l) (fmap (map (fmap f)) mtvs) (fmap (fmap f) mcx) (fmap f t)
      TyFun   l t1 t2               -> TyFun (f l) (fmap f t1) (fmap f t2)
      TyTuple l b ts                -> TyTuple (f l) b (map (fmap f) ts)
      TyList  l t                   -> TyList (f l) (fmap f t)
      TyApp   l t1 t2               -> TyApp (f l) (fmap f t1) (fmap f t2)
      TyVar   l n                   -> TyVar (f l) (fmap f n)
      TyCon   l qn                  -> TyCon (f l) (fmap f qn)
      TyParen l t                   -> TyParen (f l) (fmap f t)
      TyPred  l asst                -> TyPred (f l) (fmap f asst)
      TyInfix l ta qn tb            -> TyInfix (f l) (fmap f ta) (fmap f qn) (fmap f tb)
      TyKind  l t k                 -> TyKind (f l) (fmap f t) (fmap f k)

instance Annotated PType where
    ann t = case t of
      TyForall l mtvs cx t          -> l
      TyFun   l t1 t2               -> l
      TyTuple l b ts                -> l
      TyList  l t                   -> l
      TyApp   l t1 t2               -> l
      TyVar   l n                   -> l
      TyCon   l qn                  -> l
      TyParen l t                   -> l
      TyInfix l ta qn tb            -> l
      TyKind  l t k                 -> l
    amap f t = case t of
      TyForall l mtvs mcx t         -> TyForall (f l) mtvs mcx t
      TyFun   l t1 t2               -> TyFun (f l) t1 t2
      TyTuple l b ts                -> TyTuple (f l) b ts
      TyList  l t                   -> TyList (f l) t
      TyApp   l t1 t2               -> TyApp (f l) t1 t2
      TyVar   l n                   -> TyVar (f l) n
      TyCon   l qn                  -> TyCon (f l) qn
      TyParen l t                   -> TyParen (f l) t
      TyInfix l ta qn tb            -> TyInfix (f l) ta qn tb
      TyKind  l t k                 -> TyKind (f l) t k

data PAsst l
    = ClassA l (QName l) [PType l]
    | InfixA l (PType l) (QName l) (PType l)
    | IParam l (IPName l) (PType l)
    | EqualP l (PType l)  (PType l)
  deriving (Eq, Show)

instance Functor PAsst where
    fmap f asst = case asst of
        ClassA l qn ts      -> ClassA (f l) (fmap f qn) (map (fmap f) ts)
        InfixA l ta qn tb   -> InfixA (f l) (fmap f ta) (fmap f qn) (fmap f tb)
        IParam l ipn t      -> IParam (f l) (fmap f ipn) (fmap f t)
        EqualP l t1 t2      -> EqualP (f l) (fmap f t1) (fmap f t2)

instance Annotated PAsst where
    ann asst = case asst of
        ClassA l qn ts      -> l
        InfixA l ta qn tb   -> l
        IParam l ipn t      -> l
        EqualP l t1 t2      -> l
    amap f asst = case asst of
        ClassA l qn ts      -> ClassA (f l) qn ts
        InfixA l ta qn tb   -> InfixA (f l) ta qn tb
        IParam l ipn t      -> IParam (f l) ipn t
        EqualP l t1 t2      -> EqualP (f l) t1 t2


unit_tycon, fun_tycon, list_tycon, unboxed_singleton_tycon :: l -> PType l
unit_tycon              l = TyCon l (unit_tycon_name l)
fun_tycon               l = TyCon l (fun_tycon_name l)
list_tycon              l = TyCon l (list_tycon_name l)
unboxed_singleton_tycon l = TyCon l (unboxed_singleton_tycon_name l)

tuple_tycon :: l -> Boxed -> Int -> PType l
tuple_tycon l b i         = TyCon l (tuple_tycon_name l b i)

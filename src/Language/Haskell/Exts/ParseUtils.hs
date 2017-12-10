{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.ParseUtils
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

module Language.Haskell.Exts.ParseUtils (
      splitTyConApp         -- PType -> P (Name,[Type])
    , checkEnabled          -- (Show e, Enabled e) => e -> P ()
    , checkEnabledOneOf
    , checkToplevel         -- ??
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
    , checkTyVar            -- Name  -> P PType
    , bangType              -- L -> BangType -> Type -> Type
    , checkKind             -- Kind -> P ()
    , checkValDef           -- SrcLoc -> PExp -> Maybe Type -> Rhs -> Binds -> P Decl
    , checkExplicitPatSyn   --
    , checkClassBody        -- [ClassDecl] -> P [ClassDecl]
    , checkInstBody         -- [InstDecl] -> P [InstDecl]
    , checkUnQual           -- QName -> P Name
    , checkQualOrUnQual     -- QName -> P QName
    , checkSingleDecl       -- [Decl] -> P Decl
    , checkRevDecls         -- [Decl] -> P [Decl]
    , checkRevClsDecls      -- [ClassDecl] -> P [ClassDecl]
    , checkRevInstDecls     -- [InstDecl] -> P [InstDecl]
    , checkDataOrNew        -- DataOrNew -> [QualConDecl] -> P ()
    , checkDataOrNewG       -- DataOrNew -> [GadtDecl] -> P ()
    , checkSimpleType       -- PType -> P (Name, [TyVarBind])
    , checkSigVar           -- PExp -> P Name
    , checkDefSigDef        -- Decl -> P Decl
    , getGConName           -- S.Exp -> P QName
    , mkTyForall            -- Maybe [TyVarBind] -> PContext -> PType -> PType
    , mkRoleAnnotDecl       --
    , mkAssocType
    , mkEThingWith
    , splitTilde
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
    -- Helpers
    , updateQNameLoc        -- l -> QName l -> QName l

    , SumOrTuple(..), mkSumOrTuple

    -- Parsed expressions and types
    , PExp(..), PFieldUpdate(..), ParseXAttr(..), PType(..), PContext, PAsst(..)
    , p_unit_con            -- PExp
    , p_tuple_con           -- Boxed -> Int -> PExp
    , p_unboxed_singleton_con   -- PExp
    , pexprToQName
    ) where

import Language.Haskell.Exts.Syntax hiding ( Type(..), Asst(..), Exp(..), FieldUpdate(..), XAttr(..), Context(..) )
import qualified Language.Haskell.Exts.Syntax as S ( Type(..), Asst(..), Exp(..), FieldUpdate(..), XAttr(..), Context(..), Role(..), PatternSynDirection(..))

import Language.Haskell.Exts.ParseSyntax
import Language.Haskell.Exts.ParseMonad
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc hiding (loc)
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.ExtScheme

import Prelude hiding (mod)
import Data.List (intercalate, intersperse)
import Data.Maybe (fromJust, fromMaybe)
import Data.Either
import Control.Monad (when,unless)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative (..), (<$>))
#endif

type L = SrcSpanInfo
type S = SrcSpan

pexprToQName :: PExp l -> P (QName l)
pexprToQName (Con _ qn) = return qn
pexprToQName (List l []) = return $ Special l (ListCon l)
pexprToQName _ = fail "pexprToQName"

splitTyConApp :: PType L -> P (Name L, [S.Type L])
splitTyConApp t0 = do
            (n, pts) <- split t0 []
            ts <- mapM checkType pts
            return (n,ts)
 where
    split :: PType L -> [PType L] -> P (Name L, [PType L])
    split (TyApp _ t u) ts = split t (u:ts)
    split (TyCon _ (UnQual _ t)) ts = return (t,ts)
    split (TyInfix l a op b) ts = split (TyCon l (getMaybePromotedQName op)) (a:b:ts)
    split _ _ = fail "Illegal data/newtype declaration"

-----------------------------------------------------------------------------
-- Checking for extensions

checkEnabled :: (Show e, Enabled e) => e  -> P ()
checkEnabled e = do
    exts <- getExtensions
    unless (isEnabled e exts) $ fail errorMsg
 where errorMsg = unwords
          [ show e
          , "language extension is not enabled."
          , "Please add {-# LANGUAGE " ++ show e ++  " #-}"
          , "pragma at the top of your module."
          ]

checkEnabledOneOf :: (Show e, Enabled e) => [e] -> P ()
checkEnabledOneOf es = do
    exts <- getExtensions
    unless (any (`isEnabled` exts) es) $
        fail errorMsg
  where errorMsg = unwords
          [ "At least one of"
          , joinOr id
          , "language extensions needs to be enabled."
          , "Please add:"
          , joinOr (\s -> "{-# LANGUAGE " ++ s ++ " #-}")
          , "language pragma at the top of your module."
          ]
        joinOr f = concat . intersperse " or "  . map (f . show) $ es

checkPatternGuards :: [Stmt L] -> P ()
checkPatternGuards [Qualifier _ _] = return ()
checkPatternGuards _ = checkEnabled PatternGuards

checkToplevel :: PExp t -> P ()
checkToplevel e = do
    exts <- getExtensions
    let isQQ = case e of
            QuasiQuote {} -> isEnabled QuasiQuotes exts
            _ -> False
    unless isQQ (checkEnabled TemplateHaskell)

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
    c <- checkAssertion t
    return $ CxSingle l (ParenA l c)
checkPContext (TyPred tp p@(EqualP {})) = do
  checkEnabledOneOf [TypeFamilies, GADTs]
  return $ CxSingle tp p

checkPContext t = do
    c <- checkAssertion t
    return $ CxSingle (ann c) c

------------------------------------------------------------------------------------------------------------------- WORKING HERE

-- Check a single assertion according to the above, still leaving
-- the class assertion parameters for later.
checkAssertion :: PType L -> P (PAsst L)
-- We cannot even get here unless ImplicitParameters is enabled.
checkAssertion (TyParen l asst) = do
    asst' <- checkAssertion asst
    return $ ParenA l asst'
checkAssertion (TyPred _ p@(IParam _ _ _)) = return p
-- We cannot even get here unless TypeFamilies or GADTs is enabled.
-- N.B.: this is called only when the equality assertion is part of a
-- tuple
checkAssertion (TyPred _ p@(EqualP _ _ _)) = return p
checkAssertion t' = checkAssertion' id [] t'
    where   -- class assertions must have at least one argument
            checkAssertion' fl ts (TyCon l c) = do
                when (length ts < 1) $ checkEnabled FlexibleContexts
                checkAndWarnTypeOperators c
                return $ ClassA (fl l) c ts
            checkAssertion' fl ts (TyApp l a t) =
                -- no check on t at this stage
                checkAssertion' (const (fl l)) (t:ts) a
            checkAssertion' fl _ (TyInfix l a op b) = do
                -- infix operators require TypeOperators
                checkAndWarnTypeOperators (getMaybePromotedQName op)
                return $ InfixA (fl l) a (getMaybePromotedQName op) b
            checkAssertion' fl ts (TyParen l t) =
                checkAssertion' (const (fl l)) ts t
            checkAssertion' fl ts (TyVar l t) = do -- Dict :: cxt => Dict cxt
                checkEnabled ConstraintKinds
                return $ AppA (fl l) t ts
            checkAssertion' _ _ (TyWildCard l wc) =
                return $ WildCardA l wc
            checkAssertion' _ _ _ = fail "Illegal class assertion"

-- Check class/instance declaration for multiparams
checkMultiParam :: PType L -> P ()
checkMultiParam = checkMultiParam' []
    where
        checkMultiParam' ts (TyCon _ _) =
            when (length ts /= 1) $ checkEnabled MultiParamTypeClasses
        checkMultiParam' ts (TyApp _ a t) = checkMultiParam' (t:ts) a
        checkMultiParam' _ (TyInfix _ _ _ _) = checkEnabled MultiParamTypeClasses
        checkMultiParam' ts (TyParen _ t) = checkMultiParam' ts t
        checkMultiParam' _ _ = return ()

getSymbol :: QName L -> Maybe String
getSymbol (UnQual _ (Symbol _ s)) = Just s
getSymbol (Qual _ _ (Symbol _ s)) = Just s
getSymbol _                       = Nothing

-- | Checks whether the parameter is a symbol, and gives a nice warning for
-- "." if ExplicitForAll/TypeOperators are not enabled.
checkAndWarnTypeOperators :: QName L -> P ()
checkAndWarnTypeOperators c =
    case getSymbol c of
        Just s | s == "."  -> checkEnabledOneOf [ExplicitForAll, TypeOperators]
               | otherwise -> checkEnabled TypeOperators
        Nothing -> return ()

-- Checks simple contexts for class and instance
-- headers. If FlexibleContexts is enabled then
-- anything goes, otherwise only tyvars are allowed.
checkSContext :: Maybe (PContext L) -> P (Maybe (S.Context L))
checkSContext (Just ctxt) = case ctxt of
    CxEmpty l -> return $ Just $ S.CxEmpty l
    CxSingle l a -> checkAsst True a >>= return . Just . S.CxSingle l
    CxTuple l as -> mapM (checkAsst True) as >>= return . Just . S.CxTuple l
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
checkContext _ = return Nothing

checkAsst :: Bool -> PAsst L -> P (S.Asst L)
checkAsst isSimple asst =
    case asst of
      ClassA l qn pts -> do
                ts <- mapM (checkAsstParam isSimple) pts
                return $ S.ClassA l qn ts
      AppA l n pts    -> do
                ts <- mapM (checkAsstParam isSimple) pts
                return $ S.AppA l n ts
      InfixA l a op b -> do
                [a',b'] <- mapM (checkAsstParam isSimple) [a,b]
                return $ S.InfixA l a' op b'
      IParam l ipn pt -> do
                t <- checkType pt
                return $ S.IParam l ipn t
      EqualP l pa pb  -> do
                a <- checkType pa
                b <- checkType pb
                return $ S.EqualP l a b
      ParenA l a      -> do
                a' <- checkAsst isSimple a
                return $ S.ParenA l a'
      WildCardA l a ->
        if isSimple then fail "Malformed Context: WildCards not allowed in simple contexts"
                    else return $ S.WildCardA l a

checkAsstParam :: Bool -> PType L -> P (S.Type L)
checkAsstParam isSimple t = do
        exts <- getExtensions
        if FlexibleContexts `elem` exts
         then checkType t
         else case t of
                TyVar l n     -> return $ S.TyVar l n
                TyWildCard l mn ->  return $ S.TyWildCard l mn
                TyParen l t1  -> do t1' <- checkAsstParam isSimple t1
                                    return $ S.TyParen l t1'
                TyApp l pf pt | not isSimple    -> do
                        f <- checkAsstParam isSimple pf
                        t' <- checkType pt
                        return $ S.TyApp l f t'
                _       -> fail "Malformed context: FlexibleContexts is not enabled"

-----------------------------------------------------------------------------
-- Checking Headers


checkDataHeader :: PType L -> P (Maybe (S.Context L), DeclHead L)
checkDataHeader (TyForall _ Nothing cs t) = do
    dh <- checkSimple "data/newtype" t
    cs' <- checkContext cs
    return (cs',dh)
checkDataHeader t = do
    dh <- checkSimple "data/newtype" t
    return (Nothing,dh)

checkClassHeader :: PType L -> P (Maybe (S.Context L), DeclHead L)
checkClassHeader (TyForall _ Nothing cs t) = do
    checkMultiParam t
    dh <- checkSimple "class" t
    cs' <- checkSContext cs
    return (cs',dh)
checkClassHeader t = do
    checkMultiParam t
    dh <- checkSimple "class" t
    return (Nothing,dh)

checkSimple :: String -> PType L -> P (DeclHead L)
--checkSimple kw (TyApp _ l t) xs | isTyVarBind t = checkSimple kw l (toTyVarBind t : xs)

checkSimple kw (TyApp l h t) = do
  tvb <- mkTyVarBind kw t
  h' <- checkSimple kw h
  return $ DHApp l h' tvb
checkSimple kw (TyInfix l t1 mq t2)
  | c@(UnQual _ t) <- getMaybePromotedQName mq
  = do
       checkAndWarnTypeOperators c
       tv1 <- mkTyVarBind kw t1
       tv2 <- mkTyVarBind kw t2
       return $ DHApp l (DHInfix l tv1 t) tv2
checkSimple _kw (TyCon _ c@(UnQual l t)) = do
    checkAndWarnTypeOperators c
    return (DHead l t)
checkSimple kw (TyParen l t) = do
    dh <- checkSimple kw t
    return (DHParen l dh)
checkSimple kw _ = fail ("Illegal " ++ kw ++ " declaration")

mkTyVarBind :: String -> PType L -> P (TyVarBind L)
mkTyVarBind _ (TyVar l n) = return $ UnkindedVar l n
mkTyVarBind _ (TyKind l (TyVar _ n) k) = return $ KindedVar l n k
mkTyVarBind _ (TyCon l c@(UnQual _ n@(Symbol _ _))) = checkAndWarnTypeOperators c >> return (UnkindedVar l n)
mkTyVarBind _ (TyKind l (TyCon _ c@(UnQual _ n@(Symbol _ _))) k) = checkAndWarnTypeOperators c >> return (KindedVar l n k)
mkTyVarBind kw _ = fail ("Illegal " ++ kw ++ " declaration")

{-
isTyVarBind :: PType L -> Bool
isTyVarBind (TyVar _ _) = True
--isTyVarBind (TyCon _ (UnQual _ n@(Symbol _ _))) = True
isTyVarBind (TyKind _ (TyVar _ _) _) = True
isTyVarBind _ = False

toTyVarBind :: PType L -> TyVarBind L
toTyVarBind (TyVar l n) = UnkindedVar l n
toTyVarBind (TyKind l (TyVar _ n) k) = KindedVar l n k
-}

checkInstHeader :: PType L -> P (InstRule L)
checkInstHeader (TyParen l t) = checkInstHeader t >>= return . IParen l
checkInstHeader (TyForall l mtvs cs t) = do
    cs' <- checkSContext cs
    checkMultiParam t
    checkInsts (Just l) mtvs cs' t
checkInstHeader t = checkMultiParam t >> checkInsts Nothing Nothing Nothing t


checkInsts :: Maybe L -> Maybe [TyVarBind L] -> Maybe (S.Context L) -> PType L -> P (InstRule L)
checkInsts _ mtvs mctxt (TyParen l t) = checkInsts Nothing mtvs mctxt t >>= return . IParen l
checkInsts l1 mtvs mctxt t = do
    t' <- checkInstsGuts t
    return $ IRule (fromMaybe (fmap ann mctxt <?+> ann t') l1) mtvs mctxt t'

checkInstsGuts :: PType L -> P (InstHead L)
checkInstsGuts (TyApp l h t) = do
    t' <- checkType t
    h' <- checkInstsGuts h
    return $ IHApp l h' t'
checkInstsGuts (TyCon l c) = do
    checkAndWarnTypeOperators c
    return $ IHCon l c
checkInstsGuts (TyInfix l a op b) = do
    checkAndWarnTypeOperators (getMaybePromotedQName op)
    [ta,tb] <- checkTypes [a,b]
    return $ IHApp l (IHInfix l ta (getMaybePromotedQName op)) tb
checkInstsGuts (TyParen l t) = checkInstsGuts t >>= return . IHParen l
checkInstsGuts _ = fail "Illegal instance declaration"

checkDeriving :: [PType L] -> P [InstRule L]
checkDeriving = mapM (checkInsts Nothing Nothing Nothing)

-----------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: PExp L -> P (Pat L)
checkPattern e = checkPat e []

checkPat :: PExp L -> [Pat L] -> P (Pat L)
checkPat (Con l c) args = do
  let l' = foldl combSpanInfo l (map ann args)
  return (PApp l' c args)
checkPat (App _ f x) args = do
    x' <- checkPat x []
    checkPat f (x':args)
checkPat (InfixApp _ l op r) args
    | op =~= QVarOp () (UnQual () (Symbol () "!")) = do
        -- We must have BangPatterns on
        checkEnabled BangPatterns
        let (e,es) = splitBang r []
        ps <- mapM checkPattern (BangPat (ann op) e:es)
        checkPat l (ps++args)
checkPat e' [] = case e' of
    Var _ (UnQual l x)   -> return (PVar l x)
    Var _ (Special l (ExprHole _)) -> return (PWildCard l)
    Lit l lit            -> return (PLit l (Signless l2) lit)
            where l2 = noInfoSpan . srcInfoSpan $ l
    InfixApp loc l op r  ->
        case op of
            QConOp _ c -> do
                    l' <- checkPat l []
                    r' <- checkPat r []
                    return (PInfixApp loc l' c r')
            QVarOp ppos (UnQual _ (Symbol _ "+")) -> do
                    checkEnabled NPlusKPatterns
                    case (l,r) of
                        (Var _ (UnQual _ n@(Ident _ _)), Lit _ (Int kpos k _)) -> do
                            let pp = srcInfoSpan ppos
                                kp = srcInfoSpan kpos
                            return (PNPlusK (loc <** [pp,kp]) n k)
                        _ -> patFail ""
            _ -> patFail ""
    TupleSection l bx mes    ->
            if Nothing `notElem` mes
             then do ps <- mapM (\e -> checkPat e []) (map fromJust mes)
                     return (PTuple l bx ps)
             else fail "Illegal tuple section in pattern"
    UnboxedSum l b a e ->
      PUnboxedSum l b a <$> checkPattern e

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
                  stripRP (RPAs l' n p) = PAsPat l' n (stripRP p)
                  stripRP (RPParen l' p) = PParen l' (stripRP p)
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
                  e1 <- checkExpr e
                  return (PViewPat l e1 p)
    RecConstr l c fs   -> do
                  fs' <- mapM checkPatField fs
                  return (PRec l c fs')
    NegApp l (Lit _ lit) ->
                  let siSign = last . srcInfoPoints $ l
                      lSign = infoSpan siSign [siSign]
                  in do
                    when (not . isNegatableLiteral $ lit) (patFail $ prettyPrint e')
                    return (PLit l (Negative lSign) lit)
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

    -- Template Haskell
    SpliceExp l e -> return $ PSplice l e
    QuasiQuote l n q -> return $ PQuasiQuote l n q

    -- BangPatterns
    BangPat l e -> do
        p <- checkPat e []
        return $ PBangPat l p

    PreOp l (QVarOp _ (UnQual _ (Symbol _ "!"))) e -> do
        checkEnabled BangPatterns
        p <- checkPat e []
        return $ PBangPat l p

    e -> patFail $ prettyPrint e

checkPat e _ = patFail $ prettyPrint e

isNegatableLiteral :: Literal a -> Bool
isNegatableLiteral (Int _ _ _) = True
isNegatableLiteral (Frac _ _ _) = True
isNegatableLiteral (PrimInt _ _ _) = True
isNegatableLiteral (PrimFloat _ _ _) = True
isNegatableLiteral (PrimDouble _ _ _) = True
isNegatableLiteral _ = False

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
checkRPattern e' = case e' of
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
        p <- checkPattern e'
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

rpOpFail :: Pretty a => a -> P b
rpOpFail sym = fail $ "Unrecognized regular pattern operator: " ++ prettyPrint sym

fixRPOpPrec :: RPat L -> RPat L
fixRPOpPrec rp' = case rp' of
    RPOp l rp rpop      -> fPrecOp rp (flip (RPOp l) rpop)
    RPEither l rp1 rp2  -> RPEither l (fixRPOpPrec rp1) (fixRPOpPrec rp2)
    RPSeq l rps         -> RPSeq l $ map fixRPOpPrec rps
    RPCAs l n rp        -> RPCAs l n $ fixRPOpPrec rp
    RPAs l n rp         -> RPAs l n $ fixRPOpPrec rp
    RPParen l rp        -> RPParen l $ fixRPOpPrec rp
    _                   -> rp'

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
mkChildrenPat ps' = mkCPAux ps' []
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
checkExpr e' = case e' of
    Var l v               -> return $ S.Var l v
    OverloadedLabel l v   -> return $ S.OverloadedLabel l v
    IPVar l v             -> return $ S.IPVar l v
    Con l c               -> return $ S.Con l c
    Lit l lit             -> return $ S.Lit l lit
    InfixApp l e1 op e2   -> check2Exprs e1 e2 (flip (S.InfixApp l) op)
    App l e1 e2           -> check2Exprs e1 e2 (S.App l)
    NegApp _ (Lit _ (PrimWord _ _ _))
                          -> fail $ "Parse error: negative primitive word literal: " ++ prettyPrint e'
    NegApp l e            -> check1Expr e (S.NegApp l)
    Lambda loc ps e       -> check1Expr e (S.Lambda loc ps)
    Let l bs e            -> check1Expr e (S.Let l bs)
    If l e1 e2 e3         -> check3Exprs e1 e2 e3 (S.If l)
    MultiIf l alts        -> return (S.MultiIf l alts)
    Case l e alts         -> do
                     e1 <- checkExpr e
                     return (S.Case l e1 alts)
    Do l stmts            -> checkDo stmts >> return (S.Do l stmts)
    MDo l stmts           -> checkDo stmts >> return (S.MDo l stmts)
    TupleSection l bx mes -> if Nothing `notElem` mes
                             then checkManyExprs (map fromJust mes) (S.Tuple l bx)
                             else do checkEnabled TupleSections
                                     mes' <- mapM mCheckExpr mes
                                     return $ S.TupleSection l bx mes'
    UnboxedSum l before after e -> S.UnboxedSum l before after <$> checkExpr e


    List l es         -> checkManyExprs es (S.List l)
    ParArray l es     -> checkManyExprs es (S.ParArray l)
    -- Since we don't parse things as left or right sections, we need to mangle them into that.
    Paren l e         -> case e of
                          PostOp _ e1 op -> check1Expr e1 (flip (S.LeftSection l) op)
                          PreOp  _ op e2 -> check1Expr e2 (S.RightSection l op)
                          _            -> check1Expr e (S.Paren l)
    RecConstr l c fields      -> do
                     fields1 <- mapM checkField fields
                     return (S.RecConstr l c fields1)
    RecUpdate l e fields      -> do
                     fields1 <- mapM checkField fields
                     e1 <- checkExpr e
                     return (S.RecUpdate l e1 fields1)
    EnumFrom l e          -> check1Expr e (S.EnumFrom l)
    EnumFromTo l e1 e2    -> check2Exprs e1 e2 (S.EnumFromTo l)
    EnumFromThen l e1 e2      -> check2Exprs e1 e2 (S.EnumFromThen l)
    EnumFromThenTo l e1 e2 e3 -> check3Exprs e1 e2 e3 (S.EnumFromThenTo l)
    ParArrayFromTo l e1 e2    -> check2Exprs e1 e2 (S.ParArrayFromTo l)
    ParArrayFromThenTo l e1 e2 e3 -> check3Exprs e1 e2 e3 (S.ParArrayFromThenTo l)
    -- a parallel list comprehension, which could be just a simple one
    ParComp l e qualss        -> do
                     e1 <- checkExpr e
                     case qualss of
                      [quals] -> return (S.ListComp l e1 quals)
                      _       -> return (S.ParComp l e1 qualss)
    ParArrayComp l e qualss        -> do
                     e1 <- checkExpr e
                     return (S.ParArrayComp l e1 qualss)
    ExpTypeSig loc e ty     -> do
                     e1 <- checkExpr e
                     return (S.ExpTypeSig loc e1 ty)

    --Template Haskell
    BracketExp l e        -> return $ S.BracketExp l e
    SpliceExp l e         -> return $ S.SpliceExp l e
    TypQuote l q          -> return $ S.TypQuote l q
    VarQuote l q          -> return $ S.VarQuote l q
    QuasiQuote l n q      -> return $ S.QuasiQuote l n q

    -- Hsx
    XTag l n attrs mattr cs -> do attrs1 <- mapM checkAttr attrs
                                  cs1 <- mapM checkExpr cs
                                  mattr1 <- maybe (return Nothing)
                                              (\e -> checkExpr e >>= return . Just)
                                              mattr
                                  return $ S.XTag l n attrs1 mattr1 cs1
    XETag l n attrs mattr   -> do attrs1 <- mapM checkAttr attrs
                                  mattr1 <- maybe (return Nothing)
                                              (\e -> checkExpr e >>= return . Just)
                                              mattr
                                  return $ S.XETag l n attrs1 mattr1
    XPcdata l p       -> return $ S.XPcdata l p
    XExpTag l e       -> do e1 <- checkExpr e
                            return $ S.XExpTag l e1
    XChildTag l es    -> do es1 <- mapM checkExpr es
                            return $ S.XChildTag l es1
    -- Pragmas
    CorePragma l s e  -> check1Expr e (S.CorePragma l s)
    SCCPragma  l s e  -> check1Expr e (S.SCCPragma l s)
    GenPragma l s xx yy e -> check1Expr e (S.GenPragma l s xx yy)
--    UnknownExpPragma n s -> return $ S.UnknownExpPragma n s

    -- Arrows
    Proc l p e              -> do e1 <- checkExpr e
                                  return $ S.Proc l p e1
    LeftArrApp l e1 e2      -> check2Exprs e1 e2 (S.LeftArrApp l)
    RightArrApp l e1 e2     -> check2Exprs e1 e2 (S.RightArrApp l)
    LeftArrHighApp l e1 e2  -> check2Exprs e1 e2 (S.LeftArrHighApp l)
    RightArrHighApp l e1 e2 -> check2Exprs e1 e2 (S.RightArrHighApp l)

    -- LamdaCase
    LCase l alts -> return $ S.LCase l alts

    -- Hole
    TypeApp l ty   -> return $ S.TypeApp l ty

    _             -> fail $ "Parse error in expression: " ++ prettyPrint e'

checkAttr :: ParseXAttr L -> P (S.XAttr L)
checkAttr (XAttr l n v) = do v' <- checkExpr v
                             return $ S.XAttr l n v'

checkDo :: [Stmt t] -> P ()
checkDo [] = fail "Parse error: Last statement in a do-block must be an expression"
checkDo [Qualifier _ _] = return ()
checkDo (_:xs) = checkDo xs

-- type signature for polymorphic recursion!!
check1Expr :: PExp L -> (S.Exp L -> a) -> P a
check1Expr e1 f = do
    e1' <- checkExpr e1
    return (f e1')

check2Exprs :: PExp L -> PExp L -> (S.Exp L -> S.Exp L -> a) -> P a
check2Exprs e1 e2 f = do
    e1' <- checkExpr e1
    e2' <- checkExpr e2
    return (f e1' e2')

check3Exprs :: PExp L -> PExp L -> PExp L -> (S.Exp L -> S.Exp L -> S.Exp L -> a) -> P a
check3Exprs e1 e2 e3 f = do
    e1' <- checkExpr e1
    e2' <- checkExpr e2
    e3' <- checkExpr e3
    return (f e1' e2' e3')

checkManyExprs :: [PExp L] -> ([S.Exp L] -> a) -> P a
checkManyExprs es f = do
    es' <- mapM checkExpr es
    return (f es')

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

checkValDef :: L -> PExp L -> Maybe (S.Type L, S) -> Rhs L -> Maybe (Binds L) -> P (Decl L)
checkValDef l lhs optsig rhs whereBinds = do
    mlhs <- isFunLhs lhs []
    let whpt = srcInfoPoints l
    case mlhs of
     Just (f,es,b,pts) -> do
            ps <- mapM checkPattern es
            let l' = l { srcInfoPoints = pts ++ whpt }
            case optsig of -- only pattern bindings can have signatures
                Nothing -> return (FunBind l $
                            if b then [Match l' f ps rhs whereBinds]
                                 else let (a:bs) = ps
                                       in [InfixMatch l' a f bs rhs whereBinds])
                Just _  -> fail "Cannot give an explicit type signature to a function binding"
     Nothing     -> do
            lhs1 <- checkPattern lhs
            let lhs' = case optsig of
                        Nothing -> lhs1
                        Just (ty, pt) -> let lp = (ann lhs1 <++> ann ty) <** [pt]
                                         in PatTypeSig lp lhs1 ty
            return (PatBind l lhs' rhs whereBinds)

-- A variable binding is parsed as a PatBind.

isFunLhs :: PExp L -> [PExp L] -> P (Maybe (Name L, [PExp L], Bool, [S]))
isFunLhs (InfixApp _ l (QVarOp loc (UnQual _ op)) r) es
    | op =~= Symbol () "!" = do
        exts <- getExtensions
        if BangPatterns `elem` exts
         then let (b,bs) = splitBang r []
                  loc' = combSpanInfo loc (ann b)
               in isFunLhs l (BangPat loc' b : bs ++ es)
         else return $ Just (op, l:r:es, False, []) -- It's actually a definition of the operator !
    | otherwise =
        let infos = srcInfoPoints loc
            op'   = amap (\s -> s { srcInfoPoints = infos }) op
        in (return $ Just (op', l:r:es, False, []))
isFunLhs (App _ (Var l (UnQual _ f)) e) es = return $ Just (f, e:es, True, srcInfoPoints l)
isFunLhs (App _ f e) es = isFunLhs f (e:es)
isFunLhs (Var _ (UnQual _ f)) es@(_:_) = return $ Just (f, es, True, [])
isFunLhs (Paren l f) es@(_:_) = do mlhs <- isFunLhs f es
                                   case mlhs of
                                    Just (f',es',b,pts) ->
                                       let [x,y] = srcInfoPoints l
                                        in return $ Just (f',es',b,x:pts++[y])
                                    _ -> return Nothing
isFunLhs _ _ = return Nothing

-- Separating between signature declarations and value definitions in
-- a post-processing step

checkSigVar :: PExp L -> P (Name L)
checkSigVar (Var _ (UnQual l n)) = return $ fmap (const l) n
checkSigVar e = fail $ "Left-hand side of type signature is not a variable: " ++ prettyPrint e

checkExplicitPatSyn :: S -> S -> ([Decl L], [S]) -> S -> P (PatternSynDirection L)
checkExplicitPatSyn whereLoc openLoc (decls, semis) closeLoc =
  let l = whereLoc <^^> closeLoc  <** ([whereLoc, openLoc] ++ semis ++ [closeLoc])
  in  S.ExplicitBidirectional l  <$> mapM checkDecls decls
  where
    checkDecls :: Decl L -> P (Decl L)
    checkDecls p@(PatBind _ pat _ _) =
      case pat of
        PApp _ _ _        -> return p
        PInfixApp _ _ _ _ ->  return p
        _ -> fail "Illegal pattern binding in PatternSynonym"
    checkDecls _                 = fail "pattern synonym 'where' clause must contain a PatBind"

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
checkMethodDef (PatBind _ (PVar _ _) _ _) = return ()
checkMethodDef (PatBind loc _ _ _) =
    fail "illegal method definition" `atSrcLoc` fromSrcInfo loc
checkMethodDef _ = return ()

checkDefSigDef :: Decl L -> P (Name L,S.Type L,S)
checkDefSigDef (TypeSig loc [name] typ) =
  let (b:_) = srcInfoPoints loc in return (name,typ,b)
checkDefSigDef (TypeSig _ _ _) =
    fail "default signature must be for a single name"
checkDefSigDef _ =
    fail "default signature must be a type signature"

-----------------------------------------------------------------------------
-- Check that an identifier or symbol is unqualified.
-- For occasions when doing this in the grammar would cause conflicts.

checkUnQual :: QName L -> P (Name L)
checkUnQual (Qual  _ _ _) = fail "Illegal qualified name"
checkUnQual (UnQual  l n) = return $ fmap (const l) n
checkUnQual (Special _ _) = fail "Illegal special name"

checkQualOrUnQual :: QName L -> P (QName L)
checkQualOrUnQual n@(Qual  _ _ _) = return n
checkQualOrUnQual n@(UnQual  _ _) = return n
checkQualOrUnQual (Special _ _)   = fail "Illegal special name"

-----------------------------------------------------------------------------
-- Check that two xml tag names are equal
checkEqNames :: XName L -> XName L -> P (XName L)
checkEqNames n@(XName _ n1) (XName _ n2)
    | n1 == n2  = return n
checkEqNames n@(XDomName _ d1 n1) (XDomName _ d2 n2)
    | n1 == n2 && d1 == d2 = return n
checkEqNames n m = fail $ "opening tag '" ++ showTag n ++
                   "' does not match closing tag '" ++ showTag m ++ "'"
    where
        showTag (XName _ n') = n'
        showTag (XDomName _ d n') = d ++ ":" ++ n'


-----------------------------------------------------------------------------
-- Miscellaneous utilities

checkPrec :: Integer -> P Int
checkPrec i | 0 <= i && i <= 9 = return (fromInteger i)
            | otherwise        = fail ("Illegal precedence " ++ show i)

mkRecConstrOrUpdate :: PExp L -> [PFieldUpdate L] -> P (PExp L)
mkRecConstrOrUpdate (Con l c) fs       = return (RecConstr l c fs)
mkRecConstrOrUpdate e         fs@(_:_) = return (RecUpdate (ann e) e fs)
mkRecConstrOrUpdate _         _        = fail "Empty record update"

updateQNameLoc :: l -> QName l -> QName l
updateQNameLoc l (Qual _ mn n) = Qual l mn n
updateQNameLoc l (UnQual _ n)  = UnQual l n
updateQNameLoc l (Special _ s) = Special l s

-----------------------------------------------------------------------------
-- For standalone top level Decl parser, check that we actually only
-- parsed one Decl. This is needed since we parse matches of the same
-- FunBind as multiple separate declarations, and merge them after.
-- This should be called *after* checkRevDecls.

checkSingleDecl :: [Decl L] -> P (Decl L)
checkSingleDecl [d] = return d
checkSingleDecl ds =
    fail $ "Expected a single declaration, found " ++ show (length ds)


-- Reverse a list of declarations, merging adjacent FunBinds of the
-- same name and checking that their arities match.

checkRevDecls :: [Decl L] -> P [Decl L]
checkRevDecls = mergeFunBinds []
    where
    mergeFunBinds revDs [] = return revDs
    mergeFunBinds revDs (FunBind l' ms1@(Match _ name ps _ _:_):ds1) =
        mergeMatches ms1 ds1 l'
        where
        arity = length ps
        mergeMatches ms' (FunBind _ ms@(Match loc name' ps' _ _:_):ds) l
            | name' =~= name = do
            ignoreArity <- getIgnoreFunctionArity
            if length ps' == arity || ignoreArity
              then mergeMatches (ms++ms') ds (loc <++> l)
              else fail ("arity mismatch for '" ++ prettyPrint name ++ "'")
                    `atSrcLoc` fromSrcInfo loc
        mergeMatches ms' ds l = mergeFunBinds (FunBind l ms':revDs) ds
    mergeFunBinds revDs (FunBind l' ims1@(InfixMatch _ _ name _ _ _:_):ds1) =
        mergeInfix ims1 ds1 l'
        where
        mergeInfix ims' (FunBind _ ims@(InfixMatch loc _ name' _ _ _:_):ds) l
            | name' =~= name =
            mergeInfix (ims++ims') ds (loc <++> l)
        mergeInfix ms' ds l = mergeFunBinds (FunBind l ms':revDs) ds
    mergeFunBinds revDs (d:ds) = mergeFunBinds (d:revDs) ds

checkRevClsDecls :: [ClassDecl L] -> P [ClassDecl L]
checkRevClsDecls = mergeClsFunBinds []
    where
    mergeClsFunBinds revDs [] = return revDs
    mergeClsFunBinds revDs (ClsDecl l' (FunBind _ ms1@(Match _ name ps _ _:_)):ds1) =
        mergeMatches ms1 ds1 l'
        where
        arity = length ps
        mergeMatches ms' (ClsDecl _ (FunBind _ ms@(Match loc name' ps' _ _:_)):ds) l
            | name' =~= name = do
            ignoreArity <- getIgnoreFunctionArity
            if length ps' == arity || ignoreArity
              then mergeMatches (ms++ms') ds (loc <++> l)
              else fail ("arity mismatch for '" ++ prettyPrint name ++ "'")
                    `atSrcLoc` fromSrcInfo loc
        mergeMatches ms' ds l = mergeClsFunBinds (ClsDecl l (FunBind l ms'):revDs) ds
    mergeClsFunBinds revDs (ClsDecl l' (FunBind _ ims1@(InfixMatch _ _ name _ _ _:_)):ds1) =
        mergeInfix ims1 ds1 l'
        where
        mergeInfix ims' (ClsDecl _ (FunBind _ ims@(InfixMatch loc _ name' _ _ _:_)):ds) l
            | name' =~= name =
            mergeInfix (ims++ims') ds (loc <++> l)
        mergeInfix ms' ds l = mergeClsFunBinds (ClsDecl l (FunBind l ms'):revDs) ds
    mergeClsFunBinds revDs (d:ds) = mergeClsFunBinds (d:revDs) ds

checkRevInstDecls :: [InstDecl L] -> P [InstDecl L]
checkRevInstDecls = mergeInstFunBinds []
    where
    mergeInstFunBinds :: [InstDecl L] -> [InstDecl L] -> P [InstDecl L]
    mergeInstFunBinds revDs [] = return revDs
    mergeInstFunBinds revDs (InsDecl l' (FunBind _ ms1@(Match _ name ps _ _:_)):ds1) =
        mergeMatches ms1 ds1 l'
        where
        arity = length ps
        mergeMatches ms' (InsDecl _ (FunBind _ ms@(Match loc name' ps' _ _:_)):ds) l
            | name' =~= name = do
            ignoreArity <- getIgnoreFunctionArity
            if length ps' == arity || ignoreArity
              then mergeMatches (ms++ms') ds (loc <++> l)
              else fail ("arity mismatch for '" ++ prettyPrint name ++ "'")
                    `atSrcLoc` fromSrcInfo loc
        mergeMatches ms' ds l = mergeInstFunBinds (InsDecl l (FunBind l ms'):revDs) ds
    mergeInstFunBinds revDs (InsDecl l' (FunBind _ ims1@(InfixMatch _ _ name _ _ _:_)):ds1) =
        mergeInfix ims1 ds1 l'
        where
        mergeInfix ims' (InsDecl _ (FunBind _ ims@(InfixMatch loc _ name' _ _ _:_)):ds) l
            | name' =~= name =
            mergeInfix (ims++ims') ds (loc <++> l)
        mergeInfix ms' ds l = mergeInstFunBinds (InsDecl l (FunBind l ms'):revDs) ds
    mergeInstFunBinds revDs (d:ds) = mergeInstFunBinds (d:revDs) ds

----------------------------------------------------------------
-- Check that newtype declarations have
-- the right number (1) of constructors

checkDataOrNew :: DataOrNew L -> [QualConDecl L] -> P ()
checkDataOrNew (DataType _) _  = return ()
checkDataOrNew (NewType _) [QualConDecl _ _ _ x] = cX x >> return ()
  where cX (ConDecl _ _ [_]) = return ()
        cX (RecDecl _ _ [_]) = return ()
        cX _ = fail "newtype declaration constructor must have exactly one parameter."
checkDataOrNew _        _  = fail "newtype declaration must have exactly one constructor."

checkDataOrNewG :: DataOrNew L -> [GadtDecl L] -> P ()
checkDataOrNewG (DataType _) _  = return ()
checkDataOrNewG (NewType _) [_] = return ()
checkDataOrNewG _        _  = fail "newtype declaration must have exactly one constructor."

checkSimpleType :: PType L -> P (DeclHead L)
checkSimpleType = checkSimple "test"

---------------------------------------
-- Check actual types

-- | Add a strictness/unpack annotation on a type.
bangType :: Maybe (L -> BangType L, S) -> Maybe (Unpackedness L) -> PType L -> PType L
bangType mstrict munpack ty =
  case (mstrict,munpack) of
    (Nothing, Just upack) -> TyBang (ann upack <++> ann ty) (NoStrictAnnot noSrcSpan) upack ty
    (Just (strict, pos), _)  ->
      TyBang (fmap ann munpack <?+> noInfoSpan pos <++> ann ty) (strict (noInfoSpan pos))
        (fromMaybe (NoUnpackPragma noSrcSpan) munpack) ty
    (Nothing, Nothing) -> ty


checkType :: PType L -> P (S.Type L)
checkType t = checkT t False

checkT :: PType L -> Bool -> P (S.Type L)
checkT t simple = case t of
    TyForall l Nothing cs pt    -> do
            when simple $ checkEnabled ExplicitForAll
            ctxt <- checkContext cs
            check1Type pt (S.TyForall l Nothing ctxt)
    TyForall l tvs cs pt -> do
            checkEnabled ExplicitForAll
            ctxt <- checkContext cs
            check1Type pt (S.TyForall l tvs ctxt)
    TyFun   l at rt   -> check2Types at rt (S.TyFun l)
    TyTuple l b pts   -> checkTypes pts >>= return . S.TyTuple l b
    TyUnboxedSum l es -> checkTypes es >>= return . S.TyUnboxedSum l
    TyList  l pt      -> check1Type pt (S.TyList l)
    TyParArray l pt   -> check1Type pt (S.TyParArray l)
    TyApp   l ft at   -> check2Types ft at (S.TyApp l)
    TyVar   l n       -> return $ S.TyVar l n
    TyCon   l n       -> do
            checkAndWarnTypeOperators n
            return $ S.TyCon l n
    TyParen l pt      -> check1Type pt (S.TyParen l)
    -- Here we know that t will be used as an actual type (and not a data constructor)
    -- so we can check that TypeOperators are enabled.
    TyInfix l at op bt -> checkAndWarnTypeOperators (getMaybePromotedQName op)
                           >> check2Types at bt (flip (S.TyInfix l) op)
    TyKind  l pt k    -> check1Type pt (flip (S.TyKind l) k)

     -- TyPred can be a valid type if ConstraintKinds is enabled, unless it is an implicit parameter, which is not a valid type
    TyPred _ (ClassA l className cvars) -> mapM checkType cvars >>= \vars -> return (foldl1 (S.TyApp l) (S.TyCon l className:vars))
    TyPred _ (InfixA l t0 op t1)        -> S.TyInfix l <$> checkType t0 <*> pure (UnpromotedName (ann op) op) <*> checkType t1
    TyPred _ (EqualP l t0    t1)        -> do
      checkEnabledOneOf [TypeFamilies, GADTs]
      S.TyEquals l <$> checkType t0 <*> checkType t1

    TyPromoted l p -> return $ S.TyPromoted l p -- ??
    TySplice l s        -> do
                              checkEnabled TemplateHaskell
                              return $ S.TySplice l s
    TyBang l b u t' -> check1Type t' (S.TyBang l b u)
    TyWildCard l mn -> return $ S.TyWildCard l mn
    TyQuasiQuote l n s -> do
                              checkEnabled QuasiQuotes
                              return $ S.TyQuasiQuote l n s
    _   -> fail $ "Parse error in type: " ++ prettyPrint t

getMaybePromotedQName :: MaybePromotedName l -> QName l
getMaybePromotedQName (PromotedName _ q) = q
getMaybePromotedQName (UnpromotedName _ q) = q

check1Type :: PType L -> (S.Type L -> S.Type L) -> P (S.Type L)
check1Type pt f = checkT pt True >>= return . f

check2Types :: PType L -> PType L -> (S.Type L -> S.Type L -> S.Type L) -> P (S.Type L)
check2Types at bt f = checkT at True >>= \a -> checkT bt True >>= \b -> return (f a b)

checkTypes :: [PType L] -> P [S.Type L]
checkTypes = mapM (flip checkT True)

checkTyVar ::  Name L -> P (PType L)
checkTyVar n = do
  e <- getExtensions
  return $
    case n of
      Ident il ('_':ident) | NamedWildCards `elem` e ->
        TyWildCard il (Just (Ident (reduceSrcSpanInfo il) ident))
      _ ->
        TyVar (ann n) n
  where
    -- Reduces the length of the SrcSpanInfo by 1 so that it just covers the identifier.
    reduceSrcSpanInfo spaninfo =
      let ss = srcInfoSpan spaninfo
          ss' = ss { srcSpanStartColumn = srcSpanStartColumn ss + 1 }
      in  spaninfo { srcInfoSpan = ss' }
---------------------------------------
-- Check kinds

-- ConstraintKinds allow the kind "Constraint", but not "Nat", etc. Specifically
-- test for that.
checkKind :: Kind l -> P ()
checkKind k = case k of
        KindVar _ q | constrKind q -> checkEnabledOneOf [ConstraintKinds, DataKinds]
            where constrKind name = case name of
                    (UnQual _ (Ident _ n)) -> n == "Constraint"
                    _                      -> False

        _ -> checkEnabled DataKinds

---------------------------------------
-- Converting a complete page

checkPageModule :: PExp L -> ([ModulePragma L],[S],L) -> P (Module L)
checkPageModule xml (os,ss,inf) = do
    mod <- getModuleName
    xml' <- checkExpr xml
    case xml' of
        S.XTag  l xn ats mattr cs -> return $ XmlPage (inf<++>l<**(srcInfoPoints l ++ ss)) (ModuleName l mod) os xn ats mattr cs
        S.XETag l xn ats mattr    -> return $ XmlPage (inf<++>l<**(srcInfoPoints l ++ ss)) (ModuleName l mod) os xn ats mattr []
        _ -> fail "Unexpected expression; tag is expected"

checkHybridModule :: PExp L -> Module L -> S -> S -> P (Module L)
checkHybridModule xml (Module inf mh os is ds) s1 s2 = do
    xml' <- checkExpr xml
    case xml' of
        S.XTag  l xn ats mattr cs -> return $ XmlHybrid (inf<++>l<**(s1 : srcInfoPoints inf ++ s2 : srcInfoPoints l))
                                                mh os is ds xn ats mattr cs
        S.XETag l xn ats mattr    -> return $ XmlHybrid (inf<++>l<**(s1 : srcInfoPoints inf ++ s2 : srcInfoPoints l))
                                                mh os is ds xn ats mattr []
        _ -> fail "Unexpected expression; tag is expected"
checkHybridModule _ _ _ _ = fail "Hybrid module expected"

---------------------------------------
-- Handle dash-identifiers

mkDVar :: [String] -> String
mkDVar = intercalate "-"

---------------------------------------
-- Combine adjacent for-alls.
--
-- A valid type must have one for-all at the top of the type, or of the fn arg types

mkTyForall :: L -> Maybe [TyVarBind L] -> Maybe (PContext L) -> PType L -> PType L
mkTyForall l mtvs ctxt ty =
    case (ctxt, ty) of
        (Nothing, TyForall _ Nothing ctxt2 ty2) -> TyForall l mtvs ctxt2 ty2
        _                                       -> TyForall l mtvs ctxt ty

-- Make a role annotation

mkRoleAnnotDecl ::  S -> S -> QName L -> [(Maybe String, L)] -> P (Decl L)
mkRoleAnnotDecl l1 l2 tycon roles
  = do roles' <- mapM parse_role roles
       return (RoleAnnotDecl loc' tycon roles')
  where
    loc' =
      case roles of
        [] -> (l1 <^^> l2 <++> ann tycon) <** [l1, l2]
        _  -> (l1 <^^> l2 <++> ann tycon <++> foldl1 (<++>) (map snd roles)) <** [l1, l2]
    possible_roles = [ ("phantom", S.Phantom)
                     , ("representational", S.Representational)
                     , ("nominal", S.Nominal)]

    parse_role (Nothing, loc_role) = return $ S.RoleWildcard loc_role
    parse_role (Just role, loc_role)
      = case lookup role possible_roles of
          Just found_role -> return $ found_role loc_role
          Nothing         ->
            fail ("Illegal role name " ++ role)




mkAssocType :: S -> PType L -> (Maybe (ResultSig L), Maybe (S, S.Type L), Maybe (InjectivityInfo L)) -> P (ClassDecl L)
mkAssocType tyloc ty (mres, mty, minj)  =
  case (mres,mty, minj) of
    -- No additional information
    (Nothing, Nothing, Nothing) -> do
      dh <- checkSimpleType ty
      return $ ClsTyFam (noInfoSpan tyloc <++> ann ty) dh Nothing Nothing
    -- Type default
    (_, Just (eqloc, rhsty), Nothing) -> do
      ty' <- checkType ty
      let tyeq = TypeEqn (ann ty <++> ann rhsty <** [eqloc]) ty' rhsty
      return $ ClsTyDef (noInfoSpan tyloc <++> ann ty <** [tyloc]) tyeq
    -- Declaration with kind sig
    (Just ressig, _, _) -> do
      dh <- checkSimpleType ty
      return $ ClsTyFam (noInfoSpan tyloc <++> ann ressig <** [tyloc]) dh (Just ressig) Nothing
    -- Decl with inj info
    (Nothing, Just (eqloc, rhsty), Just injinfo) -> do
      ressig <- checkKTyVar eqloc rhsty
      dh <- checkSimpleType ty
      return $ ClsTyFam (noInfoSpan tyloc <++> ann injinfo <** [tyloc]) dh (Just ressig) minj
    _ -> error "mkAssocType"

  where
    checkKTyVar :: S -> S.Type L -> P (ResultSig L)
    checkKTyVar eqloc rhsty =
      case rhsty of
       S.TyVar l n -> return $ TyVarSig (noInfoSpan eqloc <++> l <** [eqloc]) (UnkindedVar l n)
       S.TyKind l (S.TyVar _ n) k -> return $ TyVarSig (noInfoSpan eqloc <++> l <** [eqloc]) (KindedVar l n k)
       _ -> fail ("Result of type family must be a type variable")

-- | Transform btype with strict_mark's into HsEqTy's
-- (((~a) ~b) c) ~d ==> ((~a) ~ (b c)) ~ d
splitTilde :: PType L -> PType L
splitTilde t = go t
  where go (TyApp loc t1 t2)
          | TyBang _ (LazyTy eqloc) (NoUnpackPragma _) t2' <- t2
          = TyPred loc (EqualP (loc <** [srcInfoSpan eqloc]) (go t1) t2')
          | otherwise
          = case go t1 of
              (TyPred _ (EqualP eqloc tl tr)) ->
                TyPred loc (EqualP (eqloc <++> ann t2 <** srcInfoPoints eqloc) tl (TyApp (ann tr <++> ann t2) tr t2))
              t' -> TyApp loc t' t2

        go t' = t'

-- Expects the arguments in the right order
mkEThingWith :: L -> QName L -> [Either S (CName L)] -> P (ExportSpec L)
mkEThingWith loc qn mcns = do
  when (isWc wc && not (null cnames)) (checkEnabled PatternSynonyms)
  return $ EThingWith loc wc qn cnames
  where
    isWc (NoWildcard {}) = False
    isWc _ = True

    wc :: EWildcard L
    wc = maybe (NoWildcard noSrcSpan)
               (\(n,Left s) -> EWildcard (noInfoSpan s) n)
               (findWithIndex 0 checkLeft mcns)

    checkLeft :: Either a b -> Bool
    checkLeft (Left _) = True
    checkLeft _ = False

    cnames = rights mcns

    findWithIndex :: Int -> (a -> Bool) -> [a] -> Maybe (Int, a)
    findWithIndex _ _ [] = Nothing
    findWithIndex n p (x:xs)
      | p x = Just (n, x)
      | otherwise = findWithIndex (n + 1) p xs

data SumOrTuple l = SSum Int Int (PExp l)
                  | STuple [Maybe (PExp l)]

mkSumOrTuple :: Boxed -> L -> SumOrTuple L -> P (PExp L)
mkSumOrTuple Unboxed s (SSum before after e) = return (UnboxedSum s before after e)
mkSumOrTuple boxity s (STuple ms) =
    return $ TupleSection s boxity ms
mkSumOrTuple Boxed _s (SSum {}) = fail "Boxed sums are not implemented"

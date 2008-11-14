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
      splitTyConApp     -- Type -> P (Name,[Type])
    , mkRecConstrOrUpdate   -- Exp -> [FieldUpdate] -> P Exp
    , checkPrec         -- Integer -> P Int
    , checkContext      -- Type -> P Context
    , checkAssertion    -- Type -> P Asst
    , checkDataHeader   -- Type -> P (Context,Name,[Name])
    , checkClassHeader  -- Type -> P (Context,Name,[Name])
    , checkInstHeader   -- Type -> P (Context,QName,[Type])
    , checkPattern      -- Exp -> P Pat
    , checkExpr         -- Exp -> P Exp
    , checkStmt         -- Stmt -> P Stmt
    , checkValDef       -- SrcLoc -> Exp -> Rhs -> [Decl] -> P Decl
    , checkClassBody    -- [ClassDecl] -> P [ClassDecl]
    , checkInstBody     -- [InstDecl] -> P [InstDecl]
    , checkUnQual       -- QName -> P Name
    , checkRevDecls     -- [Decl] -> P [Decl]
    , checkRevClsDecls  -- [ClassDecl] -> P [ClassDecl]
    , checkRevInstDecls -- [InstDecl] -> P [InstDecl]
    , checkDataOrNew    -- DataOrNew -> [Decl] -> P ()
    , checkSimpleType   -- Type -> P ()
    , getGConName       -- Exp -> P QName
    , mkTyForall      -- Maybe [Name] -> Context -> Type -> Type 
    -- HaRP
    , checkRPattern     -- Exp -> P RPat
    -- Hsx
    , checkEqNames      -- XName -> XName -> P XName
    , mkPageModule      -- Exp -> P Module
    , mkPage        -- Module -> SrcLoc -> Exp -> P Module
    , mkDVar        -- [String] -> String
    , mkDVarExpr        -- [String] -> Exp
    ) where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.ParseMonad
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Build

import Data.List (intersperse)

splitTyConApp :: Type -> P (Name,[Type])
splitTyConApp t0 = split t0 []
 where
    split :: Type -> [Type] -> P (Name,[Type])
    split (TyApp t u) ts = split t (u:ts)
    split (TyCon (UnQual t)) ts = return (t,ts)
    split (TyInfix a op b) ts = split (TyCon op) (a:b:ts)
    split _ _ = fail "Illegal data/newtype declaration"

-----------------------------------------------------------------------------
-- Various Syntactic Checks

checkContext :: Type -> P Context
checkContext (TyTuple Boxed ts) =
    mapM checkAssertion ts
checkContext t = do
    c <- checkAssertion t
    return [c]

-- Changed for multi-parameter type classes.
-- Further changed for implicit parameters.

checkAssertion :: Type -> P Asst
checkAssertion (TyPred p@(IParam _ _)) = return p
checkAssertion (TyPred p@(EqualP _ _)) = return p
checkAssertion t = checkAssertion' [] t
    where   checkAssertion' ts (TyCon c) = return $ ClassA c ts
            checkAssertion' ts (TyApp a t) = checkAssertion' (t:ts) a
            checkAssertion' ts (TyInfix a op b) = checkAssertion' (a:b:ts) (TyCon op)
            checkAssertion' _ _ = fail "Illegal class assertion"


checkDataHeader :: Type -> P (Context,Name,[Name])
checkDataHeader (TyForall Nothing cs t) = do
    (c,ts) <- checkSimple "data/newtype" t []
    return (cs,c,ts)
checkDataHeader t = do
    (c,ts) <- checkSimple "data/newtype" t []
    return ([],c,ts)

checkClassHeader :: Type -> P (Context,Name,[Name])
checkClassHeader (TyForall Nothing cs t) = do
    (c,ts) <- checkSimple "class" t []
    return (cs,c,ts)
checkClassHeader t = do
    (c,ts) <- checkSimple "class" t []
    return ([],c,ts)

checkSimple :: String -> Type -> [Name] -> P (Name,[Name])
checkSimple kw (TyApp l (TyVar a)) xs = checkSimple kw l (a:xs)
checkSimple _  (TyInfix (TyVar a) (UnQual t) (TyVar b)) xs = return (t,a:b:xs)
checkSimple _kw (TyCon (UnQual t))   xs = return (t,xs)
checkSimple kw _ _ = fail ("Illegal " ++ kw ++ " declaration")

checkInstHeader :: Type -> P (Context,QName,[Type])
checkInstHeader (TyForall Nothing cs t) = do
    (c,ts) <- checkInsts t []
    return (cs,c,ts)
checkInstHeader t = do
    (c,ts) <- checkInsts t []
    return ([],c,ts)
    

checkInsts :: Type -> [Type] -> P ((QName,[Type]))
checkInsts (TyApp l t) ts = checkInsts l (t:ts)
checkInsts (TyCon c)   ts = return (c,ts)
checkInsts _ _ = fail "Illegal instance declaration"

{-
checkInst :: Type -> P ()
checkInst (TyApp l _) = checkInst l
checkInst (TyVar _)   = fail "Illegal instance declaration"
checkInst _             = return ()
-}

-----------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: Exp -> P Pat
checkPattern e = checkPat e []

checkPat :: Exp -> [Pat] -> P Pat
checkPat (Con c) args = return (PApp c args)
checkPat (App f x) args = do
    x <- checkPat x []
    checkPat f (x:args)
checkPat e [] = case e of
    Var (UnQual x)   -> return (PVar x)
    Lit l            -> return (PLit l)
    InfixApp l op r  -> do
                  l <- checkPat l []
                  r <- checkPat r []
                  case op of
                    QConOp c -> return (PInfixApp l c r)
                    _ -> patFail ""
    Tuple es         -> do
                  ps <- mapM (\e -> checkPat e []) es
                  return (PTuple ps)
    List es      -> do
                  ps <- mapM checkRPattern es
                  return $ if all isStdPat ps
                            then PList $ map stripRP ps
                            else PRPat $ map fixRPOpPrec ps
            where isStdPat :: RPat -> Bool
                  isStdPat (RPPat _) = True
                  isStdPat (RPAs _ p) = isStdPat p
                  isStdPat _           = False
                  stripRP :: RPat -> Pat
                  stripRP (RPPat  p) = p
                  stripRP (RPAs n p) = PAsPat n (stripRP p)
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
    RecConstr c fs   -> do
                  fs <- mapM checkPatField fs
                  return (PRec c fs)
    NegApp (Lit l) -> return (PNeg (PLit l))
    ExpTypeSig s e t -> do
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
    e -> patFail $ show e

checkPat e _ = patFail $ show e

checkPatField :: FieldUpdate -> P PatField
checkPatField (FieldUpdate n e) = do
    p <- checkPat e []
    return (PFieldPat n p)

checkPAttr :: XAttr -> P PXAttr
checkPAttr (XAttr n v) = do p <- checkPat v []
                            return $ PXAttr n p

patFail :: String -> P a
patFail s = fail $ "Parse error in pattern: " ++ s

checkRPattern :: Exp -> P RPat
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

checkExpr :: Exp -> P Exp
checkExpr e = case e of
    Var _               -> return e
    IPVar _             -> return e
    Con _               -> return e
    Lit _               -> return e
    InfixApp e1 op e2   -> check2Exprs e1 e2 (flip InfixApp op)
    App e1 e2           -> check2Exprs e1 e2 App
    NegApp e            -> check1Expr e NegApp
    Lambda loc ps e     -> check1Expr e (Lambda loc ps)
    Let bs e            -> check1Expr e (Let bs)
--    DLet bs e           -> check1Expr e (DLet bs)
--    With e bs           -> check1Expr e (flip With bs)
    If e1 e2 e3         -> check3Exprs e1 e2 e3 If
    Case e alts         -> do
                     alts <- mapM checkAlt alts
                     e <- checkExpr e
                     return (Case e alts)
    Do stmts        -> do
                     stmts <- mapM checkStmt stmts
                     return (Do stmts)
    MDo stmts       -> do
                     stmts <- mapM checkStmt stmts
                     return (MDo stmts)
    Tuple es        -> checkManyExprs es Tuple
    List es         -> checkManyExprs es List
    -- Since we don't parse things as left sections, we need to mangle them into that.
    Paren e         -> case e of
                          PostOp e1 op -> check1Expr e1 (flip LeftSection op)
                          _              -> check1Expr e Paren
    --LeftSection e op    -> check1Expr e (flip LeftSection op)
    RightSection op e   -> check1Expr e (RightSection op)
    RecConstr c fields      -> do
                     fields <- mapM checkField fields
                     return (RecConstr c fields)
    RecUpdate e fields      -> do
                     fields <- mapM checkField fields
                     e <- checkExpr e
                     return (RecUpdate e fields)
    EnumFrom e          -> check1Expr e EnumFrom
    EnumFromTo e1 e2    -> check2Exprs e1 e2 EnumFromTo
    EnumFromThen e1 e2      -> check2Exprs e1 e2 EnumFromThen
    EnumFromThenTo e1 e2 e3 -> check3Exprs e1 e2 e3 EnumFromThenTo
    ListComp e stmts        -> do
                     --stmts <- mapM checkStmt stmts
                     e <- checkExpr e
                     return (ListComp e stmts)
    ExpTypeSig loc e ty     -> do
                     e <- checkExpr e
                     return (ExpTypeSig loc e ty)
    
    --Template Haskell
--    ReifyExp _          -> return e
    BracketExp _        -> return e
    SpliceExp _         -> return e
    TypQuote _          -> return e
    VarQuote _          -> return e
    
    -- Hsx
    XTag s n attrs mattr cs -> do attrs <- mapM checkAttr attrs
                                  cs <- mapM checkExpr cs
                                  mattr <- maybe (return Nothing) 
                                              (\e -> checkExpr e >>= return . Just) 
                                              mattr                 
                                  return $ XTag s n attrs mattr cs
    XETag s n attrs mattr   -> do attrs <- mapM checkAttr attrs
                                  mattr <- maybe (return Nothing) 
                                              (\e -> checkExpr e >>= return . Just) 
                                              mattr                 
                                  return $ XETag s n attrs mattr 
    XPcdata _       -> return e
    XExpTag e       -> do e <- checkExpr e
                          return $ XExpTag e
    _             -> fail $ "Parse error in expression: " ++ show e

checkAttr :: XAttr -> P XAttr
checkAttr (XAttr n v) = do v <- checkExpr v
                           return $ XAttr n v

-- type signature for polymorphic recursion!!
check1Expr :: Exp -> (Exp -> a) -> P a
check1Expr e1 f = do
    e1 <- checkExpr e1
    return (f e1)

check2Exprs :: Exp -> Exp -> (Exp -> Exp -> a) -> P a
check2Exprs e1 e2 f = do
    e1 <- checkExpr e1
    e2 <- checkExpr e2
    return (f e1 e2)

check3Exprs :: Exp -> Exp -> Exp -> (Exp -> Exp -> Exp -> a) -> P a
check3Exprs e1 e2 e3 f = do
    e1 <- checkExpr e1
    e2 <- checkExpr e2
    e3 <- checkExpr e3
    return (f e1 e2 e3)

checkManyExprs :: [Exp] -> ([Exp] -> a) -> P a
checkManyExprs es f = do
    es <- mapM checkExpr es
    return (f es)

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

checkField :: FieldUpdate -> P FieldUpdate
checkField (FieldUpdate n e) = check1Expr e (FieldUpdate n)

getGConName :: Exp -> P QName
getGConName (Con n) = return n
getGConName (List []) = return list_cons_name
getGConName _ = fail "Expression in reification is not a name"

-----------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef :: SrcLoc -> Exp -> Rhs -> Binds -> P Decl
checkValDef srcloc lhs rhs whereBinds =
    case isFunLhs lhs [] of
     Just (f,es) -> do
            ps <- mapM checkPattern es
            return (FunBind [Match srcloc f ps rhs whereBinds])
     Nothing     -> do
            lhs <- checkPattern lhs
            return (PatBind srcloc lhs rhs whereBinds)

-- A variable binding is parsed as an PatBind.

isFunLhs :: Exp -> [Exp] -> Maybe (Name, [Exp])
isFunLhs (InfixApp l (QVarOp (UnQual op)) r) es = Just (op, l:r:es)
isFunLhs (App (Var (UnQual f)) e) es = Just (f, e:es)
isFunLhs (App (Paren f) e) es = isFunLhs f (e:es)
isFunLhs (App f e) es = isFunLhs f (e:es)
isFunLhs _ _ = Nothing

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
checkMethodDef (PatBind _ (PVar _) _ _) = return ()
checkMethodDef (PatBind loc _ _ _) =
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

mkRecConstrOrUpdate :: Exp -> [FieldUpdate] -> P Exp
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
    mergeFunBinds revDs (FunBind ms1@(Match _ name ps _ _:_):ds1) =
        mergeMatches ms1 ds1
        where
        arity = length ps
        mergeMatches ms' (FunBind ms@(Match loc name' ps' _ _:_):ds)
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
    mergeClsFunBinds revDs (ClsDecl (FunBind ms1@(Match _ name ps _ _:_)):ds1) =
        mergeMatches ms1 ds1
        where
        arity = length ps
        mergeMatches ms' (ClsDecl (FunBind ms@(Match loc name' ps' _ _:_)):ds)
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
    mergeInstFunBinds revDs (InsDecl (FunBind ms1@(Match _ name ps _ _:_)):ds1) =
        mergeMatches ms1 ds1
        where
        arity = length ps
        mergeMatches ms' (InsDecl (FunBind ms@(Match loc name' ps' _ _:_)):ds)
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

checkSimpleType :: Type -> P (Name, [Name])
checkSimpleType t = checkSimple "test" t []

---------------------------------------
-- Converting a complete page

pageFun :: SrcLoc -> Exp -> Decl
pageFun loc e = PatBind loc namePat rhs (BDecls [])
    where namePat = PVar $ Ident "page"
          rhs = UnGuardedRhs e

mkPage :: Module -> SrcLoc -> Exp -> P Module
mkPage (Module src md exps imps decls) loc xml = do
    let page = pageFun loc xml
    return $ Module src md exps imps (decls ++ [page])
    
mkPageModule :: Exp -> P Module
mkPageModule xml = do 
    do loc <- case xml of 
           XTag l _ _ _ _ -> return l
           XETag l _ _ _  -> return l
           _ -> fail "Will not happen since mkPageModule is only called on XML expressions"
       mod <- getModuleName
       return $ (Module 
              loc
              (ModuleName mod)
              (Just [EVar $ UnQual $ Ident "page"])
              []
              [pageFun loc xml])

---------------------------------------
-- Handle dash-identifiers

mkDVar :: [String] -> String
mkDVar = concat . intersperse "-"

mkDVarExpr :: [String] -> Exp
mkDVarExpr = foldl1 (\x y -> infixApp x (op $ sym "-") y) . map (var . name)

---------------------------------------
-- Combine adjacent for-alls. 
--
-- A valid type must have one for-all at the top of the type, or of the fn arg types

mkTyForall :: Maybe [TyVarBind] -> Context -> Type -> Type
mkTyForall mtvs []   ty = mk_forall_ty mtvs ty
mkTyForall mtvs ctxt ty = TyForall mtvs ctxt ty

-- mk_forall_ty makes a pure for-all type (no context)
mk_forall_ty (Just []) ty             = ty  -- Explicit for-all with no tyvars
mk_forall_ty mtvs1     (TyForall mtvs2 ctxt ty) = mkTyForall (mtvs1 `plus` mtvs2) ctxt ty
mk_forall_ty mtvs1     ty             = TyForall mtvs1 [] ty

mtvs1       `plus` Nothing     = mtvs1
Nothing     `plus` mtvs2       = mtvs2 
(Just tvs1) `plus` (Just tvs2) = Just (tvs1 ++ tvs2)

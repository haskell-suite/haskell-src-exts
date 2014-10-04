-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Annotated.Fixity
-- Copyright   :  (c) Niklas Broberg 2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Fixity information to give the parser so that infix operators can
-- be parsed properly.
--
-----------------------------------------------------------------------------
module Language.Haskell.Exts.Annotated.Fixity
    (
    -- * Fixity representation
      Fixity(..)
    -- | The following three functions all create lists of
    --   fixities from textual representations of operators.
    --   The intended usage is e.g.
    --
    -- > fixs = infixr_ 0  ["$","$!","`seq`"]
    --
    --   Note that the operators are expected as you would
    --   write them infix, i.e. with ` characters surrounding
    --   /varid/ operators, and /varsym/ operators written as is.
    , infix_, infixl_, infixr_
    -- ** Collections of fixities
    , preludeFixities, baseFixities

    -- * Applying fixities to an AST
    , AppFixity(..)
    ) where

import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.SrcLoc

import Language.Haskell.Exts.Fixity ( Fixity(..), infix_, infixl_, infixr_, preludeFixities, baseFixities, prefixMinusFixity )
import qualified Language.Haskell.Exts.Syntax as S ( Assoc(..), QOp(..), QName(..), Name(..), SpecialCon(..), ModuleName )
import Language.Haskell.Exts.Annotated.Simplify ( sQOp, sAssoc, sQName, sModuleHead, sName )

import Control.Monad (when, (<=<), liftM, liftM2, liftM3, liftM4)
import Data.Traversable (mapM)
import Prelude hiding (mapM)

-- | All AST elements that may include expressions which in turn may
--   need fixity tweaking will be instances of this class.
class AppFixity ast where
  -- | Tweak any expressions in the element to account for the
  --   fixities given. Assumes that all operator expressions are
  --   fully left associative chains to begin with.
  applyFixities :: Monad m => [Fixity]      -- ^ The fixities to account for.
                    -> ast SrcSpanInfo      -- ^ The element to tweak.
                    -> m (ast SrcSpanInfo)  -- ^ The same element, but with operator expressions updated, or a failure.


instance AppFixity Exp where
  applyFixities fixs' = infFix fixs' <=< leafFix fixs'
    where -- This is the real meat case. We can assume a left-associative list to begin with.
          infFix fixs (InfixApp l2 a op2 z) = do
              e <- infFix fixs a
              let fixup (a1,p1) (a2,p2) y pre = do
                      when (p1 == p2 && (a1 /= a2 || a1 == S.AssocNone)) -- Ambiguous infix expression!
                           $ fail "Ambiguous infix expression"
                      if (p1 > p2 || p1 == p2 && (a1 == S.AssocLeft || a2 == S.AssocNone)) -- Already right order
                       then return $ InfixApp l2 e op2 z
                       else liftM pre (infFix fixs $ InfixApp (ann y <++> ann z) y op2 z)
              case e of
               InfixApp _ x op1 y -> fixup (askFixity fixs op1) (askFixity fixs op2) y (InfixApp l2 x op1)
               NegApp   _       y -> fixup prefixMinusFixity    (askFixity fixs op2) y (NegApp l2)
               _  -> return $ InfixApp l2 e op2 z

          infFix _ e = return e

--ambOps l = ParseFailed (getPointLoc l) $ "Ambiguous infix expression"

instance AppFixity Pat where
  applyFixities fixs' = infFix fixs' <=< leafFixP fixs'
    where -- This is the real meat case. We can assume a left-associative list to begin with.
          infFix fixs (PInfixApp l2 a op2 z) = do
              p <- infFix fixs a
              let fixup (a1,p1) (a2,p2) y pre = do
                      when (p1 == p2 && (a1 /= a2 || a1 == S.AssocNone )) -- Ambiguous infix expression!
                           $ fail "Ambiguous infix expression"
                      if (p1 > p2 || p1 == p2 && (a1 == S.AssocLeft || a2 == S.AssocNone)) -- Already right order
                       then return $ PInfixApp l2 p op2 z
                       else liftM pre (infFix fixs $ PInfixApp (ann y <++> ann z) y op2 z)
              case p of
               PInfixApp _ x op1 y -> fixup (askFixityP fixs op1) (askFixityP fixs op2) y (PInfixApp l2 x op1)
               _  -> return $ PInfixApp l2 p op2 z

          infFix _ p = return p

-- Internal: lookup associativity and precedence of an operator
askFixity :: [Fixity] -> QOp l -> (S.Assoc, Int)
askFixity xs k = askFix xs (f $ sQOp k) -- undefined -- \k -> askFixityP xs (f k) -- lookupWithDefault (AssocLeft, 9) (f k) mp
    where
        f (S.QVarOp x) = g x
        f (S.QConOp x) = g x

        g (S.Special S.Cons) = S.UnQual (S.Symbol ":")
        g x                  = x

-- Same using patterns
askFixityP :: [Fixity] -> QName l -> (S.Assoc, Int)
askFixityP xs qn = askFix xs (g $ sQName qn)
    where
        g (S.Special S.Cons) = S.UnQual (S.Symbol ":")
        g x                  = x

askFix :: [Fixity] -> S.QName -> (S.Assoc, Int)
askFix xs = \k -> lookupWithDefault (S.AssocLeft, 9) k mp
    where
        lookupWithDefault def k mp' = case lookup k mp' of
            Nothing -> def
            Just x  -> x

        mp = [(x,(a,p)) | Fixity a p x <- xs]


-------------------------------------------------------------------
-- Boilerplate - yuck!! Everything below here is internal stuff

instance AppFixity Module where
    applyFixities fixs (Module l mmh prs imp decls) =
        liftM (Module l mmh prs imp) $ appFixDecls (Just mn) fixs decls
      where (mn, _, _) = sModuleHead mmh
    applyFixities fixs (XmlPage l mn os xn xas mexp cs) =
        liftM3 (XmlPage l mn os xn) (fix xas) (fix mexp) (fix cs)
      where fix xs = mapM (applyFixities fixs) xs
    applyFixities fixs (XmlHybrid l mmh prs imp decls xn xas mexp cs) =
        liftM4 (flip (XmlHybrid l mmh prs imp) xn) (appFixDecls (Just mn) fixs decls)
                (fixe xas) (fixe mexp) (fixe cs)
      where fixe xs = let extraFixs = getFixities (Just mn) decls
                       in mapM (applyFixities (fixs++extraFixs)) xs
            (mn, _, _) = sModuleHead mmh

instance AppFixity Decl where
    applyFixities fixs decl = case decl of
        ClassDecl l ctxt dh deps cdecls   -> liftM (ClassDecl l ctxt dh deps) $ mapM (mapM fix) cdecls
        InstDecl  l olp ih idecls         -> liftM (InstDecl  l olp ih)  $ mapM (mapM fix) idecls
        SpliceDecl l spl        -> liftM (SpliceDecl l) $ fix spl
        FunBind l matches       -> liftM (FunBind l) $ mapM fix matches
        PatBind l p rhs bs      -> liftM3 (PatBind l) (fix p) (fix rhs) (mapM fix bs)
        AnnPragma l ann'         -> liftM (AnnPragma l) $ fix ann'
        _                       -> return decl
      where fix x = applyFixities fixs x

appFixDecls :: Monad m => Maybe S.ModuleName -> [Fixity] -> [Decl SrcSpanInfo] -> m [Decl SrcSpanInfo]
appFixDecls mmdl fixs decls =
    let extraFixs = getFixities mmdl decls
     in mapM (applyFixities (fixs++extraFixs)) decls

getFixities :: Maybe S.ModuleName -> [Decl l] -> [Fixity]
getFixities mmdl = concatMap (getFixity mmdl)

getFixity :: Maybe S.ModuleName -> Decl l -> [Fixity]
getFixity mmdl (InfixDecl _ a mp ops) = let p = maybe 9 id mp in map (Fixity (sAssoc a) p) (concatMap g ops)
  where g (VarOp _ x) = f $ sName x
        g (ConOp _ x) = f $ sName x
        f x = case mmdl of
              Nothing -> [            S.UnQual x]
              Just m  -> [S.Qual m x, S.UnQual x]
getFixity _ _ = []

instance AppFixity Annotation where
    applyFixities fixs ann' = case ann' of
        Ann     l n e   -> liftM (Ann l n) $ fix e
        TypeAnn l n e   -> liftM (TypeAnn l n) $ fix e
        ModuleAnn l e   -> liftM (ModuleAnn l) $ fix e
      where fix x = applyFixities fixs x

instance AppFixity ClassDecl where
    applyFixities fixs (ClsDecl l decl) = liftM (ClsDecl l) $ applyFixities fixs decl
    applyFixities _ cdecl = return cdecl

instance AppFixity InstDecl where
    applyFixities fixs (InsDecl l decl) = liftM (InsDecl l) $ applyFixities fixs decl
    applyFixities _ idecl = return idecl

instance AppFixity Match where
    applyFixities fixs match = case match of
        Match l n ps rhs bs -> liftM3 (Match l n) (mapM fix ps) (fix rhs) (mapM fix bs)
        InfixMatch l a n ps rhs bs -> liftM4 (flip (InfixMatch l) n) (fix a) (mapM fix ps) (fix rhs) (mapM fix bs)
      where fix x = applyFixities fixs x

instance AppFixity Rhs where
    applyFixities fixs rhs = case rhs of
        UnGuardedRhs l e      -> liftM (UnGuardedRhs l) $ fix e
        GuardedRhss l grhss   -> liftM (GuardedRhss l) $ mapM fix grhss
      where fix x = applyFixities fixs x

instance AppFixity GuardedRhs where
    applyFixities fixs (GuardedRhs l stmts e) = liftM2 (GuardedRhs l) (mapM fix stmts) $ fix e
      where fix x = applyFixities fixs x

instance AppFixity PatField where
    applyFixities fixs (PFieldPat l n p) = liftM (PFieldPat l n) $ applyFixities fixs p
    applyFixities _ pf = return pf

instance AppFixity RPat where
    applyFixities fixs rp' = case rp' of
        RPOp l rp op          -> liftM (flip (RPOp l) op) $ fix rp
        RPEither l a b        -> liftM2 (RPEither l) (fix a) (fix b)
        RPSeq l rps           -> liftM (RPSeq l) $ mapM fix rps
        RPGuard l p stmts     -> liftM2 (RPGuard l) (fix p) $ mapM fix stmts
        RPCAs l n rp          -> liftM (RPCAs l n) $ fix rp
        RPAs l n rp           -> liftM (RPAs l n) $ fix rp
        RPParen l rp          -> liftM (RPParen l) $ fix rp
        RPPat l p             -> liftM (RPPat l) $ fix p
      where fix x = applyFixities fixs x

instance AppFixity PXAttr where
    applyFixities fixs (PXAttr l n p) = liftM (PXAttr l n) $ applyFixities fixs p

instance AppFixity Stmt where
    applyFixities fixs stmt = case stmt of
        Generator l p e       -> liftM2 (Generator l) (fix p) (fix e)
        Qualifier l e         -> liftM (Qualifier l) $ fix e
        LetStmt l bs          -> liftM (LetStmt l) $ fix bs    -- special behavior
        RecStmt l stmts       -> liftM (RecStmt l) $ mapM fix stmts
      where fix x = applyFixities fixs x

instance AppFixity Binds where
    applyFixities fixs bs = case bs of
        BDecls l decls        -> liftM (BDecls l) $ appFixDecls Nothing fixs decls  -- special behavior
        IPBinds l ips         -> liftM (IPBinds l) $ mapM fix ips
      where fix x = applyFixities fixs x


instance AppFixity IPBind where
    applyFixities fixs (IPBind l n e) = liftM (IPBind l n) $ applyFixities fixs e

instance AppFixity FieldUpdate where
    applyFixities fixs (FieldUpdate l n e) = liftM (FieldUpdate l n) $ applyFixities fixs e
    applyFixities _ fup = return fup

instance AppFixity Alt where
    applyFixities fixs (Alt l p galts bs) = liftM3 (Alt l) (fix p) (fix galts) (mapM fix bs)
      where fix x = applyFixities fixs x

instance AppFixity QualStmt where
    applyFixities fixs qstmt = case qstmt of
        QualStmt     l s      -> liftM (QualStmt l) $ fix s
        ThenTrans    l e      -> liftM (ThenTrans l) $ fix e
        ThenBy       l e1 e2  -> liftM2 (ThenBy l) (fix e1) (fix e2)
        GroupBy      l e      -> liftM (GroupBy l) (fix e)
        GroupUsing   l e      -> liftM (GroupUsing l) (fix e)
        GroupByUsing l e1 e2  -> liftM2 (GroupByUsing l) (fix e1) (fix e2)
      where fix x = applyFixities fixs x

instance AppFixity Bracket where
    applyFixities fixs br = case br of
        ExpBracket l e    -> liftM (ExpBracket l) $ fix e
        PatBracket l p    -> liftM (PatBracket l) $ fix p
        DeclBracket l ds  -> liftM (DeclBracket l) $ mapM fix ds
        _                 -> return br
      where fix x = applyFixities fixs x

instance AppFixity Splice where
    applyFixities fixs (ParenSplice l e) = liftM (ParenSplice l) $ applyFixities fixs e
    applyFixities _ s = return s

instance AppFixity XAttr where
    applyFixities fixs (XAttr l n e) = liftM (XAttr l n) $ applyFixities fixs e


-- the boring boilerplate stuff for expressions too
-- Recursively fixes the "leaves" of the infix chains,
-- without yet touching the chain itself. We assume all chains are
-- left-associate to begin with.
leafFix :: Monad m => [Fixity] -> Exp SrcSpanInfo -> m (Exp SrcSpanInfo)
leafFix fixs e' = case e' of
    InfixApp l e1 op e2       -> liftM2 (flip (InfixApp l) op) (leafFix fixs e1) (fix e2)
    App l e1 e2               -> liftM2 (App l) (fix e1) (fix e2)
    NegApp l e                -> liftM (NegApp l) $ fix e
    Lambda l pats e           -> liftM2 (Lambda l) (mapM fix pats) $ fix e
    Let l bs e                -> liftM2 (Let l) (fix bs) $ fix e
    If l e a b                -> liftM3 (If l) (fix e) (fix a) (fix b)
    MultiIf l alts            -> liftM (MultiIf l) (mapM fix alts)
    Case l e alts             -> liftM2 (Case l) (fix e) $ mapM fix alts
    Do l stmts                -> liftM (Do l) $ mapM fix stmts
    MDo l stmts               -> liftM (MDo l) $ mapM fix stmts
    Tuple l bx exps           -> liftM (Tuple l bx) $ mapM fix exps
    List l exps               -> liftM (List l) $ mapM fix  exps
    Paren l e                 -> liftM (Paren l) $ fix e
    LeftSection l e op        -> liftM (flip (LeftSection l) op) (fix e)
    RightSection l op e       -> liftM (RightSection l op) $ fix e
    RecConstr l n fups        -> liftM (RecConstr l n) $ mapM fix fups
    RecUpdate l e fups        -> liftM2 (RecUpdate l) (fix e) $ mapM fix fups
    EnumFrom l e              -> liftM (EnumFrom l) $ fix e
    EnumFromTo l e1 e2        -> liftM2 (EnumFromTo l) (fix e1) (fix e2)
    EnumFromThen l e1 e2      -> liftM2 (EnumFromThen l) (fix e1) (fix e2)
    EnumFromThenTo l e1 e2 e3 -> liftM3 (EnumFromThenTo l) (fix e1) (fix e2) (fix e3)
    ListComp l e quals        -> liftM2 (ListComp l) (fix e) $ mapM fix quals
    ParComp  l e qualss       -> liftM2 (ParComp l) (fix e) $ mapM (mapM fix) qualss
    ExpTypeSig l e t          -> liftM (flip (ExpTypeSig l) t) (fix e)
    BracketExp l b            -> liftM (BracketExp l) $ fix b
    SpliceExp l s             -> liftM (SpliceExp l) $ fix s
    XTag l n ats mexp cs      -> liftM3 (XTag l n) (mapM fix ats) (mapM fix mexp) (mapM fix cs)
    XETag l n ats mexp        -> liftM2 (XETag l n) (mapM fix ats) (mapM fix mexp)
    XExpTag l e               -> liftM (XExpTag l) $ fix e
    XChildTag l cs            -> liftM (XChildTag l) $ mapM fix cs
    Proc l p e                -> liftM2 (Proc l) (fix p) (fix e)
    LeftArrApp l e1 e2        -> liftM2 (LeftArrApp l) (fix e1) (fix e2)
    RightArrApp l e1 e2       -> liftM2 (RightArrApp l) (fix e1) (fix e2)
    LeftArrHighApp l e1 e2    -> liftM2 (LeftArrHighApp l) (fix e1) (fix e2)
    RightArrHighApp l e1 e2   -> liftM2 (RightArrHighApp l) (fix e1) (fix e2)
    CorePragma l s e          -> liftM (CorePragma l s) (fix e)
    SCCPragma l s e           -> liftM (SCCPragma l s) (fix e)
    GenPragma l s ab cd e     -> liftM (GenPragma l s ab cd) (fix e)
    LCase l alts              -> liftM (LCase l) $ mapM fix alts

    _                         -> return e'
  where
    fix x = applyFixities fixs x

leafFixP :: Monad m => [Fixity] -> Pat SrcSpanInfo -> m (Pat SrcSpanInfo)
leafFixP fixs p' = case p' of
        PInfixApp l p1 op p2    -> liftM2 (flip (PInfixApp l) op) (leafFixP fixs p1) (fix p2)
        PApp l n ps             -> liftM (PApp l n) $ mapM fix ps
        PTuple l bx ps          -> liftM (PTuple l bx) $ mapM fix ps
        PList l ps              -> liftM (PList l) $ mapM fix ps
        PParen l p              -> liftM (PParen l) $ fix p
        PRec l n pfs            -> liftM (PRec l n) $ mapM fix pfs
        PAsPat l n p            -> liftM (PAsPat l n) $ fix p
        PIrrPat l p             -> liftM (PIrrPat l) $ fix p
        PatTypeSig l p t        -> liftM (flip (PatTypeSig l) t) (fix p)
        PViewPat l e p          -> liftM2 (PViewPat l) (fix e) (fix p)
        PRPat l rps             -> liftM (PRPat l) $ mapM fix rps
        PXTag l n ats mp ps     -> liftM3 (PXTag l n) (mapM fix ats) (mapM fix mp) (mapM fix ps)
        PXETag l n ats mp       -> liftM2 (PXETag l n) (mapM fix ats) (mapM fix mp)
        PXPatTag l p            -> liftM (PXPatTag l) $ fix p
        PXRPats l rps           -> liftM (PXRPats l) $ mapM fix rps
        PBangPat l p            -> liftM (PBangPat l) $ fix p
        _                       -> return p'
      where fix x = applyFixities fixs x

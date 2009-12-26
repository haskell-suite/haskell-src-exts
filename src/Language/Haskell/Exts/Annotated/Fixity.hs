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

import Language.Haskell.Exts.Fixity ( Fixity(..), infix_, infixl_, infixr_, preludeFixities, baseFixities )
import qualified Language.Haskell.Exts.Syntax as S ( Assoc(..), QOp(..), Op(..), QName(..), Name(..), SpecialCon(..) )
import Language.Haskell.Exts.Annotated.Simplify ( sQOp, sOp, sAssoc, sQName )

import Data.Char (isUpper)

-- | All AST elements that may include expressions which in turn may
--   need fixity tweaking will be instances of this class.
class AppFixity ast where
  -- | Tweak any expressions in the element to account for the
  --   fixities given. Assumes that all operator expressions are
  --   fully left associative chains to begin with.
  applyFixities :: [Fixity]   -- ^ The fixities to account for.
                    -> ast SrcSpanInfo  -- ^ The element to tweak.
                    -> ast SrcSpanInfo  -- ^ The same element, but with operator expressions updated.


instance AppFixity Exp where
  applyFixities fixs = infFix fixs . leafFix fixs
    where -- This is the real meat case. We can assume a left-associative list to begin with.
          infFix fixs (InfixApp l2 a op2 z) =
              let e = infFix fixs a
               in case e of
                   InfixApp l1 x op1 y ->
                      let (a1,p1) = askFixity fixs op1
                          (a2,p2) = askFixity fixs op2
                       in if (p1 == p2 && (a1 /= a2 || a1 == S.AssocNone )) -- Ambiguous infix expression!
                              || (p1 > p2 || p1 == p2 && (a1 == S.AssocLeft || a2 == S.AssocNone)) -- Already right order
                           then InfixApp l2 e op2 z
                           else InfixApp l2 x op1 (infFix fixs $ InfixApp (ann y <++> ann z) y op2 z)
                   _  -> InfixApp l2 e op2 z

          infFix _ e = e

instance AppFixity Pat where
  applyFixities fixs = infFix fixs . leafFixP fixs
    where -- This is the real meat case. We can assume a left-associative list to begin with.
          infFix fixs (PInfixApp l2 a op2 z) =
              let p = infFix fixs a
               in case p of
                   PInfixApp l1 x op1 y ->
                      let (a1,p1) = askFixityP fixs op1
                          (a2,p2) = askFixityP fixs op2
                       in if (p1 == p2 && (a1 /= a2 || a1 == S.AssocNone )) -- Ambiguous infix expression!
                              || (p1 > p2 || p1 == p2 && (a1 == S.AssocLeft || a2 == S.AssocNone)) -- Already right order
                           then PInfixApp l2 p op2 z
                           else PInfixApp l2 x op1 (infFix fixs $ PInfixApp (ann y <++> ann z) y op2 z)
                   _  -> PInfixApp l2 p op2 z

          infFix _ p = p

-- Internal: lookup associativity and precedence of an operator
askFixity :: [Fixity] -> QOp l -> (S.Assoc, Int)
askFixity xs k = askFix xs (f $ sQOp k) -- undefined -- \k -> askFixityP xs (f k) -- lookupWithDefault (AssocLeft, 9) (f k) mp
    where
        f (S.QVarOp x) = S.VarOp (g x)
        f (S.QConOp x) = S.ConOp (g x)

        g (S.Qual _ x) = x
        g (S.UnQual x) = x
        g (S.Special S.Cons) = S.Symbol ":"

-- Same using patterns
askFixityP :: [Fixity] -> QName l -> (S.Assoc, Int)
askFixityP xs qn = askFix xs (S.ConOp $ g $ sQName qn)
    where
        g (S.Qual _ x) = x
        g (S.UnQual x) = x
        g (S.Special S.Cons) = S.Symbol ":"
        
askFix :: [Fixity] -> S.Op -> (S.Assoc, Int)
askFix xs = \k -> lookupWithDefault (S.AssocLeft, 9) k mp
    where
        lookupWithDefault def k mp = case lookup k mp of
            Nothing -> def
            Just x  -> x

        mp = [(x,(a,p)) | Fixity a p x <- xs]


-------------------------------------------------------------------
-- Boilerplate - yuck!! Everything below here is internal stuff

instance AppFixity Module where
    applyFixities fixs (Module l mmh prs imp decls) =
        Module l mmh prs imp $ appFixDecls fixs decls
    applyFixities fixs (XmlPage l mn os xn xas mexp cs) =
        XmlPage l mn os xn (map fix xas) (fmap fix mexp) (map fix cs)
      where fix x = applyFixities fixs x
    applyFixities fixs (XmlHybrid l mmh prs imp decls xn xas mexp cs) =
        XmlHybrid l mmh prs imp (appFixDecls fixs decls)
                xn (map fixe xas) (fmap fixe mexp) (map fixe cs)
      where fixe x = let extraFixs = getFixities decls
                      in applyFixities (fixs++extraFixs) x

instance AppFixity Decl where
    applyFixities fixs decl = case decl of
        ClassDecl l ctxt dh deps cdecls   -> ClassDecl l ctxt dh deps $ fmap (map fix) cdecls
        InstDecl  l ctxt ih idecls        -> InstDecl  l ctxt ih      $ fmap (map fix) idecls
        SpliceDecl l spl        -> SpliceDecl l $ fix spl
        FunBind l matches       -> FunBind l $ map fix matches
        PatBind l p mt rhs bs -> PatBind l (fix p) mt (fix rhs) (fmap fix bs)
        _                       -> decl
      where fix x = applyFixities fixs x

appFixDecls :: [Fixity] -> [Decl SrcSpanInfo] -> [Decl SrcSpanInfo]
appFixDecls fixs decls =
    let extraFixs = getFixities decls
     in map (applyFixities (fixs++extraFixs)) decls

getFixities = concatMap getFixity
getFixity (InfixDecl _ a mp ops) = let p = maybe 9 id mp in map (Fixity (sAssoc a) p) (map sOp ops)
getFixity _ = []

instance AppFixity ClassDecl where
    applyFixities fixs (ClsDecl l decl) = ClsDecl l $ applyFixities fixs decl
    applyFixities _ cdecl = cdecl

instance AppFixity InstDecl where
    applyFixities fixs (InsDecl l decl) = InsDecl l $ applyFixities fixs decl
    applyFixities _ idecl = idecl

instance AppFixity Match where
    applyFixities fixs match = case match of
        Match l n ps rhs bs -> Match l n (map fix ps) (fix rhs) (fmap fix bs)
        InfixMatch l a n b rhs bs -> InfixMatch l (fix a) n (fix b) (fix rhs) (fmap fix bs)
      where fix x = applyFixities fixs x

instance AppFixity Rhs where
    applyFixities fixs rhs = case rhs of
        UnGuardedRhs l e      -> UnGuardedRhs l $ fix e
        GuardedRhss l grhss   -> GuardedRhss l $ map fix grhss
      where fix x = applyFixities fixs x

instance AppFixity GuardedRhs where
    applyFixities fixs (GuardedRhs l stmts e) = GuardedRhs l (map fix stmts) $ fix e
      where fix x = applyFixities fixs x

instance AppFixity PatField where
    applyFixities fixs (PFieldPat l n p) = PFieldPat l n $ applyFixities fixs p
    applyFixities _ pf = pf

instance AppFixity RPat where
    applyFixities fixs rp = case rp of
        RPOp l rp op          -> RPOp l (fix rp) op
        RPEither l a b        -> RPEither l (fix a) (fix b)
        RPSeq l rps           -> RPSeq l $ map fix rps
        RPGuard l p stmts     -> RPGuard l (fix p) $ map fix stmts
        RPCAs l n rp          -> RPCAs l n $ fix rp
        RPAs l n rp           -> RPAs l n $ fix rp
        RPParen l rp          -> RPParen l $ fix rp
        RPPat l p             -> RPPat l $ fix p
      where fix x = applyFixities fixs x

instance AppFixity PXAttr where
    applyFixities fixs (PXAttr l n p) = PXAttr l n $ applyFixities fixs p

instance AppFixity Stmt where
    applyFixities fixs stmt = case stmt of
        Generator l p e       -> Generator l (fix p) (fix e)
        Qualifier l e         -> Qualifier l $ fix e
        LetStmt l bs          -> LetStmt l $ fix bs    -- special behavior
        RecStmt l stmts       -> RecStmt l $ map fix stmts
      where fix x = applyFixities fixs x

instance AppFixity Binds where
    applyFixities fixs bs = case bs of
        BDecls l decls        -> BDecls l $ appFixDecls fixs decls  -- special behavior
        IPBinds l ips         -> IPBinds l $ map fix ips
      where fix x = applyFixities fixs x


instance AppFixity IPBind where
    applyFixities fixs (IPBind l n e) = IPBind l n $ applyFixities fixs e

instance AppFixity FieldUpdate where
    applyFixities fixs (FieldUpdate l n e) = FieldUpdate l n $ applyFixities fixs e
    applyFixities _ fup = fup

instance AppFixity Alt where
    applyFixities fixs (Alt l p galts bs) = Alt l (fix p) (fix galts) (fmap fix bs)
      where fix x = applyFixities fixs x

instance AppFixity GuardedAlts where
    applyFixities fixs galts = case galts of
        UnGuardedAlt l e      -> UnGuardedAlt l $ fix e
        GuardedAlts  l galts  -> GuardedAlts l $ map fix galts
      where fix x = applyFixities fixs x

instance AppFixity GuardedAlt where
    applyFixities fixs (GuardedAlt l stmts e) = GuardedAlt l (map fix stmts) (fix e)
      where fix x = applyFixities fixs x

instance AppFixity QualStmt where
    applyFixities fixs qstmt = case qstmt of
        QualStmt     l s      -> QualStmt l $ fix s
        ThenTrans    l e      -> ThenTrans l $ fix e
        ThenBy       l e1 e2  -> ThenBy l (fix e1) (fix e2)
        GroupBy      l e      -> GroupBy l (fix e)
        GroupUsing   l e      -> GroupUsing l (fix e)
        GroupByUsing l e1 e2  -> GroupByUsing l (fix e1) (fix e2)
      where fix x = applyFixities fixs x

instance AppFixity Bracket where
    applyFixities fixs br = case br of
        ExpBracket l e    -> ExpBracket l $ fix e
        PatBracket l p    -> PatBracket l $ fix p
        DeclBracket l ds  -> DeclBracket l $ map fix ds
        _                 -> br
      where fix x = applyFixities fixs x

instance AppFixity Splice where
    applyFixities fixs (ParenSplice l e) = ParenSplice l $ applyFixities fixs e
    applyFixities _ s = s

instance AppFixity XAttr where
    applyFixities fixs (XAttr l n e) = XAttr l n $ applyFixities fixs e


-- the boring boilerplate stuff for expressions too
-- Recursively fixes the "leaves" of the infix chains,
-- without yet touching the chain itself. We assume all chains are
-- left-associate to begin with.
leafFix fixs e = case e of
    InfixApp l e1 op e2       -> InfixApp l (leafFix fixs e1) op (fix e2)
    App l e1 e2               -> App l (fix e1) (fix e2)
    NegApp l e                -> NegApp l $ fix e
    Lambda l pats e           -> Lambda l (map fix pats) $ fix e
    Let l bs e                -> Let l (fix bs) $ fix e
    If l e a b                -> If l (fix e) (fix a) (fix b)
    Case l e alts             -> Case l (fix e) $ map fix alts
    Do l stmts                -> Do l $ map fix stmts
    MDo l stmts               -> MDo l $ map fix stmts
    Tuple l exps              -> Tuple l $ map fix exps
    List l exps               -> List l $ map fix  exps
    Paren l e                 -> Paren l $ fix e
    LeftSection l e op        -> LeftSection l (fix e) op
    RightSection l op e       -> RightSection l op $ fix e
    RecConstr l n fups        -> RecConstr l n $ map fix fups
    RecUpdate l e fups        -> RecUpdate l (fix e) $ map fix fups
    EnumFrom l e              -> EnumFrom l $ fix e
    EnumFromTo l e1 e2        -> EnumFromTo l (fix e1) (fix e2)
    EnumFromThen l e1 e2      -> EnumFromThen l (fix e1) (fix e2)
    EnumFromThenTo l e1 e2 e3 -> EnumFromThenTo l (fix e1) (fix e2) (fix e3)
    ListComp l e quals        -> ListComp l (fix e) $ map fix quals
    ParComp  l e qualss       -> ParComp l (fix e) $ map (map fix) qualss
    ExpTypeSig l e t          -> ExpTypeSig l (fix e) t
    BracketExp l b            -> BracketExp l $ fix b
    SpliceExp l s             -> SpliceExp l $ fix s
    XTag l n ats mexp cs      -> XTag l n (map fix ats) (fmap fix mexp) (map fix cs)
    XETag l n ats mexp        -> XETag l n (map fix ats) (fmap fix mexp)
    XExpTag l e               -> XExpTag l $ fix e
    Proc l p e                -> Proc l (fix p) (fix e)
    LeftArrApp l e1 e2        -> LeftArrApp l (fix e1) (fix e2)
    RightArrApp l e1 e2       -> RightArrApp l (fix e1) (fix e2)
    LeftArrHighApp l e1 e2    -> LeftArrHighApp l (fix e1) (fix e2)
    RightArrHighApp l e1 e2   -> RightArrHighApp l (fix e1) (fix e2)
    CorePragma l s e          -> CorePragma l s (fix e)
    SCCPragma l s e           -> SCCPragma l s (fix e)
    GenPragma l s ab cd e     -> GenPragma l s ab cd (fix e)

    _                         -> e
  where
    fix x = applyFixities fixs x

leafFixP fixs p = case p of
        PNeg l p                -> PNeg l $ fix p
        PApp l n ps             -> PApp l n $ map fix ps
        PTuple l ps             -> PTuple l $ map fix ps
        PList l ps              -> PList l $ map fix ps
        PParen l p              -> PParen l $ fix p
        PRec l n pfs            -> PRec l n $ map fix pfs
        PAsPat l n p            -> PAsPat l n $ fix p
        PIrrPat l p             -> PIrrPat l $ fix p
        PatTypeSig l p t        -> PatTypeSig l (fix p) t
        PViewPat l e p          -> PViewPat l (fix e) (fix p)
        PRPat l rps             -> PRPat l $ map fix rps
        PXTag l n ats mp ps     -> PXTag l n (map fix ats) (fmap fix mp) (map fix ps)
        PXETag l n ats mp       -> PXETag l n (map fix ats) (fmap fix mp)
        PXPatTag l p            -> PXPatTag l $ fix p
        PXRPats l rps           -> PXRPats l $ map fix rps
        PBangPat l p            -> PBangPat l $ fix p
        _                       -> p
      where fix x = applyFixities fixs x

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Fixity
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
import Language.Haskell.Exts.Annotated.SrcLoc

import Data.Char (isUpper)

type L = SrcSpanInfo

-- | Operator fixities are represented by their associativity
--   (left, right or none) and their precedence (0-9).
data Fixity = Fixity (Assoc ()) Int (Op ())

-- | All AST elements that may include expressions which in turn may
--   need fixity tweaking will be instances of this class.
class AppFixity ast where
  -- | Tweak any expressions in the element to account for the
  --   fixities given. Assumes that all operator expressions are
  --   fully left associative chains to begin with.
  applyFixities :: [Fixity]   -- ^ The fixities to account for.
                    -> ast L  -- ^ The element to tweak.
                    -> ast L  -- ^ The same element, but with operator expressions updated.


instance AppFixity Exp where
  applyFixities fixs = infFix fixs . leafFix fixs
    where -- This is the real meat case. We can assume a left-associative list to begin with.
          infFix fixs (InfixApp l2 a op2 z) =
              let e = infFix fixs a
               in case e of
                   InfixApp l1 x op1 y ->
                      let (a1,p1) = askFixity fixs op1
                          (a2,p2) = askFixity fixs op2
                       in if (p1 == p2 && (a1 /= a2 || a1 == AssocNone ())) -- Ambiguous infix expression!
                              || (p1 > p2 || p1 == p2 && (a1 == AssocLeft () || a2 == AssocNone ())) -- Already right order
                           then InfixApp l2 e op2 z
                           else InfixApp l2 x op1 (infFix fixs $ InfixApp (ann y <++> ann z) y op2 z)
                   _  -> InfixApp l2 e op2 z

          infFix _ e = e


-- Internal: lookup associativity and precedence of an operator
askFixity :: [Fixity] -> QOp l -> (Assoc (), Int)
askFixity xs = \k -> lookupWithDefault (AssocLeft (), 9) (f k) mp
    where
        lookupWithDefault def k mp = case lookup k mp of
            Nothing -> def
            Just x  -> x

        mp = [(x,(a,p)) | Fixity a p x <- xs]

        f (QVarOp l x) = nullAnn $ VarOp l (g x)
        f (QConOp l x) = nullAnn $ ConOp l (g x)

        g (Qual _ _ x) = x
        g (UnQual _ x) = x
        g (Special l (Cons _)) = Symbol l ":"

nullAnn :: Functor ast => ast l -> ast ()
nullAnn = fmap (const ())


-- | All fixities defined in the Prelude.
preludeFixities :: [Fixity]
preludeFixities = concat
    [infixr_ 9  ["."]
    ,infixl_ 9  ["!!"]
    ,infixr_ 8  ["^","^^","**"]
    ,infixl_ 7  ["*","/","`quot`","`rem`","`div`","`mod`",":%","%"]
    ,infixl_ 6  ["+","-"]
    ,infixr_ 5  [":","++"]
    ,infix_  4  ["==","/=","<","<=",">=",">","`elem`","`notElem`"]
    ,infixr_ 3  ["&&"]
    ,infixr_ 2  ["||"]
    ,infixl_ 1  [">>",">>="]
    ,infixr_ 1  ["=<<"]
    ,infixr_ 0  ["$","$!","`seq`"]
    ]

-- | All fixities defined in the base package.
--
--   Note that the @+++@ operator appears in both Control.Arrows and
--   Text.ParserCombinators.ReadP. The listed precedence for @+++@ in
--   this list is that of Control.Arrows.
baseFixities :: [Fixity]
baseFixities = preludeFixities ++ concat
    [infixl_ 9 ["!","//","!:"]
    ,infixl_ 8 ["`shift`","`rotate`","`shiftL`","`shiftR`","`rotateL`","`rotateR`"]
    ,infixl_ 7 [".&."]
    ,infixl_ 6 ["`xor`"]
    ,infix_  6 [":+"]
    ,infixl_ 5 [".|."]
    ,infixr_ 5 ["+:+","<++","<+>"] -- fixity conflict for +++ between ReadP and Arrow
    ,infix_  5 ["\\\\"]
    ,infixl_ 4 ["<$>","<$","<*>","<*","*>","<**>"]
    ,infix_  4 ["`elemP`","`notElemP`"]
    ,infixl_ 3 ["<|>"]
    ,infixr_ 3 ["&&&","***"]
    ,infixr_ 2 ["+++","|||"]
    ,infixr_ 1 ["<=<",">=>",">>>","<<<","^<<","<<^","^>>",">>^"]
    ,infixl_ 0 ["`on`"]
    ,infixr_ 0 ["`par`","`pseq`"]
    ]

infixr_, infixl_, infix_ :: Int -> [String] -> [Fixity]
infixr_ = fixity $ AssocRight ()
infixl_ = fixity $ AssocLeft ()
infix_  = fixity $ AssocNone ()

-- Internal: help function for the above definitions.
fixity :: Assoc () -> Int -> [String] -> [Fixity]
fixity a p = map (Fixity a p . op)
    where
        op ('`':xs) = (if isUpper (head xs) then ConOp else VarOp) () $ Ident () $ init xs
        op xs = (if head xs == ':' then ConOp else VarOp) () $ Symbol () xs






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

appFixDecls :: [Fixity] -> [Decl L] -> [Decl L]
appFixDecls fixs decls =
    let extraFixs = getFixities decls
     in map (applyFixities (fixs++extraFixs)) decls

getFixities = concatMap getFixity
getFixity (InfixDecl _ a mp ops) = let p = maybe 9 id mp in map (Fixity (nullAnn a) p) (map nullAnn ops)
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

instance AppFixity Pat where
    applyFixities fixs p = case p of
        PNeg l p                -> PNeg l $ fix p
        PInfixApp l a op b      -> PInfixApp l (fix a) op (fix b)
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

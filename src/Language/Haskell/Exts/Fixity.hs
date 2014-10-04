{-# LANGUAGE DeriveDataTypeable #-}
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
module Language.Haskell.Exts.Fixity
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
    , preludeFixities, baseFixities, prefixMinusFixity

    -- * Applying fixities to an AST
    , AppFixity(..)
    ) where

import Language.Haskell.Exts.Syntax

import Control.Monad (when, (<=<), liftM, liftM2, liftM3)
import Data.Traversable (mapM)
import Prelude hiding (mapM)
import Data.Data hiding (Fixity)
import Data.Maybe (fromMaybe)

-- | Operator fixities are represented by their associativity
--   (left, right or none) and their precedence (0-9).
data Fixity = Fixity Assoc Int QName
  deriving (Eq,Ord,Show,Typeable,Data)

-- | All AST elements that may include expressions which in turn may
--   need fixity tweaking will be instances of this class.
class AppFixity ast where
  -- | Tweak any expressions in the element to account for the
  --   fixities given. Assumes that all operator expressions are
  --   fully left associative chains to begin with.
  applyFixities :: Monad m => [Fixity] -- ^ The fixities to account for.
                    -> ast  -- ^ The element to tweak.
                    -> m ast  -- ^ The same element, but with operator expressions updated, or a failure.


instance AppFixity Exp where
  applyFixities fixs' = infFix fixs' <=< leafFix fixs'
    where -- This is the real meat case. We can assume a left-associative list to begin with.
          infFix fixs (InfixApp a op2 z) = do
              e <- infFix fixs a
              let fixup (a1,p1) (a2,p2) y pre = do
                      when (p1 == p2 && (a1 /= a2 || a1 == AssocNone)) -- Ambiguous infix expression!
                           $ fail "Ambiguous infix expression"
                      if p1 > p2 || p1 == p2 && (a1 == AssocLeft || a2 == AssocNone) -- Already right order
                       then return $ InfixApp e op2 z
                       else liftM pre (infFix fixs $ InfixApp y op2 z)
              case e of
               InfixApp x op1 y -> fixup (askFixity fixs op1) (askFixity fixs op2) y (InfixApp x op1)
               NegApp         y -> fixup prefixMinusFixity    (askFixity fixs op2) y  NegApp
               _  -> return $ InfixApp e op2 z

          infFix _ e = return e

instance AppFixity Pat where
  applyFixities fixs' = infFix fixs' <=< leafFixP fixs'
    where -- Same for patterns
          infFix fixs (PInfixApp a op2 z) = do
              p <- infFix fixs a
              let fixup (a1,p1) (a2,p2) y pre = do
                      when (p1 == p2 && (a1 /= a2 || a1 == AssocNone )) -- Ambiguous infix expression!
                           $ fail "Ambiguous infix expression"
                      if p1 > p2 || p1 == p2 && (a1 == AssocLeft || a2 == AssocNone) -- Already right order
                       then return $ PInfixApp p op2 z
                       else liftM pre (infFix fixs $ PInfixApp y op2 z)
              case p of
               PInfixApp x op1 y -> fixup (askFixityP fixs op1) (askFixityP fixs op2) y (PInfixApp x op1)
               _  -> return $ PInfixApp p op2 z

          infFix _ p = return p


-- Internal: lookup associativity and precedence of an operator
askFixity :: [Fixity] -> QOp -> (Assoc, Int)
askFixity xs k = askFix xs (f k) -- undefined -- \k -> askFixityP xs (f k) -- lookupWithDefault (AssocLeft, 9) (f k) mp
    where
        f (QVarOp x) = g x
        f (QConOp x) = g x

        g (Special Cons) = UnQual (Symbol ":")
        g x              = x

-- Same using patterns
askFixityP :: [Fixity] -> QName -> (Assoc, Int)
askFixityP xs qn = askFix xs (g qn)
    where
        g (Special Cons) = UnQual (Symbol ":")
        g x              = x

askFix :: [Fixity] -> QName -> (Assoc, Int)
askFix xs = \k -> lookupWithDefault (AssocLeft, 9) k mp
    where
        lookupWithDefault def k mp1 = fromMaybe def $ lookup k mp1

        mp = [(x,(a,p)) | Fixity a p x <- xs]


-- | Built-in fixity for prefix minus
prefixMinusFixity :: (Assoc, Int)
prefixMinusFixity = (AssocLeft, 6)

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
infixr_ = fixity AssocRight
infixl_ = fixity AssocLeft
infix_  = fixity AssocNone

-- Internal: help function for the above definitions.
fixity :: Assoc -> Int -> [String] -> [Fixity]
fixity a p = map (Fixity a p . op)
    where
        op ('`':xs) = UnQual $ Ident $ init xs
        op xs = UnQual $ Symbol xs






-------------------------------------------------------------------
-- Boilerplate - yuck!! Everything below here is internal stuff

instance AppFixity Module where
    applyFixities fixs (Module loc n prs mwt ext imp decls) =
        liftM (Module loc n prs mwt ext imp) $ appFixDecls fixs decls

instance AppFixity Decl where
    applyFixities fixs decl = case decl of
        ClassDecl loc ctxt n vars deps cdecls   -> liftM (ClassDecl loc ctxt n vars deps) $ mapM fix cdecls
        InstDecl loc olp tvs ctxt n ts idecls   -> liftM (InstDecl loc olp tvs ctxt n ts) $ mapM fix idecls
        SpliceDecl loc spl      -> liftM (SpliceDecl loc) $ fix spl
        FunBind matches         -> liftM FunBind $ mapM fix matches
        PatBind loc p rhs bs    -> liftM3 (PatBind loc) (fix p) (fix rhs) (fix bs)
        AnnPragma loc ann       -> liftM (AnnPragma loc) $ fix ann
        _                       -> return decl
      where fix x = applyFixities fixs x

appFixDecls :: Monad m => [Fixity] -> [Decl] -> m [Decl]
appFixDecls fixs decls =
    let extraFixs = getFixities decls
     in mapM (applyFixities (fixs++extraFixs)) decls
  where getFixities = concatMap getFixity
        getFixity (InfixDecl _ a p ops) = map (Fixity a p . g) ops
        getFixity _ = []
        g (VarOp x) = UnQual x
        g (ConOp x) = UnQual x

instance AppFixity Annotation where
    applyFixities fixs ann = case ann of
        Ann     n e   -> liftM (Ann n) $ fix e
        TypeAnn n e   -> liftM (TypeAnn n) $ fix e
        ModuleAnn e   -> liftM ModuleAnn $ fix e
      where fix x = applyFixities fixs x

instance AppFixity ClassDecl where
    applyFixities fixs (ClsDecl decl) = liftM ClsDecl $ applyFixities fixs decl
    applyFixities _ cdecl = return cdecl

instance AppFixity InstDecl where
    applyFixities fixs (InsDecl decl) = liftM InsDecl $ applyFixities fixs decl
    applyFixities _ idecl = return idecl

instance AppFixity Match where
    applyFixities fixs (Match loc n ps mt rhs bs) = liftM3 (flip (Match loc n) mt) (mapM fix ps) (fix rhs) (fix bs)
      where fix x = applyFixities fixs x

instance AppFixity Rhs where
    applyFixities fixs rhs = case rhs of
        UnGuardedRhs e      -> liftM UnGuardedRhs $ fix e
        GuardedRhss grhss   -> liftM GuardedRhss $ mapM fix grhss
      where fix x = applyFixities fixs x

instance AppFixity GuardedRhs where
    applyFixities fixs (GuardedRhs loc stmts e) = liftM2 (GuardedRhs loc) (mapM fix stmts) $ fix e
      where fix x = applyFixities fixs x

instance AppFixity PatField where
    applyFixities fixs (PFieldPat n p) = liftM (PFieldPat n) $ applyFixities fixs p
    applyFixities _ pf = return pf

instance AppFixity RPat where
    applyFixities fixs rp' = case rp' of
        RPOp rp op          -> liftM (flip RPOp op) (fix rp)
        RPEither a b        -> liftM2 RPEither (fix a) (fix b)
        RPSeq rps           -> liftM RPSeq $ mapM fix rps
        RPGuard p stmts     -> liftM2 RPGuard (fix p) $ mapM fix stmts
        RPCAs n rp          -> liftM (RPCAs n) $ fix rp
        RPAs n rp           -> liftM (RPAs n) $ fix rp
        RPParen rp          -> liftM RPParen $ fix rp
        RPPat p             -> liftM RPPat $ fix p
      where fix x = applyFixities fixs x

instance AppFixity PXAttr where
    applyFixities fixs (PXAttr n p) = liftM (PXAttr n) $ applyFixities fixs p

instance AppFixity Stmt where
    applyFixities fixs stmt = case stmt of
        Generator loc p e   -> liftM2 (Generator loc) (fix p) (fix e)
        Qualifier e         -> liftM Qualifier $ fix e
        LetStmt bs          -> liftM LetStmt $ fix bs    -- special behavior
        RecStmt stmts       -> liftM RecStmt $ mapM fix stmts
      where fix x = applyFixities fixs x

instance AppFixity Binds where
    applyFixities fixs bs = case bs of
        BDecls decls        -> liftM BDecls $ appFixDecls fixs decls  -- special behavior
        IPBinds ips         -> liftM IPBinds $ mapM fix ips
      where fix x = applyFixities fixs x


instance AppFixity IPBind where
    applyFixities fixs (IPBind loc n e) = liftM (IPBind loc n) $ applyFixities fixs e

instance AppFixity FieldUpdate where
    applyFixities fixs (FieldUpdate n e) = liftM (FieldUpdate n) $ applyFixities fixs e
    applyFixities _ fup = return fup

instance AppFixity Alt where
    applyFixities fixs (Alt loc p galts bs) = liftM3 (Alt loc) (fix p) (fix galts) (fix bs)
      where fix x = applyFixities fixs x

instance AppFixity QualStmt where
    applyFixities fixs qstmt = case qstmt of
        QualStmt     s      -> liftM QualStmt $ fix s
        ThenTrans    e      -> liftM ThenTrans $ fix e
        ThenBy       e1 e2  -> liftM2 ThenBy (fix e1) (fix e2)
        GroupBy      e      -> liftM GroupBy (fix e)
        GroupUsing   e      -> liftM GroupUsing (fix e)
        GroupByUsing e1 e2  -> liftM2 GroupByUsing (fix e1) (fix e2)
      where fix x = applyFixities fixs x

instance AppFixity Bracket where
    applyFixities fixs br = case br of
        ExpBracket e    -> liftM ExpBracket $ fix e
        PatBracket p    -> liftM PatBracket $ fix p
        DeclBracket ds  -> liftM DeclBracket $ mapM fix ds
        _               -> return br
      where fix x = applyFixities fixs x

instance AppFixity Splice where
    applyFixities fixs (ParenSplice e) = liftM ParenSplice $ applyFixities fixs e
    applyFixities _ s = return s

instance AppFixity XAttr where
    applyFixities fixs (XAttr n e) = liftM (XAttr n) $ applyFixities fixs e


-- the boring boilerplate stuff for expressions too
-- Recursively fixes the "leaves" of the infix chains,
-- without yet touching the chain itself. We assume all chains are
-- left-associate to begin with.
leafFix :: Monad m => [Fixity] -> Exp -> m Exp
leafFix fixs e' = case e' of
    InfixApp e1 op e2       -> liftM2 (flip InfixApp op) (leafFix fixs e1) (fix e2)
    App e1 e2               -> liftM2 App (fix e1) (fix e2)
    NegApp e                -> liftM NegApp $ fix e
    Lambda loc pats e       -> liftM2 (Lambda loc) (mapM fix pats) $ fix e
    Let bs e                -> liftM2 Let (fix bs) $ fix e
    If e a b                -> liftM3 If (fix e) (fix a) (fix b)
    MultiIf alts            -> liftM MultiIf (mapM fix alts)
    Case e alts             -> liftM2 Case (fix e) $ mapM fix alts
    Do stmts                -> liftM Do $ mapM fix stmts
    MDo stmts               -> liftM MDo $ mapM fix stmts
    Tuple bx exps           -> liftM (Tuple bx) $ mapM fix exps
    List exps               -> liftM List $ mapM fix  exps
    ParArray exps           -> liftM ParArray $ mapM fix exps
    Paren e                 -> liftM Paren $ fix e
    LeftSection e op        -> liftM (flip LeftSection op) (fix e)
    RightSection op e       -> liftM (RightSection op) $ fix e
    RecConstr n fups        -> liftM (RecConstr n) $ mapM fix fups
    RecUpdate e fups        -> liftM2 RecUpdate (fix e) $ mapM fix fups
    EnumFrom e              -> liftM EnumFrom $ fix e
    EnumFromTo e1 e2        -> liftM2 EnumFromTo (fix e1) (fix e2)
    EnumFromThen e1 e2      -> liftM2 EnumFromThen (fix e1) (fix e2)
    EnumFromThenTo e1 e2 e3 -> liftM3 EnumFromThenTo (fix e1) (fix e2) (fix e3)
    ParArrayFromTo e1 e2     -> liftM2 ParArrayFromTo (fix e1) (fix e2)
    ParArrayFromThenTo e1 e2 e3 -> liftM3 ParArrayFromThenTo (fix e1) (fix e2) (fix e3)
    ListComp e quals        -> liftM2 ListComp (fix e) $ mapM fix quals
    ParComp  e qualss       -> liftM2 ParComp (fix e) $ mapM (mapM fix) qualss
    ParArrayComp  e qualss  -> liftM2 ParArrayComp (fix e) $ mapM (mapM fix) qualss
    ExpTypeSig loc e t      -> liftM (flip (ExpTypeSig loc) t) (fix e)
    BracketExp b            -> liftM BracketExp $ fix b
    SpliceExp s             -> liftM SpliceExp $ fix s
    XTag loc n ats mexp cs  -> liftM3 (XTag loc n) (mapM fix ats) (mapM fix mexp) (mapM fix cs)
    XETag loc n ats mexp    -> liftM2 (XETag loc n) (mapM fix ats) (mapM fix mexp)
    XExpTag e               -> liftM XExpTag $ fix e
    XChildTag loc cs        -> liftM (XChildTag loc) $ mapM fix cs
    Proc loc p e            -> liftM2 (Proc loc) (fix p) (fix e)
    LeftArrApp e1 e2        -> liftM2 LeftArrApp (fix e1) (fix e2)
    RightArrApp e1 e2       -> liftM2 RightArrApp (fix e1) (fix e2)
    LeftArrHighApp e1 e2    -> liftM2 LeftArrHighApp (fix e1) (fix e2)
    RightArrHighApp e1 e2   -> liftM2 RightArrHighApp (fix e1) (fix e2)
    CorePragma s e          -> liftM (CorePragma s) (fix e)
    SCCPragma s e           -> liftM (SCCPragma s) (fix e)
    GenPragma s ab cd e     -> liftM (GenPragma s ab cd) (fix e)
    LCase alts              -> liftM LCase $ mapM fix alts

    _                       -> return e'
  where
    fix x = applyFixities fixs x

leafFixP :: Monad m => [Fixity] -> Pat -> m Pat
leafFixP fixs p' = case p' of
        PInfixApp p1 op p2    -> liftM2 (flip PInfixApp op) (leafFixP fixs p1) (fix p2)
        PApp n ps             -> liftM (PApp n) $ mapM fix ps
        PTuple bx ps          -> liftM (PTuple bx) $ mapM fix ps
        PList ps              -> liftM PList $ mapM fix ps
        PParen p              -> liftM PParen $ fix p
        PRec n pfs            -> liftM (PRec n) $ mapM fix pfs
        PAsPat n p            -> liftM (PAsPat n) $ fix p
        PIrrPat p             -> liftM PIrrPat $ fix p
        PatTypeSig loc p t    -> liftM (flip (PatTypeSig loc) t) (fix p)
        PViewPat e p          -> liftM2 PViewPat (fix e) (fix p)
        PRPat rps             -> liftM PRPat $ mapM fix rps
        PXTag loc n ats mp ps -> liftM3 (PXTag loc n) (mapM fix ats) (mapM fix mp) (mapM fix ps)
        PXETag loc n ats mp   -> liftM2 (PXETag loc n) (mapM fix ats) (mapM fix mp)
        PXPatTag p            -> liftM PXPatTag $ fix p
        PXRPats rps           -> liftM PXRPats $ mapM fix rps
        PBangPat p            -> liftM PBangPat $ fix p
        _                     -> return p'
      where fix x = applyFixities fixs x

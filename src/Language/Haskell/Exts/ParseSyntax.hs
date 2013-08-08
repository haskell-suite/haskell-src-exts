{-# OPTIONS_HADDOCK hide #-}
module Language.Haskell.Exts.ParseSyntax where

import Language.Haskell.Exts.Annotated.Syntax hiding ( Type(..), Asst(..), Exp(..), FieldUpdate(..), XAttr(..), Context(..) )
import qualified Language.Haskell.Exts.Annotated.Syntax as S ( Type(..), Asst(..), Exp(..), FieldUpdate(..), XAttr(..), Context(..) )

---------------------------------------
-- Expressions as we parse them (and patterns, and regular patterns)

data PExp l
    = Var l (QName l)                       -- ^ variable
    | IPVar l (IPName l)                    -- ^ implicit parameter variable
    | Con l (QName l)                       -- ^ data constructor
    | Lit l (Literal l)                     -- ^ literal constant
    | InfixApp l (PExp l) (QOp l) (PExp l)  -- ^ infix application
    | App l (PExp l) (PExp l)               -- ^ ordinary application
    | NegApp l (PExp l)                     -- ^ negation expression @-@ /exp/
    | Lambda l [Pat l] (PExp l)             -- ^ lambda expression
    | Let l (Binds l) (PExp l)              -- ^ local declarations with @let@
    | If l (PExp l) (PExp l) (PExp l)       -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    | Case l (PExp l) [Alt l]               -- ^ @case@ /exp/ @of@ /alts/
    | Do l [Stmt l]                         -- ^ @do@-expression:
                                            --   the last statement in the list
                                            --   should be an expression.
    | MDo l [Stmt l]                        -- ^ @mdo@-expression
--    | Tuple [PExp]                        -- ^ tuple expression
    | TupleSection l Boxed [Maybe (PExp l)] -- ^ tuple section expression, e.g. @(,,3)@
    | List l [PExp l]                       -- ^ list expression
    | Paren l (PExp l)                      -- ^ parenthesized expression
--     RightSection QOp PExp                -- ^ right section @(@/qop/ /exp/@)@
    | RecConstr l (QName l) [PFieldUpdate l]
                                            -- ^ record construction expression
    | RecUpdate l (PExp l) [PFieldUpdate l]
                                            -- ^ record update expression
    | EnumFrom l (PExp l)                   -- ^ unbounded arithmetic sequence,
                                            --   incrementing by 1
    | EnumFromTo l (PExp l) (PExp l)        -- ^ bounded arithmetic sequence,
                                            --   incrementing by 1
    | EnumFromThen l (PExp l) (PExp l)      -- ^ unbounded arithmetic sequence,
                                            --   with first two elements given
    | EnumFromThenTo l (PExp l) (PExp l) (PExp l)
                                            -- ^ bounded arithmetic sequence,
                                            --   with first two elements given
    | ParComp l (PExp l) [[QualStmt l]]     -- ^ parallel list comprehension
    | ExpTypeSig l (PExp l) (S.Type l)      -- ^ expression type signature
    | AsPat l (Name l) (PExp l)             -- ^ patterns only
    | WildCard l                            -- ^ patterns only
    | IrrPat l (PExp l)                     -- ^ patterns only

-- Post-ops for parsing left sections and regular patterns. Not to be left in the final tree.
    | PostOp l (PExp l) (QOp l)             -- ^ post-ops
    | PreOp l (QOp l) (PExp l)              -- ^ pre-ops

-- View patterns
    | ViewPat l (PExp l) (PExp l)           -- ^ patterns only

-- HaRP
    | SeqRP l [PExp l]                      -- ^ regular patterns only
    | GuardRP l (PExp l) [Stmt l]           -- ^ regular patterns only
    | EitherRP l (PExp l) (PExp l)          -- ^ regular patterns only
    | CAsRP l (Name l) (PExp l)             -- ^ regular patterns only

-- Template Haskell
    | VarQuote l (QName l)                  -- ^ 'x
    | TypQuote l (QName l)                  -- ^ ''T
    | BracketExp l (Bracket l)
    | SpliceExp l (Splice l)
    | QuasiQuote l String String            -- ^ [$...|...]

-- Hsx
    | XTag  l (XName l) [ParseXAttr l] (Maybe (PExp l)) [PExp l]
                                            -- ^ <Name>...</Name>
    | XETag l (XName l) [ParseXAttr l] (Maybe (PExp l))
                                            -- ^ <Name />
    | XPcdata l String                      -- ^ PCDATA
    | XExpTag l (PExp l)                    -- ^ <% ... %>
    | XChildTag l [PExp l]                  -- ^ <%> ... </%>
    | XRPats l [PExp l]                     -- ^ <[ ... ]>

-- Pragmas
    | CorePragma l      String  (PExp l)    -- ^ {-# CORE #-} pragma
    | SCCPragma  l      String  (PExp l)    -- ^ {-# SCC #-} pragma
    | GenPragma  l      String (Int, Int) (Int, Int) (PExp l)
                                            -- ^ {-# GENERATED ... #-} pragma

-- Generics
    | ExplTypeArg l (QName l) (S.Type l)    -- ^ f {| Int |} x = ...

-- Bang Patterns
    | BangPat l (PExp l)                    -- ^ f !a = ...

-- Arrows
    | Proc l (Pat l) (PExp l)               -- ^ proc p -> do
    | LeftArrApp      l (PExp l) (PExp l)   -- ^ e -< e
    | RightArrApp     l (PExp l) (PExp l)   -- ^ e >- e
    | LeftArrHighApp  l (PExp l) (PExp l)   -- ^ e -<< e
    | RightArrHighApp l (PExp l) (PExp l)   -- ^ e >>- e
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
        TupleSection l bx mes -> l
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
        XChildTag l es          -> l
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
        TupleSection l bx mes   -> TupleSection (f l) bx mes
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
        XChildTag l es          -> XChildTag (f l) es
        XRPats l es             -> XRPats (f l) es

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
          TupleSection l bx mes   -> TupleSection (f l) bx (map (fmap (fmap f)) mes)
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
          XChildTag l es          -> XChildTag (f l) (map (fmap f) es)
          XRPats l es             -> XRPats (f l) (map (fmap f) es)

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

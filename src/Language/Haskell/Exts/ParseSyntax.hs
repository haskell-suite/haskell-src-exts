{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_HADDOCK hide #-}
module Language.Haskell.Exts.ParseSyntax where

import Language.Haskell.Exts.Syntax hiding ( Type(..), Asst(..), Exp(..), FieldUpdate(..), XAttr(..), Context(..) )
import qualified Language.Haskell.Exts.Syntax as S ( Type(..), Promoted(..) )

---------------------------------------
-- Expressions as we parse them (and patterns, and regular patterns)

data PExp l
    = Var l (QName l)                       -- ^ variable
    | OverloadedLabel l String              -- ^ overloaded label #foo
    | IPVar l (IPName l)                    -- ^ implicit parameter variable
    | Con l (QName l)                       -- ^ data constructor
    | Lit l (Literal l)                     -- ^ literal constant
    | InfixApp l (PExp l) (QOp l) (PExp l)  -- ^ infix application
    | App l (PExp l) (PExp l)               -- ^ ordinary application
    | NegApp l (PExp l)                     -- ^ negation expression @-@ /exp/
    | Lambda l [Pat l] (PExp l)             -- ^ lambda expression
    | Let l (Binds l) (PExp l)              -- ^ local declarations with @let@
    | If l (PExp l) (PExp l) (PExp l)       -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    | MultiIf l [GuardedRhs l]              -- ^ @if@ @|@ /stmts/ @->@ /exp/ ...
    | Case l (PExp l) [Alt l]               -- ^ @case@ /exp/ @of@ /alts/
    | Do l [Stmt l]                         -- ^ @do@-expression:
                                            --   the last statement in the list
                                            --   should be an expression.
    | MDo l [Stmt l]                        -- ^ @mdo@-expression
    | TupleSection l Boxed [Maybe (PExp l)] -- ^ tuple section expression, e.g. @(,,3)@
    | UnboxedSum l Int Int (PExp l)         -- ^ Unboxed sum
    | List l [PExp l]                       -- ^ list expression
    | ParArray l [PExp l]                   -- ^ parallel array expression
    | Paren l (PExp l)                      -- ^ parenthesized expression
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
    | ParArrayFromTo l (PExp l) (PExp l)    -- ^ bounded arithmetic sequence,
                                            --   incrementing by 1
    | ParArrayFromThenTo l (PExp l) (PExp l) (PExp l)
                                            -- ^ bounded arithmetic sequence,
                                            --   with first two elements given
    | ParComp l (PExp l) [[QualStmt l]]     -- ^ parallel list comprehension
    | ParArrayComp l (PExp l) [[QualStmt l]] -- ^ parallel array comprehension
    | ExpTypeSig l (PExp l) (S.Type l)      -- ^ expression type signature
    | AsPat l (Name l) (PExp l)             -- ^ patterns only
    | WildCard l                            -- ^ patterns only
    | IrrPat l (PExp l)                     -- ^ patterns only

-- Post-ops for parsing left sections and regular patterns. Not to be left in the final tree.
    | PostOp l (PExp l) (QOp l)             -- ^ post-ops
    | PreOp l (QOp l) (PExp l)              -- ^ pre-ops

-- View patterns
    | ViewPat l (PExp l) (Pat l)            -- ^ patterns only

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

-- Bang Patterns
    | BangPat l (PExp l)                    -- ^ f !a = ...

-- Arrows
    | Proc l (Pat l) (PExp l)               -- ^ proc p -> do
    | LeftArrApp      l (PExp l) (PExp l)   -- ^ e -< e
    | RightArrApp     l (PExp l) (PExp l)   -- ^ e >- e
    | LeftArrHighApp  l (PExp l) (PExp l)   -- ^ e -<< e
    | RightArrHighApp l (PExp l) (PExp l)   -- ^ e >>- e

-- LambdaCase
    | LCase l [Alt l]                       -- ^ @\case@ /alts/
    | TypeApp l (S.Type l)
   deriving (Eq,Show,Functor)

data PFieldUpdate l
    = FieldUpdate l (QName l) (PExp l)
    | FieldPun l (QName l)
    | FieldWildcard l
  deriving (Eq,Show,Functor)

data ParseXAttr l = XAttr l (XName l) (PExp l)
  deriving (Eq,Show,Functor)

instance Annotated PExp where
    ann e = case e of
        Var l _                 -> l
        OverloadedLabel l _     -> l
        IPVar l _               -> l
        Con l _                 -> l
        Lit l _                 -> l
        InfixApp l _ _ _        -> l
        App l _ _               -> l
        NegApp l _              -> l
        Lambda l _ _            -> l
        Let l _ _               -> l
        If l _ _ _              -> l
        Case l _ _              -> l
        Do l _                  -> l
        MDo l _                 -> l
        TupleSection l _ _      -> l
        UnboxedSum l _ _ _      -> l
        List l _                -> l
        ParArray l _            -> l
        Paren l _               -> l
        RecConstr l _ _         -> l
        RecUpdate l _  _        -> l
        EnumFrom l _            -> l
        EnumFromTo l _ _        -> l
        EnumFromThen l _ _      -> l
        EnumFromThenTo l _ _ _  -> l
        ParArrayFromTo l _ _    -> l
        ParArrayFromThenTo l _ _ _ -> l
        ParComp  l _ _          -> l
        ParArrayComp  l _ _     -> l
        ExpTypeSig l _ _        -> l
        AsPat l _ _             -> l
        WildCard l              -> l
        IrrPat l _              -> l
        PostOp l _ _            -> l
        PreOp l _ _             -> l
        ViewPat l _ _           -> l
        SeqRP l _               -> l
        GuardRP l _ _           -> l
        EitherRP l _ _          -> l
        CAsRP l _ _             -> l

        VarQuote l _            -> l
        TypQuote l _            -> l
        BracketExp l _          -> l
        SpliceExp l _           -> l
        QuasiQuote l _ _        -> l

        XTag  l _ _ _ _         -> l
        XETag l _  _ _          -> l
        XPcdata l _             -> l
        XExpTag l _             -> l
        XChildTag l _           -> l
        XRPats l _              -> l

        CorePragma l _ _        -> l
        SCCPragma  l _ _        -> l
        GenPragma  l _ _ _ _    -> l

        BangPat l _             -> l

        Proc            l _ _   -> l
        LeftArrApp      l _ _   -> l
        RightArrApp     l _ _   -> l
        LeftArrHighApp  l _ _   -> l
        RightArrHighApp l _ _   -> l

        LCase l _               -> l
        MultiIf l _             -> l
        TypeApp l _             -> l


    amap f e' = case e' of
        Var l qn                -> Var   (f l) qn
        OverloadedLabel l qn    -> OverloadedLabel (f l) qn
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
        UnboxedSum l b a e     -> UnboxedSum (f l) b a e
        List l es               -> List (f l) es
        ParArray l es           -> ParArray (f l) es
        Paren l e               -> Paren (f l) e
        RecConstr l qn fups     -> RecConstr (f l) qn fups
        RecUpdate l e  fups     -> RecUpdate (f l) e  fups
        EnumFrom l e            -> EnumFrom (f l) e
        EnumFromTo l ef et      -> EnumFromTo (f l) ef et
        EnumFromThen l ef et    -> EnumFromThen (f l) ef et
        EnumFromThenTo l ef eth eto -> EnumFromThenTo (f l) ef eth eto
        ParArrayFromTo l ef et  -> ParArrayFromTo (f l) ef et
        ParArrayFromThenTo l ef eth eto -> ParArrayFromThenTo (f l) ef eth eto
        ParComp  l e qsss       -> ParComp  (f l) e qsss
        ParArrayComp  l e qsss  -> ParArrayComp  (f l) e qsss
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

        LCase l alts -> LCase (f l) alts
        MultiIf l alts -> MultiIf (f l) alts
        TypeApp l ty -> TypeApp (f l) ty

instance Annotated PFieldUpdate where
    ann (FieldUpdate l _  _) = l
    ann (FieldPun l _)       = l
    ann (FieldWildcard l)    = l
    amap f (FieldUpdate l qn e) = FieldUpdate (f l) qn e
    amap f (FieldPun l n)       = FieldPun (f l) n
    amap f (FieldWildcard l)    = FieldWildcard (f l)

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
    | CxEmpty  l
 deriving (Eq, Show, Functor)

instance Annotated PContext where
  ann (CxSingle l _ ) = l
  ann (CxTuple  l _)  = l
  ann (CxEmpty  l)       = l
  amap f (CxSingle l asst ) = CxSingle (f l) asst
  amap f (CxTuple  l assts) = CxTuple  (f l) assts
  amap f (CxEmpty l) = CxEmpty (f l)

data PType l
     = TyForall l
        (Maybe [TyVarBind l])
        (Maybe (PContext l))
        (PType l)
     | TyFun   l (PType l) (PType l)            -- ^ function type
     | TyTuple l Boxed     [PType l]            -- ^ tuple type, possibly boxed
     | TyUnboxedSum l [PType l]                 -- ^ unboxed sum
     | TyList  l (PType l)                      -- ^ list syntax, e.g. [a], as opposed to [] a
     | TyParArray l (PType l)                   -- ^ parallel array syntax, e.g. [:a:]
     | TyApp   l (PType l) (PType l)            -- ^ application of a type constructor
     | TyVar   l (Name l)                       -- ^ type variable
     | TyCon   l (QName l)                      -- ^ named type or type constructor
     | TyParen l (PType l)                      -- ^ type surrounded by parentheses
     | TyPred  l (PAsst l)                      -- ^ assertion of an implicit parameter
     | TyInfix l (PType l) (MaybePromotedName l) (PType l)  -- ^ infix type constructor
     | TyKind  l (PType l) (Kind l)             -- ^ type with explicit kind signature
     | TyPromoted l (S.Promoted l)              -- ^ promoted data type
     | TySplice l (Splice l)                    -- ^ template haskell splice type
     | TyBang l (BangType l) (Unpackedness l) (PType l) -- ^ Strict type marked with \"@!@\" or type marked with UNPACK pragma.
     | TyWildCard l (Maybe (Name l))            -- ^ Type wildcard
     | TyQuasiQuote l String String             -- ^ @[qq| |]@
  deriving (Eq, Show, Functor)

instance Annotated PType where
    ann t = case t of
      TyForall l _ _ _              -> l
      TyFun   l _ _                 -> l
      TyTuple l _ _                 -> l
      TyUnboxedSum l _              -> l
      TyList  l _                   -> l
      TyParArray  l _               -> l
      TyApp   l _ _                 -> l
      TyVar   l _                   -> l
      TyCon   l _                   -> l
      TyParen l _                   -> l
      TyInfix l _ _ _               -> l
      TyKind  l _ _                 -> l
      TyPromoted l   _              -> l
      TyPred l _                    -> l
      TySplice l _                  -> l
      TyBang  l _ _ _                 -> l
      TyWildCard  l _               -> l
      TyQuasiQuote l _ _            -> l
    amap f t' = case t' of
      TyForall l mtvs mcx t         -> TyForall (f l) mtvs mcx t
      TyFun   l t1 t2               -> TyFun (f l) t1 t2
      TyTuple l b ts                -> TyTuple (f l) b ts
      TyUnboxedSum l ts             -> TyUnboxedSum (f l) ts
      TyList  l t                   -> TyList (f l) t
      TyParArray  l t               -> TyParArray (f l) t
      TyApp   l t1 t2               -> TyApp (f l) t1 t2
      TyVar   l n                   -> TyVar (f l) n
      TyCon   l qn                  -> TyCon (f l) qn
      TyParen l t                   -> TyParen (f l) t
      TyInfix l ta qn tb            -> TyInfix (f l) ta qn tb
      TyKind  l t k                 -> TyKind (f l) t k
      TyPromoted l   p              -> TyPromoted (f l)   p
      TyPred l asst                 -> TyPred (f l) asst
      TySplice l s                  -> TySplice (f l) s
      TyBang  l b u t                 -> TyBang (f l) b u t
      TyWildCard l mn               -> TyWildCard (f l) mn
      TyQuasiQuote l n s            -> TyQuasiQuote (f l) n s

data PAsst l
    = ClassA l (QName l) [PType l]
    | AppA l (Name l) [PType l]
    | InfixA l (PType l) (QName l) (PType l)
    | IParam l (IPName l) (PType l)
    | EqualP l (PType l)  (PType l)
    | ParenA l (PAsst l)
    | WildCardA l (Maybe (Name l))
  deriving (Eq, Show, Functor)

instance Annotated PAsst where
    ann asst = case asst of
        ClassA l _ _        -> l
        AppA l _ _          -> l
        InfixA l _ _ _      -> l
        IParam l _ _        -> l
        EqualP l _ _        -> l
        ParenA l _          -> l
        WildCardA l _       -> l
    amap f asst = case asst of
        ClassA l qn ts      -> ClassA (f l) qn ts
        AppA l t ts         -> AppA (f l) t ts
        InfixA l ta qn tb   -> InfixA (f l) ta qn tb
        IParam l ipn t      -> IParam (f l) ipn t
        EqualP l t1 t2      -> EqualP (f l) t1 t2
        ParenA l a          -> ParenA (f l) a
        WildCardA l mn      -> WildCardA (f l) mn

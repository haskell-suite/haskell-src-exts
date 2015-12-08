{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Annotated.Syntax
-- Copyright   :  (c) Niklas Broberg 2004-2009,
--                (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- A suite of datatypes describing the (semi-concrete) abstract syntax of Haskell 98
-- <http://www.haskell.org/onlinereport/> plus registered extensions, including:
--
--   * multi-parameter type classes with functional dependencies (MultiParamTypeClasses, FunctionalDependencies)
--
--   * parameters of type class assertions are unrestricted (FlexibleContexts)
--
--   * 'forall' types as universal and existential quantification (RankNTypes, ExistentialQuantification, etc)
--
--   * pattern guards (PatternGuards)
--
--   * implicit parameters (ImplicitParameters)
--
--   * generalised algebraic data types (GADTs)
--
--   * template haskell (TemplateHaskell)
--
--   * empty data type declarations (EmptyDataDecls)
--
--   * unboxed tuples (UnboxedTuples)
--
--   * regular patterns (RegularPatterns)
--
--   * HSP-style XML expressions and patterns (XmlSyntax)
--
-- All nodes in the syntax tree are annotated with something of a user-definable data type.
-- When parsing, this annotation will contain information about the source location that the
-- particular node comes from.
--
-----------------------------------------------------------------------------

module Language.Haskell.Exts.Annotated.Syntax (
    -- * Modules
    Module(..), ModuleHead(..), WarningText(..), ExportSpecList(..), ExportSpec(..),
    ImportDecl(..), ImportSpecList(..), ImportSpec(..), Assoc(..), Namespace(..),
    -- * Declarations
    Decl(..), DeclHead(..), InstRule(..), InstHead(..), Binds(..), IPBind(..), PatternSynDirection(..),
    -- ** Type classes and instances
    ClassDecl(..), InstDecl(..), Deriving(..),
    -- ** Data type declarations
    DataOrNew(..), ConDecl(..), FieldDecl(..), QualConDecl(..), GadtDecl(..), BangType(..),
    -- ** Function bindings
    Match(..), Rhs(..), GuardedRhs(..),
    -- * Class Assertions and Contexts
    Context(..), FunDep(..), Asst(..),
    -- * Types
    Type(..), Boxed(..), Kind(..), TyVarBind(..), Promoted(..),
    TypeEqn (..),
    -- * Expressions
    Exp(..), Stmt(..), QualStmt(..), FieldUpdate(..),
    Alt(..), XAttr(..),
    -- * Patterns
    Pat(..), PatField(..), PXAttr(..), RPat(..), RPatOp(..),
    -- * Literals
    Literal(..), Sign(..),
    -- * Variables, Constructors and Operators
    ModuleName(..), QName(..), Name(..), QOp(..), Op(..),
    SpecialCon(..), CName(..), IPName(..), XName(..), Role(..),

    -- * Template Haskell
    Bracket(..), Splice(..),

    -- * FFI
    Safety(..), CallConv(..),

    -- * Pragmas
    ModulePragma(..), Tool(..), Overlap(..),
    Rule(..), RuleVar(..), Activation(..),
    Annotation(..), BooleanFormula(..),

    -- * Builtin names

    -- ** Modules
    prelude_mod, main_mod,
    -- ** Main function of a program
    main_name,
    -- ** Constructors
    unit_con_name, tuple_con_name, list_cons_name, unboxed_singleton_con_name,
    unit_con, tuple_con, unboxed_singleton_con,
    -- ** Special identifiers
    as_name, qualified_name, hiding_name, minus_name, bang_name, dot_name, star_name,
    export_name, safe_name, unsafe_name, interruptible_name, threadsafe_name,
    stdcall_name, ccall_name, cplusplus_name, dotnet_name, jvm_name, js_name,
    javascript_name, capi_name, forall_name, family_name, role_name,
    -- ** Type constructors
    unit_tycon_name, fun_tycon_name, list_tycon_name, tuple_tycon_name, unboxed_singleton_tycon_name,
    unit_tycon, fun_tycon, list_tycon, tuple_tycon, unboxed_singleton_tycon,

    -- * Source coordinates
    -- SrcLoc(..),

    -- * Annotated trees
    Annotated(..), (=~=),
  ) where

import Prelude hiding (id)

import Data.Data
import GHC.Generics (Generic)
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

-- | The name of a Haskell module.
data ModuleName l = ModuleName l String
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Constructors with special syntax.
-- These names are never qualified, and always refer to builtin type or
-- data constructors.
data SpecialCon l
    = UnitCon l             -- ^ unit type and data constructor @()@
    | ListCon l             -- ^ list type constructor @[]@
    | FunCon  l             -- ^ function type constructor @->@
    | TupleCon l Boxed Int  -- ^ /n/-ary tuple type and data
                            --   constructors @(,)@ etc, possibly boxed @(\#,\#)@
    | Cons l                -- ^ list data constructor @(:)@
    | UnboxedSingleCon l    -- ^ unboxed singleton tuple constructor @(\# \#)@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | This type is used to represent qualified variables, and also
-- qualified constructors.
data QName l
    = Qual    l (ModuleName l) (Name l) -- ^ name qualified with a module name
    | UnQual  l                (Name l) -- ^ unqualified local name
    | Special l (SpecialCon l)          -- ^ built-in constructor with special syntax
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | This type is used to represent variables, and also constructors.
data Name l
    = Ident  l String   -- ^ /varid/ or /conid/.
    | Symbol l String   -- ^ /varsym/ or /consym/
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An implicit parameter name.
data IPName l
    = IPDup l String -- ^ ?/ident/, non-linear implicit parameter
    | IPLin l String -- ^ %/ident/, linear implicit parameter
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Possibly qualified infix operators (/qop/), appearing in expressions.
data QOp l
    = QVarOp l (QName l) -- ^ variable operator (/qvarop/)
    | QConOp l (QName l) -- ^ constructor operator (/qconop/)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Operators appearing in @infix@ declarations are never qualified.
data Op l
    = VarOp l (Name l)    -- ^ variable operator (/varop/)
    | ConOp l (Name l)    -- ^ constructor operator (/conop/)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A name (/cname/) of a component of a class or data type in an @import@
-- or export specification.
data CName l
    = VarName l (Name l) -- ^ name of a method or field
    | ConName l (Name l) -- ^ name of a data constructor
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A complete Haskell source module.
data Module l
    = Module l (Maybe (ModuleHead l)) [ModulePragma l] [ImportDecl l] [Decl l]
    -- ^ an ordinary Haskell module
    | XmlPage l (ModuleName l) [ModulePragma l] (XName l) [XAttr l] (Maybe (Exp l)) [Exp l]
    -- ^ a module consisting of a single XML document. The ModuleName never appears in the source
    --   but is needed for semantic purposes, it will be the same as the file name.
    | XmlHybrid l (Maybe (ModuleHead l)) [ModulePragma l] [ImportDecl l] [Decl l]
                (XName l) [XAttr l] (Maybe (Exp l)) [Exp l]
    -- ^ a hybrid module combining an XML document with an ordinary module
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | The head of a module, including the name and export specification.
data ModuleHead l = ModuleHead l (ModuleName l) (Maybe (WarningText l)) (Maybe (ExportSpecList l))
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An explicit export specification.
data ExportSpecList l
    = ExportSpecList l [ExportSpec l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An item in a module's export specification.
data ExportSpec l
     = EVar l (QName l)                 -- ^ variable.
     | EAbs l (Namespace l) (QName l)   -- ^ @T@:
                                        --   a class or datatype exported abstractly,
                                        --   or a type synonym.
     | EThingAll l (QName l)            -- ^ @T(..)@:
                                        --   a class exported with all of its methods, or
                                        --   a datatype exported with all of its constructors.
     | EThingWith l (QName l) [CName l] -- ^ @T(C_1,...,C_n)@:
                                        --   a class exported with some of its methods, or
                                        --   a datatype exported with some of its constructors.
     | EModuleContents l (ModuleName l) -- ^ @module M@:
                                        --   re-export a module.
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Namespaces for imports/exports.
data Namespace l = NoNamespace l | TypeNamespace l | PatternNamespace l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An import declaration.
data ImportDecl l = ImportDecl
    { importAnn :: l                   -- ^ annotation, used by parser for position of the @import@ keyword.
    , importModule :: ModuleName l     -- ^ name of the module imported.
    , importQualified :: Bool          -- ^ imported @qualified@?
    , importSrc :: Bool                -- ^ imported with @{-\# SOURCE \#-}@?
    , importSafe :: Bool               -- ^ Import @safe@?
    , importPkg :: Maybe String        -- ^ imported with explicit package name
    , importAs :: Maybe (ModuleName l) -- ^ optional alias name in an @as@ clause.
    , importSpecs :: Maybe (ImportSpecList l)
            -- ^ optional list of import specifications.
    }
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An explicit import specification list.
data ImportSpecList l
    = ImportSpecList l Bool [ImportSpec l]
            -- A list of import specifications.
            -- The 'Bool' is 'True' if the names are excluded
            -- by @hiding@.
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An import specification, representing a single explicit item imported
--   (or hidden) from a module.
data ImportSpec l
     = IVar l (Name l)                  -- ^ variable
     | IAbs l (Namespace l) (Name l)    -- ^ @T@:
                                        --   the name of a class, datatype or type synonym.
     | IThingAll l (Name l)             -- ^ @T(..)@:
                                        --   a class imported with all of its methods, or
                                        --   a datatype imported with all of its constructors.
     | IThingWith l (Name l) [CName l]  -- ^ @T(C_1,...,C_n)@:
                                        --   a class imported with some of its methods, or
                                        --   a datatype imported with some of its constructors.
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Associativity of an operator.
data Assoc l
     = AssocNone  l -- ^ non-associative operator (declared with @infix@)
     | AssocLeft  l -- ^ left-associative operator (declared with @infixl@).
     | AssocRight l -- ^ right-associative operator (declared with @infixr@)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A top-level declaration.
data Decl l
     = TypeDecl     l (DeclHead l) (Type l)
     -- ^ A type declaration
     | TypeFamDecl  l (DeclHead l) (Maybe (Kind l))
     -- ^ A type family declaration
     | ClosedTypeFamDecl  l (DeclHead l) (Maybe (Kind l))  [TypeEqn l]
     -- ^ A closed type family declaration
     | DataDecl     l (DataOrNew l) (Maybe (Context l)) (DeclHead l)                  [QualConDecl l] (Maybe (Deriving l))
     -- ^ A data OR newtype declaration
     | GDataDecl    l (DataOrNew l) (Maybe (Context l)) (DeclHead l) (Maybe (Kind l)) [GadtDecl l]    (Maybe (Deriving l))
     -- ^ A data OR newtype declaration, GADT style
     | DataFamDecl  l {-data-}      (Maybe (Context l)) (DeclHead l) (Maybe (Kind l))
     -- ^ A data family declaration
     | TypeInsDecl  l (Type l) (Type l)
     -- ^ A type family instance declaration
     | DataInsDecl  l (DataOrNew l) (Type l)                  [QualConDecl l] (Maybe (Deriving l))
     -- ^ A data family instance declaration
     | GDataInsDecl l (DataOrNew l) (Type l) (Maybe (Kind l)) [GadtDecl l]    (Maybe (Deriving l))
     -- ^ A data family instance declaration, GADT style
     | ClassDecl    l (Maybe (Context l)) (DeclHead l) [FunDep l] (Maybe [ClassDecl l])
     -- ^ A declaration of a type class
     | InstDecl     l (Maybe (Overlap l)) (InstRule l) (Maybe [InstDecl l])
     -- ^ An declaration of a type class instance
     | DerivDecl    l (Maybe (Overlap l)) (InstRule l)
     -- ^ A standalone deriving declaration
     | InfixDecl    l (Assoc l) (Maybe Int) [Op l]
     -- ^ A declaration of operator fixity
     | DefaultDecl  l [Type l]
     -- ^ A declaration of default types
     | SpliceDecl   l (Exp l)
     -- ^ A Template Haskell splicing declaration
     | TypeSig      l [Name l] (Type l)
     -- ^ A type signature declaration
     | PatSynSig    l (Name l) (Maybe [TyVarBind l]) (Maybe (Context l)) (Maybe (Context l)) (Type l)
     -- ^ A pattern synonym signature declation
     | FunBind      l [Match l]
     -- ^ A set of function binding clauses
     | PatBind      l (Pat l) (Rhs l) {-where-} (Maybe (Binds l))
     -- ^ A pattern binding
     | PatSyn l (Pat l) (Pat l) (PatternSynDirection l)
     -- ^ A pattern synonym binding
     | ForImp       l (CallConv l) (Maybe (Safety l)) (Maybe String) (Name l) (Type l)
     -- ^ A foreign import declaration
     | ForExp       l (CallConv l)                    (Maybe String) (Name l) (Type l)
     -- ^ A foreign export declaration
     | RulePragmaDecl   l [Rule l]
     -- ^ A RULES pragma
     | DeprPragmaDecl   l [([Name l], String)]
     -- ^ A DEPRECATED pragma
     | WarnPragmaDecl   l [([Name l], String)]
     -- ^ A WARNING pragma
     | InlineSig        l Bool (Maybe (Activation l)) (QName l)
     -- ^ An INLINE pragma
     | InlineConlikeSig l      (Maybe (Activation l)) (QName l)
     -- ^ An INLINE CONLIKE pragma
     | SpecSig          l      (Maybe (Activation l)) (QName l) [Type l]
     -- ^ A SPECIALISE pragma
     | SpecInlineSig    l Bool (Maybe (Activation l)) (QName l) [Type l]
     -- ^ A SPECIALISE INLINE pragma
     | InstSig          l      (InstRule l)
     -- ^ A SPECIALISE instance pragma
     | AnnPragma        l (Annotation l)
     -- ^ An ANN pragma
     | MinimalPragma    l (Maybe (BooleanFormula l))
     -- ^ A MINIMAL pragma
     | RoleAnnotDecl    l (QName l) [Role l]
     -- ^ A role annotation
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

data  PatternSynDirection l =
      Unidirectional -- ^ A unidirectional pattern synonym with "<-"
    | ImplicitBidirectional  -- ^ A bidirectional pattern synonym with "="
    | ExplicitBidirectional l [Decl l]  -- ^ A birectional pattern synonym with the construction specified.
    deriving (Eq, Ord, Show, Data, Typeable, Foldable, Traversable, Functor, Generic)

-- | A type equation as found in closed type families.
data TypeEqn l = TypeEqn l (Type l) (Type l) deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An annotation through an ANN pragma.
data Annotation l
    = Ann       l (Name l)  (Exp l)
    -- ^ An annotation for a declared name.
    | TypeAnn   l (Name l)  (Exp l)
    -- ^ An annotation for a declared type.
    | ModuleAnn l           (Exp l)
    -- ^ An annotation for the defining module.
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A boolean formula for MINIMAL pragmas.
data BooleanFormula l
    = VarFormula l (Name l)              -- ^ A variable.
    | AndFormula l [BooleanFormula l]    -- ^ And boolean formulas.
    | OrFormula l [BooleanFormula l]     -- ^ Or boolean formulas.
    | ParenFormula l (BooleanFormula l)  -- ^ Parenthesized boolean formulas.
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

data Role l
  = Nominal l
  | Representational l
  | Phantom l
  | RoleWildcard l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A flag stating whether a declaration is a data or newtype declaration.
data DataOrNew l = DataType l | NewType l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | The head of a type or class declaration, which consists of the type
-- or class name applied to some type variables
--
-- @class C a b@ is represented as
--
-- >DHApp
-- >   ()
-- >   (DHApp
-- >      () (DHead () (Ident () "C")) (UnkindedVar () (Ident () "a")))
-- >   (UnkindedVar () (Ident () "b"))
--
-- (where the annotation type @l@ is instantiated with @()@)
--
-- @class (a :< b) c@ is represented as
--
-- >DHApp
-- >   ()
-- >   (DHParen
-- >      ()
-- >      (DHApp
-- >         ()
-- >         (DHInfix () (UnkindedVar () (Ident () "a")) (Symbol () ":<"))
-- >         (UnkindedVar () (Ident () "b"))))
-- >   (UnkindedVar () (Ident () "c"))
data DeclHead l
    = DHead l (Name l) -- ^ type or class name
    | DHInfix l (TyVarBind l) (Name l) -- ^ infix application of the type/class name to the left operand
    | DHParen l (DeclHead l) -- ^ parenthesized declaration head
    | DHApp   l (DeclHead l) (TyVarBind l) -- ^ application to one more type variable
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | The instance declaration rule, which is, roughly, the part of the instance declaration before the @where@ keyword.
--
-- Example: @instance Ord a => Ord (Maybe a)@ is represented as
--
-- >IRule
-- >   ()
-- >   Nothing
-- >   (Just
-- >      (CxSingle
-- >         ()
-- >         (ClassA
-- >            () (UnQual () (Ident () "Ord")) [ TyVar () (Ident () "a") ])))
-- >   (IHApp
-- >      ()
-- >      (IHCon () (UnQual () (Ident () "Ord")))
-- >      (TyParen
-- >         ()
-- >         (TyApp
-- >            ()
-- >            (TyCon () (UnQual () (Ident () "Maybe")))
-- >            (TyVar () (Ident () "a")))))
--
-- An optional explicit forall after @instance@ is supported:
-- @instance forall a . Ord a => Ord (Maybe a) where@ becomes
--
-- >IRule
-- >   ()
-- >   (Just [ UnkindedVar () (Ident () "a") ])
-- >   ...
data InstRule l
    = IRule l (Maybe [TyVarBind l]) (Maybe (Context l)) (InstHead l)
    | IParen l (InstRule l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- See bugs #7 and #31 for more details and use cases for the rationale
-- of the split. DeclOrInstHead should be used by DeclHead as the name implies.

-- | The instance head. The split between rule/head allow us to represent
-- @instance (Bounded a => Bounded [a]) where@ faithfully.
--
-- The structure of 'InstHead' follows one of 'DeclHead'.
--
-- For example, @instance C (Maybe a) Int where@ is represented as
--
-- >IHApp
-- >   ()
-- >   (IHApp
-- >      ()
-- >      (IHCon () (UnQual () (Ident () "C")))
-- >      (TyParen
-- >         ()
-- >         (TyApp
-- >            ()
-- >            (TyCon () (UnQual () (Ident () "Maybe")))
-- >            (TyVar () (Ident () "a")))))
-- >   (TyCon () (UnQual () (Ident () "Int")))))
data InstHead l
    = IHCon l (QName l) -- ^ type or class name
    | IHInfix l (Type l) (QName l) -- ^ infix application of the type/class name to the left operand
    | IHParen l (InstHead l) -- ^ parenthesized instance head
    | IHApp   l (InstHead l) (Type l) -- ^ application to one more type
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A deriving clause following a data type declaration.
data Deriving l = Deriving l [InstRule l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A binding group inside a @let@ or @where@ clause.
data Binds l
    = BDecls  l [Decl l]     -- ^ An ordinary binding group
    | IPBinds l [IPBind l]   -- ^ A binding group for implicit parameters
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A binding of an implicit parameter.
data IPBind l = IPBind l (IPName l) (Exp l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Clauses of a function binding.
data Match l
     = Match l      (Name l) [Pat l]         (Rhs l) {-where-} (Maybe (Binds l))
        -- ^ A clause defined with prefix notation, i.e. the function name
        --  followed by its argument patterns, the right-hand side and an
        --  optional where clause.
     | InfixMatch l (Pat l) (Name l) [Pat l] (Rhs l) {-where-} (Maybe (Binds l))
        -- ^ A clause defined with infix notation, i.e. first its first argument
        --  pattern, then the function name, then its following argument(s),
        --  the right-hand side and an optional where clause.
        --  Note that there can be more than two arguments to a function declared
        --  infix, hence the list of pattern arguments.
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A single constructor declaration within a data type declaration,
--   which may have an existential quantification binding.
data QualConDecl l
    = QualConDecl l
        {-forall-} (Maybe [TyVarBind l]) {- . -} (Maybe (Context l))
        {- => -} (ConDecl l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Declaration of an ordinary data constructor.
data ConDecl l
     = ConDecl l (Name l) [Type l]
                -- ^ ordinary data constructor
     | InfixConDecl l (Type l) (Name l) (Type l)
                -- ^ infix data constructor
     | RecDecl l (Name l) [FieldDecl l]
                -- ^ record constructor
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Declaration of a (list of) named field(s).
data FieldDecl l = FieldDecl l [Name l] (Type l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)


-- | A single constructor declaration in a GADT data type declaration.
--
-- If the GADT is declared using the record syntax, e.g.
--
-- >data Ty where
-- >  TCon :: { field1 :: Int, field2 :: Bool } -> Ty
--
-- then the fields are stored as a list of 'FieldDecl's, and the final type
-- (@Ty@ in the above example) is stored in the last 'Type' field.
--
-- If the GADT is declared using the ordinary syntax, e.g.
--
-- >data Ty where
-- >  TCon :: Int -> Bool -> Ty
--
-- then @'Maybe' ['FieldDecl' l]@ is 'Nothing', and the whole constructor's
-- type (such as @Int -> Bool -> Ty@) is stored in the last 'Type' field.
data GadtDecl l
    = GadtDecl l (Name l) (Maybe [FieldDecl l]) (Type l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Declarations inside a class declaration.
data ClassDecl l
    = ClsDecl    l (Decl l)
            -- ^ ordinary declaration
    | ClsDataFam l (Maybe (Context l)) (DeclHead l) (Maybe (Kind l))
            -- ^ declaration of an associated data type
    | ClsTyFam   l                     (DeclHead l) (Maybe (Kind l))
            -- ^ declaration of an associated type synonym
    | ClsTyDef   l (Type l) (Type l)
            -- ^ default choice for an associated type synonym
    | ClsDefSig  l (Name l) (Type l)
            -- ^ default signature
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Declarations inside an instance declaration.
data InstDecl l
    = InsDecl   l (Decl l)
            -- ^ ordinary declaration
    | InsType   l (Type l) (Type l)
            -- ^ an associated type definition
    | InsData   l (DataOrNew l) (Type l) [QualConDecl l] (Maybe (Deriving l))
            -- ^ an associated data type implementation
    | InsGData  l (DataOrNew l) (Type l) (Maybe (Kind l)) [GadtDecl l] (Maybe (Deriving l))
            -- ^ an associated data type implemented using GADT style
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | The type of a constructor argument or field, optionally including
--   a strictness annotation.
data BangType l
     = BangedTy   l -- ^ strict component, marked with \"@!@\"
     | UnpackedTy l -- ^ unboxed component, marked with an UNPACK pragma
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | The right hand side of a function binding, pattern binding, or a case
--   alternative.
data Rhs l
     = UnGuardedRhs l (Exp l) -- ^ unguarded right hand side (/exp/)
     | GuardedRhss  l [GuardedRhs l]
                -- ^ guarded right hand side (/gdrhs/)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A guarded right hand side @|@ /stmts/ @=@ /exp/, or @|@ /stmts/ @->@ /exp/
--   for case alternatives.
--   The guard is a series of statements when using pattern guards,
--   otherwise it will be a single qualifier expression.
data GuardedRhs l
     = GuardedRhs l [Stmt l] (Exp l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A type qualified with a context.
--   An unqualified type has an empty context.
data Type l
     = TyForall l
        (Maybe [TyVarBind l])
        (Maybe (Context l))
        (Type l)                                -- ^ qualified type
     | TyFun   l (Type l) (Type l)              -- ^ function type
     | TyTuple l Boxed [Type l]                 -- ^ tuple type, possibly boxed
     | TyList  l (Type l)                       -- ^ list syntax, e.g. [a], as opposed to [] a
     | TyParArray  l (Type l)                   -- ^ parallel array syntax, e.g. [:a:]
     | TyApp   l (Type l) (Type l)              -- ^ application of a type constructor
     | TyVar   l (Name l)                       -- ^ type variable
     | TyCon   l (QName l)                      -- ^ named type or type constructor
     | TyParen l (Type l)                       -- ^ type surrounded by parentheses
     | TyInfix l (Type l) (QName l) (Type l)    -- ^ infix type constructor
     | TyKind  l (Type l) (Kind l)              -- ^ type with explicit kind signature
     | TyPromoted l (Promoted l)                -- ^ @'K@, a promoted data type (-XDataKinds).
     | TyEquals l (Type l) (Type l)             -- ^ type equality predicate enabled by ConstraintKinds
     | TySplice l (Splice l)                    -- ^ template haskell splice type
     | TyBang l (BangType l) (Type l)           -- ^ Strict type marked with \"@!@\" or type marked with UNPACK pragma.
     | TyWildCard l (Maybe (Name l))            -- ^ Either an anonymous of named type wildcard
     | TyQuasiQuote l String String             -- ^ @[$/name/| /string/ |]@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Bools here are True if there was a leading quote which may be
-- left out. For example @'[k1,k2]@ means the same thing as @[k1,k2]@.
data Promoted l
        = PromotedInteger l Integer String -- ^ parsed value and raw string
        | PromotedString l String String -- ^ parsed value and raw string
        | PromotedCon l Bool (QName l)
        | PromotedList l Bool [Type l]
        | PromotedTuple l [Type l]
        | PromotedUnit l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Flag denoting whether a tuple is boxed or unboxed.
data Boxed = Boxed | Unboxed
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A type variable declaration, optionally with an explicit kind annotation.
data TyVarBind l
    = KindedVar   l (Name l) (Kind l)  -- ^ variable binding with kind annotation
    | UnkindedVar l (Name l)           -- ^ ordinary variable binding
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An explicit kind annotation.
data Kind l
    = KindStar  l                    -- ^ @*@, the kind of types
    | KindFn    l (Kind l) (Kind l)  -- ^ @->@, the kind of a type constructor
    | KindParen l (Kind l)           -- ^ a parenthesised kind
    | KindVar   l (QName l)          -- ^ @k@, a kind variable (-XPolyKinds)
    | KindApp   l (Kind l) (Kind l)  -- ^ @k1 k2@
    | KindTuple l [Kind l]           -- ^ @'(k1,k2,k3)@, a promoted tuple
    | KindList  l (Kind l)           -- ^ @'[k1]@, a promoted list literal
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)


-- | A functional dependency, given on the form
--   l1 l2 ... ln -> r2 r3 .. rn
data FunDep l
    = FunDep l [Name l] [Name l]
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A context is a set of assertions
data Context l
    = CxSingle l (Asst l)
    | CxTuple  l [Asst l]
    | CxEmpty  l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Class assertions.
--   In Haskell 98, the argument would be a /tyvar/, but this definition
--   allows multiple parameters, and allows them to be /type/s.
--   Also extended with support for implicit parameters and equality constraints.
data Asst l
        = ClassA l (QName l) [Type l]           -- ^ ordinary class assertion
        | AppA l (Name l) [Type l]              -- ^ constraint kind assertion, @Dict :: cxt a => Dict cxt@
        | InfixA l (Type l) (QName l) (Type l)  -- ^ class assertion where the class name is given infix
        | IParam l (IPName l) (Type l)          -- ^ implicit parameter assertion
        | EqualP l (Type l) (Type l)            -- ^ type equality constraint
        | ParenA l (Asst l)                     -- ^ parenthesised class assertion
        | WildCardA l (Maybe (Name l))          -- ^ Context Wildcard
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | /literal/
-- Values of this type hold the abstract value of the literal, along with the
-- precise string representation used.  For example, @10@, @0o12@ and @0xa@
-- have the same value representation, but each carry a different string representation.
data Literal l
    = Char       l Char     String     -- ^ character literal
    | String     l String   String     -- ^ string literal
    | Int        l Integer  String     -- ^ integer literal
    | Frac       l Rational String     -- ^ floating point literal
    | PrimInt    l Integer  String     -- ^ unboxed integer literal
    | PrimWord   l Integer  String     -- ^ unboxed word literal
    | PrimFloat  l Rational String     -- ^ unboxed float literal
    | PrimDouble l Rational String     -- ^ unboxed double literal
    | PrimChar   l Char     String     -- ^ unboxed character literal
    | PrimString l String   String     -- ^ unboxed string literal
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An indication whether a literal pattern has been negated or not.
data Sign l
    = Signless l
    | Negative l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Haskell expressions.
data Exp l
    = Var l (QName l)                       -- ^ variable
    | IPVar l (IPName l)                    -- ^ implicit parameter variable
    | Con l (QName l)                       -- ^ data constructor
    | Lit l (Literal l)                     -- ^ literal constant
    | InfixApp l (Exp l) (QOp l) (Exp l)    -- ^ infix application
    | App l (Exp l) (Exp l)                 -- ^ ordinary application
    | NegApp l (Exp l)                      -- ^ negation expression @-/exp/@ (unary minus)
    | Lambda l [Pat l] (Exp l)              -- ^ lambda expression
    | Let l (Binds l) (Exp l)               -- ^ local declarations with @let@ ... @in@ ...
    | If l (Exp l) (Exp l) (Exp l)          -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    | MultiIf l [GuardedRhs l]              -- ^ @if@ @|@ /stmts/ @->@ /exp/ ...
    | Case l (Exp l) [Alt l]                -- ^ @case@ /exp/ @of@ /alts/
    | Do l [Stmt l]                         -- ^ @do@-expression:
                                            --   the last statement in the list
                                            --   should be an expression.
    | MDo l [Stmt l]                        -- ^ @mdo@-expression
    | Tuple l Boxed [Exp l]                 -- ^ tuple expression
    | TupleSection l Boxed [Maybe (Exp l)]  -- ^ tuple section expression, e.g. @(,,3)@
    | List l [Exp l]                        -- ^ list expression
    | ParArray l [Exp l]                    -- ^ parallel array expression
    | Paren l (Exp l)                       -- ^ parenthesised expression
    | LeftSection l (Exp l) (QOp l)         -- ^ left section @(@/exp/ /qop/@)@
    | RightSection l (QOp l) (Exp l)        -- ^ right section @(@/qop/ /exp/@)@
    | RecConstr l (QName l) [FieldUpdate l] -- ^ record construction expression
    | RecUpdate l (Exp l)   [FieldUpdate l] -- ^ record update expression
    | EnumFrom l (Exp l)                    -- ^ unbounded arithmetic sequence,
                                            --   incrementing by 1: @[from ..]@
    | EnumFromTo l (Exp l) (Exp l)          -- ^ bounded arithmetic sequence,
                                            --   incrementing by 1 @[from .. to]@
    | EnumFromThen l (Exp l) (Exp l)        -- ^ unbounded arithmetic sequence,
                                            --   with first two elements given @[from, then ..]@
    | EnumFromThenTo l (Exp l) (Exp l) (Exp l)
                                            -- ^ bounded arithmetic sequence,
                                            --   with first two elements given @[from, then .. to]@
    | ParArrayFromTo l (Exp l) (Exp l)      -- ^ Parallel array bounded arithmetic sequence,
                                            --   incrementing by 1 @[:from .. to:]@
    | ParArrayFromThenTo l (Exp l) (Exp l) (Exp l)
                                            -- ^ bounded arithmetic sequence,
                                            --   with first two elements given @[:from, then .. to:]@
    | ListComp l (Exp l) [QualStmt l]       -- ^ ordinary list comprehension
    | ParComp  l (Exp l) [[QualStmt l]]     -- ^ parallel list comprehension
    | ParArrayComp  l (Exp l) [[QualStmt l]] -- ^ parallel array comprehension
    | ExpTypeSig l (Exp l) (Type l)         -- ^ expression with explicit type signature

    | VarQuote l (QName l)                  -- ^ @'x@ for template haskell reifying of expressions
    | TypQuote l (QName l)                  -- ^ @''T@ for template haskell reifying of types
    | BracketExp l (Bracket l)              -- ^ template haskell bracket expression
    | SpliceExp l (Splice l)                -- ^ template haskell splice expression
    | QuasiQuote l String String            -- ^ quasi-quotaion: @[$/name/| /string/ |]@

-- Hsx
    | XTag l (XName l) [XAttr l] (Maybe (Exp l)) [Exp l]
                                            -- ^ xml element, with attributes and children
    | XETag l (XName l) [XAttr l] (Maybe (Exp l))
                                            -- ^ empty xml element, with attributes
    | XPcdata l String                      -- ^ PCDATA child element
    | XExpTag l (Exp l)                     -- ^ escaped haskell expression inside xml
    | XChildTag l [Exp l]                   -- ^ children of an xml element


-- Pragmas
    | CorePragma l      String (Exp l)      -- ^ CORE pragma
    | SCCPragma  l      String (Exp l)      -- ^ SCC pragma
    | GenPragma  l      String (Int, Int) (Int, Int) (Exp l)
                                            -- ^ GENERATED pragma

-- Arrows
    | Proc            l (Pat l) (Exp l)     -- ^ arrows proc: @proc@ /pat/ @->@ /exp/
    | LeftArrApp      l (Exp l) (Exp l)     -- ^ arrow application (from left): /exp/ @-<@ /exp/
    | RightArrApp     l (Exp l) (Exp l)     -- ^ arrow application (from right): /exp/ @>-@ /exp/
    | LeftArrHighApp  l (Exp l) (Exp l)     -- ^ higher-order arrow application (from left): /exp/ @-<<@ /exp/
    | RightArrHighApp l (Exp l) (Exp l)     -- ^ higher-order arrow application (from right): /exp/ @>>-@ /exp/

-- LambdaCase
    | LCase l [Alt l]                       -- ^ @\case@ /alts/

-- Holes
    | ExprHole l                            -- ^ Expression hole
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | The name of an xml element or attribute,
--   possibly qualified with a namespace.
data XName l
    = XName l String              -- <name ...
    | XDomName l String String    -- <dom:name ...
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An xml attribute, which is a name-expression pair.
data XAttr l = XAttr l (XName l) (Exp l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A template haskell bracket expression.
data Bracket l
    = ExpBracket l (Exp l)        -- ^ expression bracket: @[| ... |]@
    | PatBracket l (Pat l)        -- ^ pattern bracket: @[p| ... |]@
    | TypeBracket l (Type l)      -- ^ type bracket: @[t| ... |]@
    | DeclBracket l [Decl l]      -- ^ declaration bracket: @[d| ... |]@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A template haskell splice expression
data Splice l
    = IdSplice l String           -- ^ variable splice: @$var@
    | ParenSplice l (Exp l)       -- ^ parenthesised expression splice: @$(/exp/)@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | The safety of a foreign function call.
data Safety l
    = PlayRisky l         -- ^ unsafe
    | PlaySafe l Bool     -- ^ safe ('False') or threadsafe ('True')
    | PlayInterruptible l -- ^ interruptible
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | The calling convention of a foreign function call.
data CallConv l
    = StdCall l
    | CCall l
    | CPlusPlus l
    | DotNet l
    | Jvm l
    | Js l
    | JavaScript l
    | CApi l
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A top level options pragma, preceding the module header.
data ModulePragma l
    = LanguagePragma   l [Name l]  -- ^ LANGUAGE pragma
    | OptionsPragma    l (Maybe Tool) String
                        -- ^ OPTIONS pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
    | AnnModulePragma  l (Annotation l)
                        -- ^ ANN pragma with module scope
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Recognised tools for OPTIONS pragmas.
data Tool = GHC | HUGS | NHC98 | YHC | HADDOCK | UnknownTool String
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | Recognised overlaps for overlap pragmas.
data Overlap l
    = NoOverlap l   -- ^ NO_OVERLAP pragma
    | Overlap l     -- ^ OVERLAP pragma
    | Incoherent l  -- ^ INCOHERENT pragma
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Activation clause of a RULES pragma.
data Activation l
    = ActiveFrom   l Int
    | ActiveUntil  l Int
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | The body of a RULES pragma.
data Rule l
    = Rule l String (Maybe (Activation l)) (Maybe [RuleVar l]) (Exp l) (Exp l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Variables used in a RULES pragma, optionally annotated with types
data RuleVar l
    = RuleVar l (Name l)
    | TypedRuleVar l (Name l) (Type l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | Warning text to optionally use in the module header of e.g.
--   a deprecated module.
data WarningText l
    = DeprText l String
    | WarnText l String
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)


-- | A pattern, to be matched against a value.
data Pat l
    = PVar l (Name l)                       -- ^ variable
    | PLit l (Sign l) (Literal l)           -- ^ literal constant
    | PNPlusK l (Name l) Integer            -- ^ n+k pattern
    | PInfixApp l (Pat l) (QName l) (Pat l) -- ^ pattern with an infix data constructor
    | PApp l (QName l) [Pat l]              -- ^ data constructor and argument patterns
    | PTuple l Boxed [Pat l]                -- ^ tuple pattern
    | PList l [Pat l]                       -- ^ list pattern
    | PParen l (Pat l)                      -- ^ parenthesized pattern
    | PRec l (QName l) [PatField l]         -- ^ labelled pattern, record style
    | PAsPat l (Name l) (Pat l)             -- ^ @\@@-pattern
    | PWildCard l                           -- ^ wildcard pattern: @_@
    | PIrrPat l (Pat l)                     -- ^ irrefutable pattern: @~/pat/@
    | PatTypeSig l (Pat l) (Type l)         -- ^ pattern with type signature
    | PViewPat l (Exp l) (Pat l)            -- ^ view patterns of the form @(/exp/ -> /pat/)@
    | PRPat l [RPat l]                      -- ^ regular list pattern
    | PXTag l (XName l) [PXAttr l] (Maybe (Pat l)) [Pat l]
                                            -- ^ XML element pattern
    | PXETag l (XName l) [PXAttr l] (Maybe (Pat l))
                                            -- ^ XML singleton element pattern
    | PXPcdata l String                     -- ^ XML PCDATA pattern
    | PXPatTag l (Pat l)                    -- ^ XML embedded pattern
    | PXRPats  l [RPat l]                   -- ^ XML regular list pattern
    | PQuasiQuote l String String           -- ^ quasi quote pattern: @[$/name/| /string/ |]@
    | PBangPat l (Pat l)                    -- ^ strict (bang) pattern: @f !x = ...@
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An XML attribute in a pattern.
data PXAttr l = PXAttr l (XName l) (Pat l)
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A regular pattern operator.
data RPatOp l
    = RPStar  l  -- ^ @*@ = 0 or more
    | RPStarG l  -- ^ @*!@ = 0 or more, greedy
    | RPPlus  l  -- ^ @+@ = 1 or more
    | RPPlusG l  -- ^ @+!@ = 1 or more, greedy
    | RPOpt   l  -- ^ @?@ = 0 or 1
    | RPOptG  l  -- ^ @?!@ = 0 or 1, greedy
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An entity in a regular pattern.
data RPat l
    = RPOp l (RPat l) (RPatOp l)   -- ^ operator pattern, e.g. pat*
    | RPEither l (RPat l) (RPat l) -- ^ choice pattern, e.g. (1 | 2)
    | RPSeq l [RPat l]             -- ^ sequence pattern, e.g. (| 1, 2, 3 |)
    | RPGuard l (Pat l) [Stmt l]   -- ^ guarded pattern, e.g. (| p | p < 3 |)
    | RPCAs l (Name l) (RPat l)    -- ^ non-linear variable binding, e.g. (foo\@:(1 | 2))*
    | RPAs l (Name l) (RPat l)     -- ^ linear variable binding, e.g. foo\@(1 | 2)
    | RPParen l (RPat l)           -- ^ parenthesised pattern, e.g. (2*)
    | RPPat l (Pat l)              -- ^ an ordinary pattern
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An /fpat/ in a labeled record pattern.
data PatField l
    = PFieldPat l (QName l) (Pat l)     -- ^ ordinary label-pattern pair
    | PFieldPun l (QName l)             -- ^ record field pun
    | PFieldWildcard l                  -- ^ record field wildcard
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A statement, representing both a /stmt/ in a @do@-expression,
--   an ordinary /qual/ in a list comprehension, as well as a /stmt/
--   in a pattern guard.
data Stmt l
    = Generator l (Pat l) (Exp l)
                            -- ^ a generator: /pat/ @<-@ /exp/
    | Qualifier l (Exp l)   -- ^ an /exp/ by itself: in a @do@-expression,
                            --   an action whose result is discarded;
                            --   in a list comprehension and pattern guard,
                            --   a guard expression
    | LetStmt l (Binds l)   -- ^ local bindings
    | RecStmt l [Stmt l]    -- ^ a recursive binding group for arrows
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | A general /transqual/ in a list comprehension,
--   which could potentially be a transform of the kind
--   enabled by TransformListComp.
data QualStmt l
    = QualStmt     l (Stmt l)         -- ^ an ordinary statement
    | ThenTrans    l (Exp l)          -- ^ @then@ /exp/
    | ThenBy       l (Exp l) (Exp l)  -- ^ @then@ /exp/ @by@ /exp/
    | GroupBy      l (Exp l)          -- ^ @then@ @group@ @by@ /exp/
    | GroupUsing   l (Exp l)          -- ^ @then@ @group@ @using@ /exp/
    | GroupByUsing l (Exp l) (Exp l)  -- ^ @then@ @group@ @by@ /exp/ @using@ /exp/
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An /fbind/ in a labeled construction or update expression.
data FieldUpdate l
    = FieldUpdate l (QName l) (Exp l)    -- ^ ordinary label-expresion pair
    | FieldPun l (QName l)               -- ^ record field pun
    | FieldWildcard l                    -- ^ record field wildcard
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-- | An /alt/ alternative in a @case@ expression.
data Alt l
    = Alt l (Pat l) (Rhs l) (Maybe (Binds l))
  deriving (Eq,Ord,Show,Typeable,Data,Foldable,Traversable,Functor,Generic)

-----------------------------------------------------------------------------
-- Builtin names.

prelude_mod, main_mod :: l -> ModuleName l
prelude_mod l = ModuleName l "Prelude"
main_mod    l = ModuleName l "Main"

main_name :: l -> Name l
main_name l = Ident l "main"

unit_con_name :: l -> QName l
unit_con_name l = Special l (UnitCon l)

tuple_con_name :: l -> Boxed -> Int -> QName l
tuple_con_name l b i = Special l (TupleCon l b (i+1))

list_cons_name :: l -> QName l
list_cons_name l = Special l (Cons l)

unboxed_singleton_con_name :: l -> QName l
unboxed_singleton_con_name l = Special l (UnboxedSingleCon l)

unit_con :: l -> Exp l
unit_con l = Con l $ unit_con_name l

tuple_con :: l -> Boxed -> Int -> Exp l
tuple_con l b i = Con l (tuple_con_name l b i)

unboxed_singleton_con :: l -> Exp l
unboxed_singleton_con l = Con l (unboxed_singleton_con_name l)

as_name, qualified_name, hiding_name, minus_name, bang_name, dot_name, star_name :: l -> Name l
as_name        l = Ident  l "as"
qualified_name l = Ident  l "qualified"
hiding_name    l = Ident  l "hiding"
minus_name     l = Symbol l "-"
bang_name      l = Symbol l "!"
dot_name       l = Symbol l "."
star_name      l = Symbol l "*"

export_name, safe_name, unsafe_name, interruptible_name, threadsafe_name,
  stdcall_name, ccall_name, cplusplus_name, dotnet_name,
  jvm_name, js_name, javascript_name, capi_name, forall_name,
  family_name, role_name :: l -> Name l
export_name     l = Ident l "export"
safe_name       l = Ident l "safe"
unsafe_name     l = Ident l "unsafe"
interruptible_name l = Ident l "interruptible"
threadsafe_name l = Ident l "threadsafe"
stdcall_name    l = Ident l "stdcall"
ccall_name      l = Ident l "ccall"
cplusplus_name  l = Ident l "cplusplus"
dotnet_name     l = Ident l "dotnet"
jvm_name        l = Ident l "jvm"
js_name         l = Ident l "js"
javascript_name l = Ident l "javascript"
capi_name       l = Ident l "capi"
forall_name     l = Ident l "forall"
family_name     l = Ident l "family"
role_name       l = Ident l "role"

unit_tycon_name, fun_tycon_name, list_tycon_name, unboxed_singleton_tycon_name :: l -> QName l
unit_tycon_name l = unit_con_name l
fun_tycon_name  l = Special l (FunCon l)
list_tycon_name l = Special l (ListCon l)
unboxed_singleton_tycon_name l = Special l (UnboxedSingleCon l)

tuple_tycon_name :: l -> Boxed -> Int -> QName l
tuple_tycon_name l b i = tuple_con_name l b i

unit_tycon, fun_tycon, list_tycon, unboxed_singleton_tycon :: l -> Type l
unit_tycon l = TyCon l $ unit_tycon_name l
fun_tycon  l = TyCon l $ fun_tycon_name  l
list_tycon l = TyCon l $ list_tycon_name l
unboxed_singleton_tycon l = TyCon l $ unboxed_singleton_tycon_name l

tuple_tycon :: l -> Boxed -> Int -> Type l
tuple_tycon l b i = TyCon l (tuple_tycon_name l b i)

-----------------------------------------------------------------------------
-- AST traversal, boiler-plate style

-- | Test if two AST elements are equal modulo annotations.
(=~=) :: (Annotated a, Eq (a ())) => a l1 -> a l2 -> Bool
a =~= b = fmap (const ()) a == fmap (const ()) b

-----------------------------------------------------------------------------
-- Reading annotations

-- | AST nodes are annotated, and this class allows manipulation of the annotations.
class Functor ast => Annotated ast where
    -- | Retrieve the annotation of an AST node.
    ann :: ast l -> l
    -- | Change the annotation of an AST node. Note that only the annotation of
    --   the node itself is affected, and not the annotations of any child nodes.
    --   if all nodes in the AST tree are to be affected, use 'fmap'.
    amap :: (l -> l) -> ast l -> ast l

instance Annotated ModuleName where
    ann (ModuleName l _) = l
    amap f (ModuleName l n) = ModuleName (f l) n

instance Annotated SpecialCon where
    ann sc = case sc of
        UnitCon l   -> l
        ListCon l   -> l
        FunCon  l   -> l
        TupleCon l _ _  -> l
        Cons l      -> l
        UnboxedSingleCon l  -> l
    amap = fmap

instance Annotated QName where
    ann qn = case qn of
        Qual    l _ _  -> l
        UnQual  l   _  -> l
        Special l _    -> l
    amap f qn = case qn of
        Qual    l mn n  -> Qual    (f l) mn n
        UnQual  l    n  -> UnQual  (f l)    n
        Special l sc    -> Special (f l) sc

instance Annotated Name where
    ann (Ident  l _) = l
    ann (Symbol l _) = l
    amap = fmap

instance Annotated IPName where
    ann (IPDup l _) = l
    ann (IPLin l _) = l
    amap = fmap

instance Annotated QOp where
    ann (QVarOp l _) = l
    ann (QConOp l _) = l
    amap f (QVarOp l qn) = QVarOp (f l) qn
    amap f (QConOp l qn) = QConOp (f l) qn

instance Annotated Op where
    ann (VarOp l _) = l
    ann (ConOp l _) = l
    amap f (VarOp l n) = VarOp (f l) n
    amap f (ConOp l n) = ConOp (f l) n

instance Annotated CName where
    ann (VarName l _) = l
    ann (ConName l _) = l
    amap f (VarName l n) = VarName (f l) n
    amap f (ConName l n) = ConName (f l) n

instance Annotated Module where
    ann (Module l _ _ _ _)            = l
    ann (XmlPage l _ _ _ _ _ _)       = l
    ann (XmlHybrid l _ _ _ _ _ _ _ _) = l

    amap f (Module l mmh ops iss dcls) =
        Module (f l) mmh ops iss dcls
    amap f (XmlPage l mn os xn xas me es) =
        XmlPage (f l) mn os xn xas me es
    amap f (XmlHybrid l mmh ops iss dcls xn xas me es) =
        XmlHybrid (f l) mmh ops iss dcls xn xas me es

instance Annotated ModuleHead where
    ann (ModuleHead l _ _ _)         = l
    amap f (ModuleHead l n mwt mesl) = ModuleHead (f l) n mwt mesl

instance Annotated ExportSpecList where
    ann (ExportSpecList l _)      = l
    amap f (ExportSpecList l ess) = ExportSpecList (f l) ess

instance Annotated ExportSpec where
    ann es = case es of
        EVar l _            -> l
        EAbs l _ _          -> l
        EThingAll l _       -> l
        EThingWith l _ _    -> l
        EModuleContents l _ -> l
    amap f es = case es of
        EVar l qn     -> EVar (f l) qn
        EAbs l n qn       -> EAbs (f l) n qn
        EThingAll l qn  -> EThingAll (f l) qn
        EThingWith l qn cns -> EThingWith (f l) qn cns
        EModuleContents l mn    -> EModuleContents (f l) mn

instance Annotated Namespace where
    ann es = case es of
        NoNamespace l   -> l
        TypeNamespace l -> l
        PatternNamespace l -> l
    amap f es = case es of
        NoNamespace l   -> NoNamespace (f l)
        TypeNamespace l -> TypeNamespace (f l)
        PatternNamespace l -> PatternNamespace (f l)

instance Annotated ImportDecl where
    ann (ImportDecl l _ _ _ _ _ _ _) = l
    amap f (ImportDecl l mn qual src safe pkg mmn mis) =
        ImportDecl (f l) mn qual src safe pkg mmn mis

instance Annotated ImportSpecList where
    ann (ImportSpecList l _ _)      = l
    amap f (ImportSpecList l b iss) = ImportSpecList (f l) b iss

instance Annotated ImportSpec where
    ann is = case is of
        IVar l _         -> l
        IAbs l _ _       -> l
        IThingAll l _    -> l
        IThingWith l _ _ -> l
    amap f is = case is of
        IVar l n        -> IVar (f l) n
        IAbs l ns n     -> IAbs (f l) ns n
        IThingAll l n   -> IThingAll (f l) n
        IThingWith l n cns  -> IThingWith (f l) n cns

instance Annotated Assoc where
    ann (AssocNone  l) = l
    ann (AssocLeft  l) = l
    ann (AssocRight l) = l
    amap = fmap

instance Annotated Deriving where
    ann (Deriving l _)      = l
    amap f (Deriving l ihs) = Deriving (f l) ihs

instance Annotated TypeEqn where
    ann (TypeEqn l _ _) = l
    amap f (TypeEqn l a b) = TypeEqn (f l) a b

instance Annotated Decl where
    ann decl = case decl of
        TypeDecl     l _ _              -> l
        TypeFamDecl  l _ _              -> l
        ClosedTypeFamDecl  l _ _ _      -> l
        DataDecl     l _ _ _ _ _        -> l
        GDataDecl    l _ _ _ _ _ _      -> l
        DataFamDecl  l    _ _ _         -> l
        TypeInsDecl  l _  _             -> l
        DataInsDecl  l _ _ _ _          -> l
        GDataInsDecl l _ _ _ _ _        -> l
        ClassDecl    l _ _ _ _          -> l
        InstDecl     l _ _ _            -> l
        DerivDecl    l _ _              -> l
        InfixDecl    l _ _ _            -> l
        DefaultDecl  l _                -> l
        SpliceDecl   l _                -> l
        TypeSig      l _ _              -> l
        PatSynSig    l _ _ _ _ _        -> l
        FunBind      l _                -> l
        PatBind      l _ _ _            -> l
        ForImp       l _ _ _ _ _        -> l
        ForExp       l _ _ _ _          -> l
        RulePragmaDecl   l _            -> l
        DeprPragmaDecl   l _            -> l
        WarnPragmaDecl   l _            -> l
        InlineSig        l _ _ _        -> l
        InlineConlikeSig l   _ _        -> l
        SpecSig          l   _ _ _      -> l
        SpecInlineSig    l _ _ _ _      -> l
        InstSig          l _            -> l
        AnnPragma        l _            -> l
        MinimalPragma    l _            -> l
        RoleAnnotDecl    l _ _          -> l
        PatSyn           l _ _ _        -> l
    amap f decl = case decl of
        TypeDecl     l dh t      -> TypeDecl    (f l) dh t
        TypeFamDecl  l dh mk     -> TypeFamDecl (f l) dh mk
        ClosedTypeFamDecl  l dh mk eqns  -> ClosedTypeFamDecl (f l) dh mk eqns
        DataDecl     l dn mcx dh cds ders ->
            DataDecl (f l) dn mcx dh cds ders
        GDataDecl    l dn mcx dh mk gds ders ->
            GDataDecl (f l) dn mcx dh mk gds ders
        DataFamDecl  l mcx dh mk         -> DataFamDecl (f l) mcx dh mk
        TypeInsDecl  l t1 t2             -> TypeInsDecl (f l) t1 t2
        DataInsDecl  l dn t cds ders     -> DataInsDecl (f l) dn t cds ders
        GDataInsDecl l dn t mk gds ders  -> GDataInsDecl (f l) dn t mk gds ders
        ClassDecl    l mcx dh fds cds    -> ClassDecl (f l) mcx dh fds cds
        InstDecl     l mo ih ids         -> InstDecl (f l) mo ih ids
        DerivDecl    l mo ih             -> DerivDecl (f l) mo ih
        InfixDecl    l a k ops           -> InfixDecl (f l) a k ops
        DefaultDecl  l ts                -> DefaultDecl (f l) ts
        SpliceDecl   l sp                -> SpliceDecl (f l) sp
        TypeSig      l ns t              -> TypeSig (f l) ns t
        PatSynSig    l n dh c1 c2 t      -> PatSynSig (f l) n dh c1 c2 t
        FunBind      l ms                -> FunBind (f l) ms
        PatBind      l p rhs bs          -> PatBind (f l) p rhs bs
        ForImp       l cc msf s n t      -> ForImp (f l) cc msf s n t
        ForExp       l cc     s n t      -> ForExp (f l) cc     s n t
        RulePragmaDecl   l rs            -> RulePragmaDecl (f l) rs
        DeprPragmaDecl   l nss           -> DeprPragmaDecl (f l) nss
        WarnPragmaDecl   l nss           -> WarnPragmaDecl (f l) nss
        InlineSig        l b act qn      -> InlineSig (f l) b act qn
        InlineConlikeSig l   act qn      -> InlineConlikeSig (f l) act qn
        SpecSig          l   act qn ts   -> SpecSig       (f l)   act qn ts
        SpecInlineSig    l b act qn ts   -> SpecInlineSig (f l) b act qn ts
        InstSig          l ih            -> InstSig (f l) ih
        AnnPragma        l ann'          -> AnnPragma (f l) ann'
        MinimalPragma    l b             -> MinimalPragma (f l) b
        RoleAnnotDecl    l t rs          -> RoleAnnotDecl (f l) t rs
        PatSyn           l p r d         -> PatSyn (f l) p r d

instance Annotated Role where
    ann r = case r of
      RoleWildcard l -> l
      Representational l -> l
      Phantom l -> l
      Nominal l -> l
    amap f r = case r of
      RoleWildcard l -> RoleWildcard (f l)
      Representational l -> Representational (f l)
      Phantom l -> Phantom (f l)
      Nominal l -> Nominal (f l)

instance Annotated Annotation where
    ann (Ann     l _ _) = l
    ann (TypeAnn l _ _) = l
    ann (ModuleAnn l _) = l
    amap f (Ann     l n e) = Ann     (f l) n e
    amap f (TypeAnn l n e) = TypeAnn (f l) n e
    amap f (ModuleAnn l e) = ModuleAnn (f l) e

instance Annotated BooleanFormula where
    ann (VarFormula l _)   = l
    ann (AndFormula l _)   = l
    ann (OrFormula l _)    = l
    ann (ParenFormula l _) = l
    amap f (VarFormula l n)   = VarFormula (f l) n
    amap f (AndFormula l bs)  = AndFormula (f l) bs
    amap f (OrFormula l bs)   = OrFormula (f l) bs
    amap f (ParenFormula l b) = ParenFormula (f l) b

instance Annotated DataOrNew where
    ann (DataType l) = l
    ann (NewType  l) = l
    amap = fmap

instance Annotated DeclHead where
    ann (DHead l _)              = l
    ann (DHInfix l _ _)          = l
    ann (DHParen l _)            = l
    ann (DHApp l _ _)            = l
    amap f (DHead l n)           = DHead (f l) n
    amap f (DHInfix l tva n)     = DHInfix (f l) tva n
    amap f (DHParen l dh)        = DHParen (f l) dh
    amap f (DHApp l dh t)        = DHApp (f l) dh t

instance Annotated InstRule where
    ann (IRule l _ _ _)         = l
    ann (IParen l _)            = l
    amap f (IRule l mtv cxt qn) = IRule (f l) mtv cxt qn
    amap f (IParen l ih)        = IParen (f l) ih

instance Annotated InstHead where
    ann (IHCon l _)              = l
    ann (IHInfix l _ _)          = l
    ann (IHParen l _)            = l
    ann (IHApp l _ _)            = l
    amap f (IHCon l n)           = IHCon (f l) n
    amap f (IHInfix l tva n)     = IHInfix (f l) tva n
    amap f (IHParen l dh)        = IHParen (f l) dh
    amap f (IHApp l dh t)        = IHApp (f l) dh t

instance Annotated Binds where
    ann (BDecls  l _) = l
    ann (IPBinds l _) = l
    amap f (BDecls  l decls) = BDecls (f l) decls
    amap f (IPBinds l ibs)   = IPBinds (f l) ibs

instance Annotated IPBind where
    ann (IPBind l _ _) = l
    amap f (IPBind l ipn e) = IPBind (f l) ipn e

instance Annotated Match where
    ann (Match l _ _ _ _)        = l
    ann (InfixMatch l _ _ _ _ _) = l
    amap f (Match l n ps rhs bs) = Match (f l) n ps rhs bs
    amap f (InfixMatch l a n b rhs bs) = InfixMatch (f l) a n b rhs bs

instance Annotated QualConDecl where
    ann (QualConDecl l _ _ _) = l
    amap f (QualConDecl l tvs cx cd) = QualConDecl (f l) tvs cx cd

instance Annotated ConDecl where
    ann (ConDecl l _ _)        = l
    ann (InfixConDecl l _ _ _) = l
    ann (RecDecl l _ _)        = l
    amap f (ConDecl l n bts) = ConDecl (f l) n bts
    amap f (InfixConDecl l ta n tb) = InfixConDecl (f l) ta n tb
    amap f (RecDecl l n fds) = RecDecl (f l) n fds

instance Annotated FieldDecl where
    ann (FieldDecl l _ _) = l
    amap f (FieldDecl l ns t) = FieldDecl (f l) ns t

instance Annotated GadtDecl where
    ann (GadtDecl l _  _ _) = l
    amap f (GadtDecl l n t1 t2) = GadtDecl (f l) n t1 t2

instance Annotated ClassDecl where
    ann (ClsDecl    l _)      = l
    ann (ClsDataFam l _ _ _)  = l
    ann (ClsTyFam   l    _ _) = l
    ann (ClsTyDef   l _ _)    = l
    ann (ClsDefSig  l _ _)    = l
    amap f (ClsDecl    l d) = ClsDecl (f l) d
    amap f (ClsDataFam l mcx dh mk) = ClsDataFam (f l) mcx dh mk
    amap f (ClsTyFam   l     dh mk) = ClsTyFam   (f l)     dh mk
    amap f (ClsTyDef   l t1 t2) = ClsTyDef (f l) t1 t2
    amap f (ClsDefSig  l n t) = ClsDefSig (f l) n t

instance Annotated InstDecl where
    ann id = case id of
        InsDecl   l _            -> l
        InsType   l _ _          -> l
        InsData   l _ _  _ _     -> l
        InsGData  l _ _ _ _ _    -> l
--        InsInline l _ _ _    -> l
    amap f id = case id of
        InsDecl   l d           -> InsDecl (f l) d
        InsType   l t1 t2       -> InsType (f l) t1 t2
        InsData   l dn t    cds ders -> InsData  (f l) dn t    cds ders
        InsGData  l dn t mk gds ders -> InsGData (f l) dn t mk gds ders
--        InsInline l b act qn    -> InsInline (f l) b act qn

instance Annotated BangType where
     ann (BangedTy   l)    = l
     ann (UnpackedTy l)    = l
     amap f (BangedTy   l) = BangedTy (f l)
     amap f (UnpackedTy l) = UnpackedTy (f l)

instance Annotated Rhs where
     ann (UnGuardedRhs l _) = l
     ann (GuardedRhss  l _) = l
     amap f (UnGuardedRhs l e)     = UnGuardedRhs (f l) e
     amap f (GuardedRhss  l grhss) = GuardedRhss  (f l) grhss

instance Annotated GuardedRhs where
     ann (GuardedRhs l _ _) = l
     amap f (GuardedRhs l ss e) = GuardedRhs (f l) ss e

instance Annotated Type where
    ann t = case t of
      TyForall l _ _ _              -> l
      TyFun   l _ _                 -> l
      TyTuple l _ _                 -> l
      TyList  l _                   -> l
      TyParArray  l _               -> l
      TyApp   l _ _                 -> l
      TyVar   l _                   -> l
      TyCon   l _                   -> l
      TyParen l _                   -> l
      TyInfix l _ _ _               -> l
      TyKind  l _ _                 -> l
      TyPromoted l   _              -> l
      TyEquals l _ _                -> l
      TySplice l _                  -> l
      TyBang l _ _                  -> l
      TyWildCard l _                -> l
      TyQuasiQuote l _ _            -> l
    amap f t1 = case t1 of
      TyForall l mtvs mcx t         -> TyForall (f l) mtvs mcx t
      TyFun   l t1' t2              -> TyFun (f l) t1' t2
      TyTuple l b ts                -> TyTuple (f l) b ts
      TyList  l t                   -> TyList (f l) t
      TyParArray  l t               -> TyParArray (f l) t
      TyApp   l t1' t2              -> TyApp (f l) t1' t2
      TyVar   l n                   -> TyVar (f l) n
      TyCon   l qn                  -> TyCon (f l) qn
      TyParen l t                   -> TyParen (f l) t
      TyInfix l ta qn tb            -> TyInfix (f l) ta qn tb
      TyKind  l t k                 -> TyKind (f l) t k
      TyPromoted l   p              -> TyPromoted (f l)   p
      TyEquals l a b                -> TyEquals (f l) a b
      TySplice l s                  -> TySplice (f l) s
      TyBang l b t                  -> TyBang (f l) b t
      TyWildCard l n                -> TyWildCard (f l) n
      TyQuasiQuote l n s            -> TyQuasiQuote (f l) n s

instance Annotated TyVarBind where
    ann (KindedVar   l _ _) = l
    ann (UnkindedVar l _)   = l
    amap f (KindedVar   l n k) = KindedVar   (f l) n k
    amap f (UnkindedVar l n)   = UnkindedVar (f l) n

instance Annotated Kind where
    ann (KindStar l) = l
    ann (KindFn   l _ _) = l
    ann (KindParen l _)  = l
    ann (KindVar l _)    = l
    ann (KindApp l _ _)  = l
    ann (KindTuple l _)  = l
    ann (KindList  l _)  = l
    amap f (KindStar l) = KindStar (f l)
    amap f (KindFn   l k1 k2) = KindFn (f l) k1 k2
    amap f (KindParen l k) = KindParen (f l) k
    amap f (KindVar l n) = KindVar (f l) n
    amap f (KindApp l k1 k2) = KindApp (f l) k1 k2
    amap f (KindTuple l ks) = KindTuple (f l) ks
    amap f (KindList  l ks) = KindList  (f l) ks

instance Annotated FunDep where
    ann (FunDep l _ _) = l
    amap f (FunDep l ns1 ns2) = FunDep (f l) ns1 ns2

instance Annotated Context where
    ann (CxSingle l _) = l
    ann (CxTuple  l _) = l
    ann (CxEmpty  l)   = l
    amap f (CxSingle l asst ) = CxSingle (f l) asst
    amap f (CxTuple  l assts) = CxTuple  (f l) assts
    amap f (CxEmpty l) = CxEmpty (f l)

instance Annotated Asst where
    ann asst = case asst of
        ClassA l _ _     -> l
        AppA l _ _       -> l
        InfixA l _ _ _   -> l
        IParam l _ _     -> l
        EqualP l _ _     -> l
        ParenA l _       -> l
        WildCardA l _    -> l
    amap f asst = case asst of
        ClassA l qn ts      -> ClassA (f l) qn ts
        AppA   l n ns       -> AppA   (f l) n ns
        InfixA l ta qn tb   -> InfixA (f l) ta qn tb
        IParam l ipn t      -> IParam (f l) ipn t
        EqualP l t1 t2      -> EqualP (f l) t1 t2
        ParenA l a          -> ParenA (f l) a
        WildCardA l mn      -> WildCardA (f l) mn

instance Annotated Literal where
    ann lit = case lit of
        Char    l _    _  -> l
        String  l _    _  -> l
        Int     l _    _  -> l
        Frac    l _    _  -> l
        PrimInt    l _ _  -> l
        PrimWord   l _ _  -> l
        PrimFloat  l _ _  -> l
        PrimDouble l _ _  -> l
        PrimChar   l _ _  -> l
        PrimString l _ _  -> l
    amap = fmap

instance Annotated Sign where
    ann sg = case sg of
        Signless l -> l
        Negative l -> l
    amap = fmap

instance Annotated Exp where
    ann e = case e of
        Var l _                -> l
        IPVar l _              -> l
        Con l _                -> l
        Lit l _                -> l
        InfixApp l _ _ _       -> l
        App l _ _              -> l
        NegApp l _             -> l
        Lambda l _ _           -> l
        Let l _ _              -> l
        If l _ _ _             -> l
        MultiIf l _            -> l
        Case l _ _             -> l
        Do l _                 -> l
        MDo l _                -> l
        Tuple l _ _            -> l
        TupleSection l _ _     -> l
        List l _               -> l
        ParArray l _           -> l
        Paren l _              -> l
        LeftSection l _ _      -> l
        RightSection l _ _     -> l
        RecConstr l _ _        -> l
        RecUpdate l _ _        -> l
        EnumFrom l _           -> l
        EnumFromTo l _ _       -> l
        EnumFromThen l _ _     -> l
        EnumFromThenTo l _ _ _ -> l
        ParArrayFromTo l _ _   -> l
        ParArrayFromThenTo l _ _ _ -> l
        ListComp l _ _         -> l
        ParComp  l _ _         -> l
        ParArrayComp  l _ _    -> l
        ExpTypeSig l _ _       -> l
        VarQuote l _           -> l
        TypQuote l _           -> l
        BracketExp l _         -> l
        SpliceExp l _          -> l
        QuasiQuote l _ _       -> l

        XTag  l _ _ _ _        -> l
        XETag l _ _ _          -> l
        XPcdata l _            -> l
        XExpTag l _            -> l
        XChildTag l _          -> l

        CorePragma l _ _       -> l
        SCCPragma  l _ _       -> l
        GenPragma  l _ _ _ _   -> l

        Proc            l _ _  -> l
        LeftArrApp      l _ _  -> l
        RightArrApp     l _ _  -> l
        LeftArrHighApp  l _ _  -> l
        RightArrHighApp l _ _  -> l

        LCase l _              -> l
        ExprHole l             -> l

    amap f e1 = case e1 of
        Var l qn        -> Var (f l) qn
        IPVar l ipn     -> IPVar (f l) ipn
        Con l qn        -> Con (f l) qn
        Lit l lit       -> Lit (f l) lit
        InfixApp l e1' qop e2    -> InfixApp (f l) e1' qop e2
        App l e1' e2    -> App (f l) e1' e2
        NegApp l e      -> NegApp (f l) e
        Lambda l ps e   -> Lambda (f l) ps e
        Let l bs e      -> Let (f l) bs e
        If l ec et ee   -> If (f l) ec et ee
        Case l e alts   -> Case (f l) e alts
        Do l ss         -> Do (f l) ss
        MDo l ss        -> MDo (f l) ss
        Tuple l bx es   -> Tuple (f l) bx es
        TupleSection l bx mes -> TupleSection (f l) bx mes
        List l es       -> List (f l) es
        ParArray l es   -> ParArray (f l) es
        Paren l e       -> Paren (f l) e
        LeftSection l e qop     -> LeftSection (f l) e qop
        RightSection l qop e    -> RightSection (f l) qop e
        RecConstr l qn fups     -> RecConstr (f l) qn fups
        RecUpdate l e  fups     -> RecUpdate (f l) e  fups
        EnumFrom l e            -> EnumFrom (f l) e
        EnumFromTo l ef et      -> EnumFromTo (f l) ef et
        EnumFromThen l ef et    -> EnumFromThen (f l) ef et
        EnumFromThenTo l ef eth eto -> EnumFromThenTo (f l) ef eth eto
        ParArrayFromTo l ef et  -> ParArrayFromTo (f l) ef et
        ParArrayFromThenTo l ef eth eto -> ParArrayFromThenTo (f l) ef eth eto
        ListComp l e qss        -> ListComp (f l) e qss
        ParComp  l e qsss       -> ParComp  (f l) e qsss
        ParArrayComp  l e qsss  -> ParArrayComp  (f l) e qsss
        ExpTypeSig l e t        -> ExpTypeSig (f l) e t
        VarQuote l qn           -> VarQuote (f l) qn
        TypQuote l qn           -> TypQuote (f l) qn
        BracketExp l br         -> BracketExp (f l) br
        SpliceExp l sp          -> SpliceExp (f l) sp
        QuasiQuote l sn se      -> QuasiQuote (f l) sn se

        XTag  l xn xas me es     -> XTag  (f l) xn xas me es
        XETag l xn xas me        -> XETag (f l) xn xas me
        XPcdata l s              -> XPcdata (f l) s
        XExpTag l e              -> XExpTag (f l) e
        XChildTag l es           -> XChildTag (f l) es

        CorePragma l s e   -> CorePragma (f l) s e
        SCCPragma  l s e   -> SCCPragma (f l) s e
        GenPragma  l s n12 n34 e -> GenPragma (f l) s n12 n34 e

        Proc            l p e  -> Proc (f l) p e
        LeftArrApp      l e1' e2 -> LeftArrApp      (f l) e1' e2
        RightArrApp     l e1' e2 -> RightArrApp     (f l) e1' e2
        LeftArrHighApp  l e1' e2 -> LeftArrHighApp  (f l) e1' e2
        RightArrHighApp l e1' e2 -> RightArrHighApp (f l) e1' e2

        LCase l alts -> LCase (f l) alts
        MultiIf l alts -> MultiIf (f l) alts
        ExprHole l      -> ExprHole (f l)


instance Annotated XName where
    ann (XName l _)      = l
    ann (XDomName l _ _) = l
    amap = fmap

instance Annotated XAttr where
    ann (XAttr l _ _) = l
    amap f (XAttr l xn e) = XAttr (f l) xn e

instance Annotated Bracket where
    ann (ExpBracket l _)  = l
    ann (PatBracket l _)  = l
    ann (TypeBracket l _) = l
    ann (DeclBracket l _) = l
    amap f (ExpBracket l e) = ExpBracket (f l) e
    amap f (PatBracket l p) = PatBracket (f l) p
    amap f (TypeBracket l t) = TypeBracket (f l) t
    amap f (DeclBracket l ds) = DeclBracket (f l) ds

instance Annotated Splice where
    ann (IdSplice l _)    = l
    ann (ParenSplice l _) = l
    amap f (IdSplice l s) = IdSplice (f l) s
    amap f (ParenSplice l e) = ParenSplice (f l) e

instance Annotated Safety where
    ann (PlayRisky l) = l
    ann (PlaySafe l _) = l
    ann (PlayInterruptible l) = l
    amap = fmap

instance Annotated CallConv where
    ann (StdCall l) = l
    ann (CCall l) = l
    ann (CPlusPlus l) = l
    ann (DotNet l) = l
    ann (Jvm l) = l
    ann (Js l) = l
    ann (JavaScript l) = l
    ann (CApi l) = l
    amap = fmap

instance Annotated ModulePragma where
    ann (LanguagePragma   l _)   = l
    ann (OptionsPragma    l _ _) = l
    ann (AnnModulePragma  l _)   = l
    amap f (LanguagePragma   l ns) = LanguagePragma (f l) ns
    amap f (AnnModulePragma  l a) = AnnModulePragma (f l) a
    amap f p = fmap f p

instance Annotated Overlap where
    ann (NoOverlap l)  = l
    ann (Overlap l)    = l
    ann (Incoherent l) = l
    amap = fmap

instance Annotated Activation where
    ann (ActiveFrom   l _) = l
    ann (ActiveUntil  l _) = l
    amap = fmap

instance Annotated Rule where
    ann (Rule l _ _ _ _ _) = l
    amap f (Rule l s act mrvs e1 e2) = Rule (f l) s act mrvs e1 e2

instance Annotated RuleVar where
    ann (RuleVar l _)        = l
    ann (TypedRuleVar l _ _) = l
    amap f (RuleVar l n) = RuleVar (f l) n
    amap f (TypedRuleVar l n t) = TypedRuleVar (f l) n t

instance Annotated WarningText where
    ann (DeprText l _) = l
    ann (WarnText l _) = l
    amap = fmap

instance Annotated Pat where
    ann p = case p of
      PVar l _          -> l
      PLit l _ _        -> l
      PNPlusK l _ _     -> l
      PInfixApp l _ _ _ -> l
      PApp l _ _        -> l
      PTuple l _ _      -> l
      PList l _         -> l
      PParen l _        -> l
      PRec l _ _        -> l
      PAsPat l _ _      -> l
      PWildCard l       -> l
      PIrrPat l _       -> l
      PatTypeSig l _ _  -> l
      PViewPat l _ _    -> l
      PRPat l _         -> l
      PXTag l _ _ _ _   -> l
      PXETag l _ _ _    -> l
      PXPcdata l _      -> l
      PXPatTag l _      -> l
      PXRPats  l _      -> l
      PQuasiQuote l _ _ -> l
      PBangPat l _      -> l
    amap f p1 = case p1 of
      PVar l n          -> PVar (f l) n
      PLit l sg lit     -> PLit (f l) sg lit
      PNPlusK l n k     -> PNPlusK (f l) n k
      PInfixApp l pa qn pb  -> PInfixApp (f l) pa qn pb
      PApp l qn ps      -> PApp (f l) qn ps
      PTuple l bx ps    -> PTuple (f l) bx ps
      PList l ps        -> PList (f l) ps
      PParen l p        -> PParen (f l) p
      PRec l qn pfs     -> PRec (f l) qn pfs
      PAsPat l n p      -> PAsPat (f l) n p
      PWildCard l       -> PWildCard (f l)
      PIrrPat l p       -> PIrrPat (f l) p
      PatTypeSig l p t  -> PatTypeSig (f l) p t
      PViewPat l e p    -> PViewPat (f l) e p
      PRPat l rps       -> PRPat (f l) rps
      PXTag l xn pxas mp ps -> PXTag  (f l) xn pxas mp ps
      PXETag l xn pxas mp   -> PXETag (f l) xn pxas mp
      PXPcdata l s      -> PXPcdata (f l) s
      PXPatTag l p      -> PXPatTag (f l) p
      PXRPats  l rps    -> PXRPats  (f l) rps
      PQuasiQuote l sn st   -> PQuasiQuote (f l) sn st
      PBangPat l p          -> PBangPat (f l) p

instance Annotated PXAttr where
    ann (PXAttr l _ _) = l
    amap f (PXAttr l xn p) = PXAttr (f l) xn p

instance Annotated RPatOp where
    ann (RPStar  l) = l
    ann (RPStarG l) = l
    ann (RPPlus  l) = l
    ann (RPPlusG l) = l
    ann (RPOpt   l) = l
    ann (RPOptG  l) = l
    amap = fmap

instance Annotated RPat where
    ann rp = case rp of
      RPOp l _ _            -> l
      RPEither l _ _        -> l
      RPSeq l _             -> l
      RPGuard l _ _         -> l
      RPCAs l _ _           -> l
      RPAs l _ _            -> l
      RPParen l _           -> l
      RPPat l _             -> l
    amap f rp1 = case rp1 of
      RPOp l rp rop         -> RPOp (f l) rp rop
      RPEither l rp1' rp2   -> RPEither (f l) rp1' rp2
      RPSeq l rps           -> RPSeq (f l) rps
      RPGuard l p ss        -> RPGuard (f l) p ss
      RPCAs l n rp          -> RPCAs (f l) n rp
      RPAs l n rp           -> RPAs (f l) n rp
      RPParen l rp          -> RPParen (f l) rp
      RPPat l p             -> RPPat (f l) p

instance Annotated PatField where
    ann (PFieldPat l _ _)  = l
    ann (PFieldPun l _)    = l
    ann (PFieldWildcard l) = l
    amap f (PFieldPat l qn p) = PFieldPat (f l) qn p
    amap f (PFieldPun l n) = PFieldPun (f l) n
    amap f (PFieldWildcard l) = PFieldWildcard (f l)

instance Annotated Stmt where
    ann (Generator l _ _) = l
    ann (Qualifier l _)   = l
    ann (LetStmt l _)     = l
    ann (RecStmt l _)     = l
    amap f (Generator l p e) = Generator (f l) p e
    amap f (Qualifier l e)   = Qualifier (f l) e
    amap f (LetStmt l bs)    = LetStmt (f l) bs
    amap f (RecStmt l ss)    = RecStmt (f l) ss

instance Annotated QualStmt where
    ann (QualStmt     l _)   = l
    ann (ThenTrans    l _)   = l
    ann (ThenBy       l _ _) = l
    ann (GroupBy      l _)   = l
    ann (GroupUsing   l _)   = l
    ann (GroupByUsing l _ _) = l
    amap f (QualStmt     l s) = QualStmt (f l) s
    amap f (ThenTrans    l e) = ThenTrans (f l) e
    amap f (ThenBy       l e1 e2) = ThenBy (f l) e1 e2
    amap f (GroupBy      l e) = GroupBy (f l) e
    amap f (GroupUsing   l e) = GroupUsing (f l) e
    amap f (GroupByUsing l e1 e2) = GroupByUsing (f l) e1 e2

instance Annotated FieldUpdate where
    ann (FieldUpdate l _ _)  = l
    ann (FieldPun l _)       = l
    ann (FieldWildcard l)    = l
    amap f (FieldUpdate l qn e) = FieldUpdate (f l) qn e
    amap f (FieldPun l n)       = FieldPun (f l) n
    amap f (FieldWildcard l)    = FieldWildcard (f l)

instance Annotated Alt where
    ann (Alt l _ _ _) = l
    amap f (Alt l p gs bs) = Alt (f l) p gs bs

instance Annotated Promoted where
    ann (PromotedInteger l _ _) = l
    ann (PromotedString l _ _)  = l
    ann (PromotedCon l _ _)     = l
    ann (PromotedList l _ _)    = l
    ann (PromotedTuple l _)     = l
    ann (PromotedUnit l)        = l
    amap f (PromotedInteger l int raw) = PromotedInteger (f l) int raw
    amap f (PromotedString l str raw) = PromotedString (f l) str raw
    amap f (PromotedCon l b qn)   = PromotedCon (f l) b qn
    amap f (PromotedList l b ps)  = PromotedList  (f l) b ps
    amap f (PromotedTuple l ps) = PromotedTuple (f l) ps
    amap f (PromotedUnit l)     = PromotedUnit (f l)

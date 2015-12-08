{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Syntax
-- Copyright   :  (c) Niklas Broberg 2004-2009,
--                (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- A suite of datatypes describing the abstract syntax of Haskell 98
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
-----------------------------------------------------------------------------

module Language.Haskell.Exts.Syntax (
    -- * Modules
    Module(..), WarningText(..), ExportSpec(..),
    ImportDecl(..), ImportSpec(..), Assoc(..), Namespace(..),
    -- * Declarations
    Decl(..), Binds(..), IPBind(..), PatternSynDirection(..),
    -- ** Type classes and instances
    ClassDecl(..), InstDecl(..), Deriving,
    -- ** Data type declarations
    DataOrNew(..), ConDecl(..), QualConDecl(..), GadtDecl(..), BangType(..),
    -- ** Function bindings
    Match(..), Rhs(..), GuardedRhs(..),
    -- * Class Assertions and Contexts
    Context, FunDep(..), Asst(..),
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
    SrcLoc(..),
  ) where


import Data.Data
import GHC.Generics (Generic)
import Language.Haskell.Exts.SrcLoc (SrcLoc(..))
import Language.Haskell.Exts.Annotated.Syntax (Boxed(..), Tool(..))


-- | The name of a Haskell module.
newtype ModuleName = ModuleName String
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | Constructors with special syntax.
-- These names are never qualified, and always refer to builtin type or
-- data constructors.
data SpecialCon
    = UnitCon               -- ^ unit type and data constructor @()@
    | ListCon               -- ^ list type constructor @[]@
    | FunCon                -- ^ function type constructor @->@
    | TupleCon Boxed Int    -- ^ /n/-ary tuple type and data
                            --   constructors @(,)@ etc, possibly boxed @(\#,\#)@
    | Cons                  -- ^ list data constructor @(:)@
    | UnboxedSingleCon      -- ^ unboxed singleton tuple constructor @(\# \#)@
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | This type is used to represent qualified variables, and also
--   qualified constructors.
data QName
    = Qual ModuleName Name    -- ^ name qualified with a module name
    | UnQual Name             -- ^ unqualified local name
    | Special SpecialCon      -- ^ built-in constructor with special syntax
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | This type is used to represent variables, and also constructors.
data Name
    = Ident String    -- ^ /varid/ or /conid/.
    | Symbol String   -- ^ /varsym/ or /consym/
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | An implicit parameter name.
data IPName
    = IPDup String -- ^ ?/ident/, non-linear implicit parameter
    | IPLin String -- ^ %/ident/, linear implicit parameter
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | Possibly qualified infix operators (/qop/), appearing in expressions.
data QOp
    = QVarOp QName  -- ^ variable operator (/qvarop/)
    | QConOp QName  -- ^ constructor operator (/qconop/)
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | Operators appearing in @infix@ declarations are never qualified.
data Op
    = VarOp Name    -- ^ variable operator (/varop/)
    | ConOp Name    -- ^ constructor operator (/conop/)
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A name (/cname/) of a component of a class or data type in an @import@
--   or export specification.
data CName
    = VarName Name  -- ^ name of a method or field
    | ConName Name  -- ^ name of a data constructor
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A complete Haskell source module.
data Module = Module SrcLoc ModuleName [ModulePragma] (Maybe WarningText)
                        (Maybe [ExportSpec]) [ImportDecl] [Decl]
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | An item in a module's export specification.
data ExportSpec
     = EVar QName                   -- ^ variable.
     | EAbs Namespace QName         -- ^ @T@:
                                    --   a class or datatype exported abstractly,
                                    --   or a type synonym.
     | EThingAll QName              -- ^ @T(..)@:
                                    --   a class exported with all of its methods, or
                                    --   a datatype exported with all of its constructors.
     | EThingWith QName [CName]     -- ^ @T(C_1,...,C_n)@:
                                    --   a class exported with some of its methods, or
                                    --   a datatype exported with some of its constructors.
     | EModuleContents ModuleName   -- ^ @module M@:
                                    --   re-export a module.
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | Namespaces for imports/exports.
data Namespace = NoNamespace | TypeNamespace | PatternNamespace
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | An import declaration.
data ImportDecl = ImportDecl
    { importLoc :: SrcLoc           -- ^ position of the @import@ keyword.
    , importModule :: ModuleName    -- ^ name of the module imported.
    , importQualified :: Bool       -- ^ imported @qualified@?
    , importSrc :: Bool             -- ^ imported with @{-\# SOURCE \#-}@?
    , importSafe :: Bool            -- ^ Import @safe@?
    , importPkg :: Maybe String     -- ^ imported with explicit package name
    , importAs :: Maybe ModuleName  -- ^ optional alias name in an @as@ clause.
    , importSpecs :: Maybe (Bool,[ImportSpec])
            -- ^ optional list of import specifications.
            -- The 'Bool' is 'True' if the names are excluded
            -- by @hiding@.
    }
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | An import specification, representing a single explicit item imported
--   (or hidden) from a module.
data ImportSpec
     = IVar Name                -- ^ variable.
     | IAbs Namespace Name      -- ^ @T@:
                                --   the name of a class, datatype or type synonym.
     | IThingAll Name           -- ^ @T(..)@:
                                --   a class imported with all of its methods, or
                                --   a datatype imported with all of its constructors.
     | IThingWith Name [CName]  -- ^ @T(C_1,...,C_n)@:
                                --   a class imported with some of its methods, or
                                --   a datatype imported with some of its constructors.
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | Associativity of an operator.
data Assoc
     = AssocNone  -- ^ non-associative operator (declared with @infix@)
     | AssocLeft  -- ^ left-associative operator (declared with @infixl@).
     | AssocRight -- ^ right-associative operator (declared with @infixr@)
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A single derived instance, which may have arguments since it may be a MPTC.
type Deriving = (QName, [Type])

-- | A top-level declaration.
data Decl
     = TypeDecl     SrcLoc Name [TyVarBind] Type
     -- ^ A type declaration
     | TypeFamDecl  SrcLoc Name [TyVarBind] (Maybe Kind)
     -- ^ A type family declaration
     | ClosedTypeFamDecl  SrcLoc Name [TyVarBind] (Maybe Kind) [TypeEqn]
     -- ^ A closed type family declaration
     | DataDecl     SrcLoc DataOrNew Context Name [TyVarBind]              [QualConDecl] [Deriving]
     -- ^ A data OR newtype declaration
     | GDataDecl    SrcLoc DataOrNew Context Name [TyVarBind] (Maybe Kind) [GadtDecl]    [Deriving]
     -- ^ A data OR newtype declaration, GADT style
     | DataFamDecl  SrcLoc {-data-}  Context Name [TyVarBind] (Maybe Kind)
     -- ^ A data family declaration
     | TypeInsDecl  SrcLoc Type Type
     -- ^ A type family instance declaration
     | DataInsDecl  SrcLoc DataOrNew Type              [QualConDecl] [Deriving]
     -- ^ A data family instance declaration
     | GDataInsDecl SrcLoc DataOrNew Type (Maybe Kind) [GadtDecl]    [Deriving]
     -- ^ A data family instance declaration, GADT style
     | ClassDecl    SrcLoc Context Name [TyVarBind] [FunDep] [ClassDecl]
     -- ^ A declaration of a type class
     | InstDecl     SrcLoc (Maybe Overlap) [TyVarBind] Context QName [Type] [InstDecl]
     -- ^ An declaration of a type class instance
     | DerivDecl    SrcLoc (Maybe Overlap) [TyVarBind] Context QName [Type]
     -- ^ A standalone deriving declaration
     | InfixDecl    SrcLoc Assoc Int [Op]
     -- ^ A declaration of operator fixity
     | DefaultDecl  SrcLoc [Type]
     -- ^ A declaration of default types
     | SpliceDecl   SrcLoc Exp
     -- ^ A Template Haskell splicing declaration
     | TypeSig      SrcLoc [Name] Type
     -- ^ A type signature declaration
     | PatSynSig    SrcLoc Name (Maybe [TyVarBind]) Context Context Type
     -- ^ Pattern Synonym Signature
     | FunBind      [Match]
     -- ^ A set of function binding clauses
     | PatBind      SrcLoc Pat Rhs {-where-} (Maybe Binds)
     -- ^ A pattern binding
     | ForImp   SrcLoc CallConv Safety String Name Type
     -- ^ A foreign import declaration
     | ForExp   SrcLoc CallConv          String Name Type
     -- ^ A foreign export declaration
     | PatSyn   SrcLoc Pat Pat PatternSynDirection

     | RulePragmaDecl   SrcLoc [Rule]
     -- ^ A RULES pragma
     | DeprPragmaDecl   SrcLoc [([Name], String)]
     -- ^ A DEPRECATED pragma
     | WarnPragmaDecl   SrcLoc [([Name], String)]
     -- ^ A WARNING pragma
     | InlineSig        SrcLoc Bool Activation QName
     -- ^ An INLINE pragma
     | InlineConlikeSig SrcLoc      Activation QName
     -- ^ An INLINE CONLIKE pragma
     | SpecSig          SrcLoc      Activation QName [Type]
     -- ^ A SPECIALISE pragma
     | SpecInlineSig    SrcLoc Bool Activation QName [Type]
     -- ^ A SPECIALISE INLINE pragma
     | InstSig          SrcLoc [TyVarBind] Context QName [Type]
     -- ^ A SPECIALISE instance pragma
     | AnnPragma        SrcLoc Annotation
     -- ^ An ANN pragma
     | MinimalPragma    SrcLoc (Maybe BooleanFormula)
     -- ^ A MINIMAL pragma
     | RoleAnnotDecl    SrcLoc QName [Role]
     -- ^ A role annotation

  deriving (Eq,Ord,Show,Typeable,Data,Generic)

data  PatternSynDirection =
      Unidirectional -- ^ A unidirectional pattern synonym with "<-"
    | ImplicitBidirectional  -- ^ A bidirectional pattern synonym with "="
    | ExplicitBidirectional [Decl]  -- ^ A birectional pattern synonym with the construction specified.
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A type equation of the form @rhs = lhs@ used in closed type families.
data TypeEqn = TypeEqn Type Type deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | An annotation through an ANN pragma.
data Annotation
    = Ann       Name Exp
    -- ^ An annotation for a declared name.
    | TypeAnn   Name Exp
    -- ^ An annotation for a declared type.
    | ModuleAnn      Exp
    -- ^ An annotation for the defining module.
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A boolean formula for MINIMAL pragmas.
data BooleanFormula
    = VarFormula Name                -- ^ A variable.
    | AndFormula [BooleanFormula]    -- ^ And boolean formulas.
    | OrFormula [BooleanFormula]     -- ^ Or boolean formulas.
    | ParenFormula BooleanFormula    -- ^ Parenthesized boolean formulas.
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

data Role
  = Nominal
  | Representational
  | Phantom
  | RoleWildcard
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A flag stating whether a declaration is a data or newtype declaration.
data DataOrNew = DataType | NewType
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A binding group inside a @let@ or @where@ clause.
data Binds
    = BDecls [Decl]     -- ^ An ordinary binding group
    | IPBinds [IPBind]  -- ^ A binding group for implicit parameters
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A binding of an implicit parameter.
data IPBind = IPBind SrcLoc IPName Exp
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | Clauses of a function binding.
data Match
     = Match SrcLoc Name [Pat] (Maybe Type) Rhs {-where-} (Maybe Binds)
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A single constructor declaration within a data type declaration,
--   which may have an existential quantification binding.
data QualConDecl
    = QualConDecl SrcLoc
        {-forall-} [TyVarBind] {- . -} Context
        {- => -} ConDecl
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | Declaration of an ordinary data constructor.
data ConDecl
     = ConDecl Name [Type]
                -- ^ ordinary data constructor
     | InfixConDecl Type Name Type
                -- ^ infix data constructor
     | RecDecl Name [([Name],Type)]
                -- ^ record constructor
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A single constructor declaration in a GADT data type declaration.
data GadtDecl
    = GadtDecl SrcLoc Name [([Name], Type)] Type
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | Declarations inside a class declaration.
data ClassDecl
    = ClsDecl    Decl
            -- ^ ordinary declaration
    | ClsDataFam SrcLoc Context Name [TyVarBind] (Maybe Kind)
            -- ^ declaration of an associated data type
    | ClsTyFam   SrcLoc         Name [TyVarBind] (Maybe Kind)
            -- ^ declaration of an associated type synonym
    | ClsTyDef   SrcLoc Type    Type
            -- ^ default choice for an associated type synonym
    | ClsDefSig  SrcLoc Name Type
            -- ^ default signature
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | Declarations inside an instance declaration.
data InstDecl
    = InsDecl   Decl
            -- ^ ordinary declaration
    | InsType   SrcLoc Type Type
            -- ^ an associated type definition
    | InsData   SrcLoc DataOrNew Type [QualConDecl] [Deriving]
            -- ^ an associated data type implementation
    | InsGData  SrcLoc DataOrNew Type (Maybe Kind) [GadtDecl] [Deriving]
            -- ^ an associated data type implemented using GADT style
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | The type of a constructor argument or field, optionally including
--   a strictness annotation.
data BangType
     = BangedTy    -- ^ strict component, marked with \"@!@\"
     | UnpackedTy  -- ^ unboxed component, marked with an UNPACK pragma
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | The right hand side of a function binding, pattern binding, or a case
--   alternative.
data Rhs
     = UnGuardedRhs Exp -- ^ unguarded right hand side (/exp/)
     | GuardedRhss  [GuardedRhs]
                        -- ^ guarded right hand side (/gdrhs/)
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A guarded right hand side @|@ /stmts/ @=@ /exp/, or @|@ /stmts/ @->@ /exp/
--   for case alternatives.
--   The guard is a series of statements when using pattern guards,
--   otherwise it will be a single qualifier expression.
data GuardedRhs
     = GuardedRhs SrcLoc [Stmt] Exp
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A type qualified with a context.
--   An unqualified type has an empty context.
data Type
     = TyForall
        (Maybe [TyVarBind])
        Context
        Type                    -- ^ qualified type
     | TyFun   Type Type        -- ^ function type
     | TyTuple Boxed [Type]     -- ^ tuple type, possibly boxed
     | TyList  Type             -- ^ list syntax, e.g. [a], as opposed to [] a
     | TyParArray Type          -- ^ parallel array syntax, e.g. [:a:]
     | TyApp   Type Type        -- ^ application of a type constructor
     | TyVar   Name             -- ^ type variable
     | TyCon   QName            -- ^ named type or type constructor
     | TyParen Type             -- ^ type surrounded by parentheses
     | TyInfix Type QName Type  -- ^ infix type constructor
     | TyKind  Type Kind        -- ^ type with explicit kind signature
     | TyPromoted Promoted      -- ^ promoted data type (-XDataKinds)
     | TyEquals Type Type       -- ^ type equality predicate enabled by ConstraintKinds
     | TySplice Splice          -- ^ template haskell splice type
     | TyBang BangType Type     -- ^ Strict type marked with \"@!@\" or type marked with UNPACK pragma.
     | TyWildCard (Maybe Name)  -- ^ Type wildcard
     | TyQuasiQuote String String -- ^ quasi quote @[qq|  |]@
  deriving (Eq,Ord,Show,Typeable,Data)

data Promoted
        = PromotedInteger Integer
        | PromotedString String
        | PromotedCon Bool QName
        | PromotedList Bool [Type]
        | PromotedTuple [Type]
        | PromotedUnit
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A type variable declaration, optionally with an explicit kind annotation.
data TyVarBind
    = KindedVar Name Kind   -- ^ variable binding with kind annotation
    | UnkindedVar Name      -- ^ ordinary variable binding
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | An explicit kind annotation.
data Kind
    = KindStar          -- ^ @*@, the kind of types
    | KindFn Kind Kind  -- ^ @->@, the kind of a type constructor
    | KindParen Kind    -- ^ a kind surrounded by parentheses
    | KindVar QName     -- ^ a kind variable (as of yet unsupported by compilers)
    | KindApp Kind Kind -- ^ @k1 k2@
    | KindTuple [Kind]  -- ^ @(k1,k2,k3)@, kind of a promoted tuple
    | KindList Kind     -- ^ @[k1]@, kind of a promoted list
  deriving (Eq,Ord,Show,Typeable,Data,Generic)


-- | A functional dependency, given on the form
--   l1 l2 ... ln -> r2 r3 .. rn
data FunDep
    = FunDep [Name] [Name]
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A context is a set of assertions
type Context = [Asst]

-- | Class assertions.
--   In Haskell 98, the argument would be a /tyvar/, but this definition
--   allows multiple parameters, and allows them to be /type/s.
--   Also extended with support for implicit parameters and equality constraints.
data Asst = ClassA QName [Type]     -- ^ ordinary class assertion
          | AppA Name [Type]        -- ^ constraint kind assertion, @Dict :: cxt a => Dict cxt@
          | InfixA Type QName Type  -- ^ class assertion where the class name is given infix
          | IParam IPName Type      -- ^ implicit parameter assertion
          | EqualP Type   Type      -- ^ type equality constraint
          | ParenA Asst             -- ^ parenthesised class assertion
          | WildCardA (Maybe Name) -- ^ A wildcard
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | /literal/
-- Values of this type hold the abstract value of the literal, not the
-- precise string representation used.  For example, @10@, @0o12@ and @0xa@
-- have the same representation.
data Literal
    = Char    Char          -- ^ character literal
    | String  String        -- ^ string literal
    | Int     Integer       -- ^ integer literal
    | Frac    Rational      -- ^ floating point literal
    | PrimInt    Integer    -- ^ unboxed integer literal
    | PrimWord   Integer    -- ^ unboxed word literal
    | PrimFloat  Rational   -- ^ unboxed float literal
    | PrimDouble Rational   -- ^ unboxed double literal
    | PrimChar   Char       -- ^ unboxed character literal
    | PrimString String     -- ^ unboxed string literal
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | An indication whether a literal pattern has been negated or not.
data Sign
    = Signless
    | Negative
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | Haskell expressions.
data Exp
    = Var QName                 -- ^ variable
    | IPVar IPName              -- ^ implicit parameter variable
    | Con QName                 -- ^ data constructor
    | Lit Literal               -- ^ literal constant
    | InfixApp Exp QOp Exp      -- ^ infix application
    | App Exp Exp               -- ^ ordinary application
    | NegApp Exp                -- ^ negation expression @-/exp/@ (unary minus)
    | Lambda SrcLoc [Pat] Exp   -- ^ lambda expression
    | Let Binds Exp             -- ^ local declarations with @let@ ... @in@ ...
    | If Exp Exp Exp            -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    | MultiIf [GuardedRhs]      -- ^ @if@ @|@ /exp/ @->@ /exp/ ...
    | Case Exp [Alt]            -- ^ @case@ /exp/ @of@ /alts/
    | Do [Stmt]                 -- ^ @do@-expression:
                                --   the last statement in the list
                                --   should be an expression.
    | MDo [Stmt]                -- ^ @mdo@-expression
    | Tuple Boxed [Exp]         -- ^ tuple expression
    | TupleSection Boxed [Maybe Exp]  -- ^ tuple section expression, e.g. @(,,3)@
    | List [Exp]                -- ^ list expression
    | ParArray [Exp]            -- ^ parallel array expression
    | Paren Exp                 -- ^ parenthesised expression
    | LeftSection Exp QOp       -- ^ left section @(@/exp/ /qop/@)@
    | RightSection QOp Exp      -- ^ right section @(@/qop/ /exp/@)@
    | RecConstr QName [FieldUpdate]
                                -- ^ record construction expression
    | RecUpdate Exp [FieldUpdate]
                                -- ^ record update expression
    | EnumFrom Exp              -- ^ unbounded arithmetic sequence,
                                --   incrementing by 1: @[from ..]@
    | EnumFromTo Exp Exp        -- ^ bounded arithmetic sequence,
                                --   incrementing by 1 @[from .. to]@
    | EnumFromThen Exp Exp      -- ^ unbounded arithmetic sequence,
                                --   with first two elements given @[from, then ..]@
    | EnumFromThenTo Exp Exp Exp
                                -- ^ bounded arithmetic sequence,
                                --   with first two elements given @[from, then .. to]@
    | ParArrayFromTo Exp Exp    -- ^ bounded arithmetic sequence,
                                --   incrementing by 1 @[from .. to]@
    | ParArrayFromThenTo Exp Exp Exp
                                -- ^ bounded arithmetic sequence,
                                --   with first two elements given @[from, then .. to]@
    | ListComp Exp  [QualStmt]    -- ^ ordinary list comprehension
    | ParComp  Exp [[QualStmt]]   -- ^ parallel list comprehension
    | ParArrayComp  Exp [[QualStmt]]
                                  -- ^ parallel array comprehension
    | ExpTypeSig SrcLoc Exp Type  -- ^ expression with explicit type signature

    | VarQuote QName            -- ^ @'x@ for template haskell reifying of expressions
    | TypQuote QName            -- ^ @''T@ for template haskell reifying of types
    | BracketExp Bracket        -- ^ template haskell bracket expression
    | SpliceExp Splice          -- ^ template haskell splice expression
    | QuasiQuote String String  -- ^ quasi-quotaion: @[$/name/| /string/ |]@

-- Hsx
    | XTag SrcLoc XName [XAttr] (Maybe Exp) [Exp]
                                -- ^ xml element, with attributes and children
    | XETag SrcLoc XName [XAttr] (Maybe Exp)
                                -- ^ empty xml element, with attributes
    | XPcdata String            -- ^ PCDATA child element
    | XExpTag Exp               -- ^ escaped haskell expression inside xml
    | XChildTag SrcLoc [Exp]    -- ^ children of an xml element

-- Pragmas
    | CorePragma        String Exp      -- ^ CORE pragma
    | SCCPragma         String Exp      -- ^ SCC pragma
    | GenPragma         String (Int, Int) (Int, Int) Exp
                                        -- ^ GENERATED pragma

-- Arrows
    | Proc SrcLoc     Pat Exp   -- ^ arrows proc: @proc@ /pat/ @->@ /exp/
    | LeftArrApp      Exp Exp   -- ^ arrow application (from left): /exp/ @-<@ /exp/
    | RightArrApp     Exp Exp   -- ^ arrow application (from right): /exp/ @>-@ /exp/
    | LeftArrHighApp  Exp Exp   -- ^ higher-order arrow application (from left): /exp/ @-<<@ /exp/
    | RightArrHighApp Exp Exp   -- ^ higher-order arrow application (from right): /exp/ @>>-@ /exp/

-- LambdaCase
    | LCase [Alt] -- ^ @\case@ /alts/

-- Holes
    | ExprHole                  -- ^ Expression hole
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | The name of an xml element or attribute,
--   possibly qualified with a namespace.
data XName
    = XName String              -- <name ...
    | XDomName String String    -- <dom:name ...
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | An xml attribute, which is a name-expression pair.
data XAttr = XAttr XName Exp
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A template haskell bracket expression.
data Bracket
    = ExpBracket Exp        -- ^ expression bracket: @[| ... |]@
    | PatBracket Pat        -- ^ pattern bracket: @[p| ... |]@
    | TypeBracket Type      -- ^ type bracket: @[t| ... |]@
    | DeclBracket [Decl]    -- ^ declaration bracket: @[d| ... |]@
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A template haskell splice expression
data Splice
    = IdSplice String       -- ^ variable splice: @$var@
    | ParenSplice Exp       -- ^ parenthesised expression splice: @$(/exp/)@
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | The safety of a foreign function call.
data Safety
    = PlayRisky         -- ^ unsafe
    | PlaySafe Bool     -- ^ safe ('False') or threadsafe ('True')
    | PlayInterruptible -- ^ interruptible
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | The calling convention of a foreign function call.
data CallConv
    = StdCall
    | CCall
    | CPlusPlus
    | DotNet
    | Jvm
    | Js
    | JavaScript
    | CApi
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A top level options pragma, preceding the module header.
data ModulePragma
    = LanguagePragma   SrcLoc [Name]    -- ^ LANGUAGE pragma
    | OptionsPragma    SrcLoc (Maybe Tool) String
                        -- ^ OPTIONS pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
    | AnnModulePragma  SrcLoc Annotation
                        -- ^ ANN pragma with module scope
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | Recognised overlaps for overlap pragmas.
data Overlap
    = NoOverlap   -- ^ NO_OVERLAP pragma
    | Overlap     -- ^ OVERLAP pragma
    | Incoherent  -- ^ INCOHERENT pragma
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | Activation clause of a RULES pragma.
data Activation
    = AlwaysActive
    | ActiveFrom  Int
    | ActiveUntil Int
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | The body of a RULES pragma.
data Rule
    = Rule String Activation (Maybe [RuleVar]) Exp Exp
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | Variables used in a RULES pragma, optionally annotated with types
data RuleVar
    = RuleVar Name
    | TypedRuleVar Name Type
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | Warning text to optionally use in the module header of e.g.
--   a deprecated module.
data WarningText
    = DeprText String
    | WarnText String
  deriving (Eq,Ord,Show,Typeable,Data,Generic)


-- | A pattern, to be matched against a value.
data Pat
    = PVar Name                     -- ^ variable
    | PLit Sign Literal             -- ^ literal constant
    | PNPlusK Name Integer          -- ^ n+k pattern
    | PInfixApp Pat QName Pat       -- ^ pattern with an infix data constructor
    | PApp QName [Pat]              -- ^ data constructor and argument patterns
    | PTuple Boxed [Pat]            -- ^ tuple pattern
    | PList [Pat]                   -- ^ list pattern
    | PParen Pat                    -- ^ parenthesized pattern
    | PRec QName [PatField]         -- ^ labelled pattern, record style
    | PAsPat Name Pat               -- ^ @\@@-pattern
    | PWildCard                     -- ^ wildcard pattern: @_@
    | PIrrPat Pat                   -- ^ irrefutable pattern: @~/pat/@
    | PatTypeSig SrcLoc Pat Type    -- ^ pattern with type signature
    | PViewPat Exp Pat              -- ^ view patterns of the form @(/exp/ -> /pat/)@
    | PRPat [RPat]                  -- ^ regular list pattern
    | PXTag SrcLoc XName [PXAttr] (Maybe Pat) [Pat]
                                    -- ^ XML element pattern
    | PXETag SrcLoc XName [PXAttr] (Maybe Pat)
                                    -- ^ XML singleton element pattern
    | PXPcdata String               -- ^ XML PCDATA pattern
    | PXPatTag Pat                  -- ^ XML embedded pattern
    | PXRPats [RPat]                -- ^ XML regular list pattern
    | PQuasiQuote String String     -- ^ quasi quote patter: @[$/name/| /string/ |]@
    | PBangPat Pat                  -- ^ strict (bang) pattern: @f !x = ...@
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | An XML attribute in a pattern.
data PXAttr = PXAttr XName Pat
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A regular pattern operator.
data RPatOp
    = RPStar    -- ^ @*@ = 0 or more
    | RPStarG   -- ^ @*!@ = 0 or more, greedy
    | RPPlus    -- ^ @+@ = 1 or more
    | RPPlusG   -- ^ @+!@ = 1 or more, greedy
    | RPOpt     -- ^ @?@ = 0 or 1
    | RPOptG    -- ^ @?!@ = 0 or 1, greedy
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | An entity in a regular pattern.
data RPat
    = RPOp RPat RPatOp      -- ^ operator pattern, e.g. pat*
    | RPEither RPat RPat    -- ^ choice pattern, e.g. (1 | 2)
    | RPSeq [RPat]          -- ^ sequence pattern, e.g. (| 1, 2, 3 |)
    | RPGuard Pat [Stmt]    -- ^ guarded pattern, e.g. (| p | p < 3 |)
    | RPCAs Name RPat       -- ^ non-linear variable binding, e.g. (foo\@:(1 | 2))*
    | RPAs Name RPat        -- ^ linear variable binding, e.g. foo\@(1 | 2)
    | RPParen RPat          -- ^ parenthesised pattern, e.g. (2*)
    | RPPat Pat             -- ^ an ordinary pattern
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | An /fpat/ in a labeled record pattern.
data PatField
    = PFieldPat QName Pat       -- ^ ordinary label-pattern pair
    | PFieldPun QName           -- ^ record field pun
    | PFieldWildcard            -- ^ record field wildcard
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A statement, representing both a /stmt/ in a @do@-expression,
--   an ordinary /qual/ in a list comprehension, as well as a /stmt/
--   in a pattern guard.
data Stmt
    = Generator SrcLoc Pat Exp
                        -- ^ a generator: /pat/ @<-@ /exp/
    | Qualifier Exp     -- ^ an /exp/ by itself: in a @do@-expression,
                        --   an action whose result is discarded;
                        --   in a list comprehension and pattern guard,
                        --   a guard expression
    | LetStmt Binds     -- ^ local bindings
    | RecStmt [Stmt]    -- ^ a recursive binding group for arrows
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | A general /transqual/ in a list comprehension,
--   which could potentially be a transform of the kind
--   enabled by TransformListComp.
data QualStmt
    = QualStmt     Stmt         -- ^ an ordinary statement
    | ThenTrans    Exp          -- ^ @then@ /exp/
    | ThenBy       Exp Exp      -- ^ @then@ /exp/ @by@ /exp/
    | GroupBy      Exp          -- ^ @then@ @group@ @by@ /exp/
    | GroupUsing   Exp          -- ^ @then@ @group@ @using@ /exp/
    | GroupByUsing Exp Exp      -- ^ @then@ @group@ @by@ /exp/ @using@ /exp/
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | An /fbind/ in a labeled construction or update expression.
data FieldUpdate
    = FieldUpdate QName Exp     -- ^ ordinary label-expresion pair
    | FieldPun QName            -- ^ record field pun
    | FieldWildcard             -- ^ record field wildcard
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-- | An /alt/ alternative in a @case@ expression.
data Alt
    = Alt SrcLoc Pat Rhs (Maybe Binds)
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

-----------------------------------------------------------------------------
-- Builtin names.

prelude_mod, main_mod :: ModuleName
prelude_mod = ModuleName "Prelude"
main_mod    = ModuleName "Main"

main_name :: Name
main_name = Ident "main"

unit_con_name :: QName
unit_con_name = Special UnitCon

tuple_con_name :: Boxed -> Int -> QName
tuple_con_name b i = Special (TupleCon b (i+1))

list_cons_name :: QName
list_cons_name = Special Cons

unboxed_singleton_con_name :: QName
unboxed_singleton_con_name = Special UnboxedSingleCon

unit_con :: Exp
unit_con = Con unit_con_name

tuple_con :: Boxed -> Int -> Exp
tuple_con b i = Con (tuple_con_name b i)

unboxed_singleton_con :: Exp
unboxed_singleton_con = Con unboxed_singleton_con_name

as_name, qualified_name, hiding_name, minus_name, bang_name, dot_name, star_name :: Name
as_name        = Ident "as"
qualified_name = Ident "qualified"
hiding_name    = Ident "hiding"
minus_name     = Symbol "-"
bang_name      = Symbol "!"
dot_name       = Symbol "."
star_name      = Symbol "*"

export_name, safe_name, unsafe_name, interruptible_name, threadsafe_name,
  stdcall_name, ccall_name, cplusplus_name, dotnet_name,
  jvm_name, js_name, javascript_name, capi_name, forall_name,
  family_name, role_name :: Name
export_name     = Ident "export"
safe_name       = Ident "safe"
unsafe_name     = Ident "unsafe"
interruptible_name = Ident "interruptible"
threadsafe_name = Ident "threadsafe"
stdcall_name    = Ident "stdcall"
ccall_name      = Ident "ccall"
cplusplus_name  = Ident "cplusplus"
dotnet_name     = Ident "dotnet"
jvm_name        = Ident "jvm"
js_name         = Ident "js"
javascript_name = Ident "js"
capi_name       = Ident "capi"
forall_name     = Ident "forall"
family_name     = Ident "family"
role_name       = Ident "role"

unit_tycon_name, fun_tycon_name, list_tycon_name, unboxed_singleton_tycon_name :: QName
unit_tycon_name = unit_con_name
fun_tycon_name  = Special FunCon
list_tycon_name = Special ListCon
unboxed_singleton_tycon_name = Special UnboxedSingleCon

tuple_tycon_name :: Boxed -> Int -> QName
tuple_tycon_name b i = tuple_con_name b i

unit_tycon, fun_tycon, list_tycon, unboxed_singleton_tycon :: Type
unit_tycon = TyCon unit_tycon_name
fun_tycon  = TyCon fun_tycon_name
list_tycon = TyCon list_tycon_name
unboxed_singleton_tycon = TyCon unboxed_singleton_tycon_name

tuple_tycon :: Boxed -> Int -> Type
tuple_tycon b i = TyCon (tuple_tycon_name b i)

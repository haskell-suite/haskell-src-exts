{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Syntax
-- Copyright   :  (c) Niklas Broberg 2004,
--                (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, d00nibro@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- A suite of datatypes describing the abstract syntax of Haskell 98
-- <http://www.haskell.org/onlinereport/> plus some extensions:
--
--   * multi-parameter type classes with functional dependencies
--
--   * parameters of type class assertions are unrestricted
--
--   * 'forall' types as universal and existential quantification
--
--   * pattern guards
--
--   * implicit parameters
--
--   * generalised algebraic data types
--
--   * template haskell
--
--   * empty data type declarations
--
--   * unboxed tuples
--
--   * regular patterns (HaRP)
--
--   * HSP-style XML expressions and patterns (HSP)
--
-- Also worth noting is that (n+k) patterns from Haskell 98 are not supported
-----------------------------------------------------------------------------

module Language.Haskell.Exts.Syntax (
    -- * Modules
    Module(..), ExportSpec(..),
    ImportDecl(..), ImportSpec(..), Assoc(..),
    -- * Declarations
    Decl(..), Binds(..), IPBind(..), 
    ClassDecl(..), InstDecl(..), Deriving,
    GadtDecl(..), ConDecl(..), QualConDecl(..), BangType(..),
    Match(..), Rhs(..), GuardedRhs(..), DataOrNew(..),
    -- * Class Assertions and Contexts
    Context, FunDep(..), Asst(..),
    -- * Types
    Type(..), Boxed(..), Kind(..), TyVarBind(..),
    -- * Expressions
    Exp(..), Stmt(..), FieldUpdate(..),
    Alt(..), GuardedAlts(..), GuardedAlt(..), 
    -- * Patterns
    Pat(..), PatField(..),
    -- * Literals
    Literal(..),
    -- * Variables, Constructors and Operators
    ModuleName(..), QName(..), Name(..), QOp(..), Op(..),
    SpecialCon(..), CName(..), IPName(..),
    
    -- * Template Haskell
    -- HsReify(..), 
    Bracket(..), Splice(..),
    
    -- * HaRP
    RPat(..), RPatOp(..),
    
    -- * Hsx
    XAttr(..), XName(..), PXAttr(..),

    -- * FFI
    Safety(..), CallConv(..),

    -- * Pragmas
    OptionPragma(..), Tool(..), WarningText(..), 
    Rule(..), RuleVar(..), Activation(..),

    -- * Builtin names

    -- ** Modules
    prelude_mod, main_mod,
    -- ** Main function of a program
    main_name,
    -- ** Constructors
    unit_con_name, tuple_con_name, list_cons_name,
    unit_con, tuple_con,
    -- ** Special identifiers
    as_name, qualified_name, hiding_name, minus_name, pling_name, dot_name, star_name,
    export_name, safe_name, unsafe_name, threadsafe_name, stdcall_name, ccall_name,
    -- ** Type constructors
    unit_tycon_name, fun_tycon_name, list_tycon_name, tuple_tycon_name,
    unit_tycon, fun_tycon, list_tycon, tuple_tycon,

    -- * Source coordinates
    SrcLoc(..),
  ) where


#ifdef __GLASGOW_HASKELL__
#ifdef BASE4
import Data.Data
#else
import Data.Generics (Data(..),Typeable(..))
#endif
#endif

-- | A position in the source.
data SrcLoc = SrcLoc {
        srcFilename :: String,
        srcLine :: Int,
        srcColumn :: Int
        }
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif
  
-- | The name of a Haskell module.
newtype ModuleName = ModuleName String
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Constructors with special syntax.
-- These names are never qualified, and always refer to builtin type or
-- data constructors.

data SpecialCon
    = UnitCon     -- ^ unit type and data constructor @()@
    | ListCon     -- ^ list type constructor @[]@
    | FunCon      -- ^ function type constructor @->@
    | TupleCon Int    -- ^ /n/-ary tuple type and data
                --   constructors @(,)@ etc
    | Cons        -- ^ list data constructor @(:)@
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | This type is used to represent qualified variables, and also
-- qualified constructors.
data QName
    = Qual ModuleName Name    -- ^ name qualified with a module name
    | UnQual Name     -- ^ unqualified name
    | Special SpecialCon  -- ^ built-in constructor with special syntax
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | This type is used to represent variables, and also constructors.
data Name
    = Ident String    -- ^ /varid/ or /conid/.
    | Symbol String   -- ^ /varsym/ or /consym/
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | This type is used to represent implicit parameter names.
data IPName
    = IPDup String -- ?x
    | IPLin String -- %x
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Possibly qualified infix operators (/qop/), appearing in expressions.
data QOp
    = QVarOp QName  -- ^ variable operator (/qvarop/)
    | QConOp QName  -- ^ constructor operator (/qconop/)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Operators, appearing in @infix@ declarations.
data Op
    = VarOp Name    -- ^ variable operator (/varop/)
    | ConOp Name    -- ^ constructor operator (/conop/)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A name (/cname/) of a component of a class or data type in an @import@
-- or export specification.
data CName
    = VarName Name  -- ^ name of a method or field
    | ConName Name  -- ^ name of a data constructor
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A Haskell source module.
data Module = Module SrcLoc ModuleName [OptionPragma] (Maybe WarningText) 
                        (Maybe [ExportSpec]) [ImportDecl] [Decl]
#ifdef __GLASGOW_HASKELL__
  deriving (Show,Typeable,Data)
#else
  deriving (Show)
#endif

-- | Export specification.
data ExportSpec
     = EVar QName           -- ^ variable
     | EAbs QName           -- ^ @T@:
            -- a class or datatype exported abstractly,
            -- or a type synonym.
     | EThingAll QName          -- ^ @T(..)@:
            -- a class exported with all of its methods, or
            -- a datatype exported with all of its constructors.
     | EThingWith QName [CName]   -- ^ @T(C_1,...,C_n)@:
            -- a class exported with some of its methods, or
            -- a datatype exported with some of its constructors.
     | EModuleContents ModuleName     -- ^ @module M@:
            -- re-export a module.
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Import declaration.
data ImportDecl = ImportDecl
    { importLoc :: SrcLoc           -- ^ position of the @import@ keyword.
    , importModule :: ModuleName    -- ^ name of the module imported.
    , importQualified :: Bool       -- ^ imported @qualified@?
    , importSrc :: Bool             -- ^ imported with {-# SOURCE #-}
    , importAs :: Maybe ModuleName  -- ^ optional alias name in an
                    -- @as@ clause.
    , importSpecs :: Maybe (Bool,[ImportSpec])
            -- ^ optional list of import specifications.
            -- The 'Bool' is 'True' if the names are excluded
            -- by @hiding@.
    }
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Import specification.
data ImportSpec
     = IVar Name            -- ^ variable
     | IAbs Name            -- ^ @T@:
            -- the name of a class, datatype or type synonym.
     | IThingAll Name           -- ^ @T(..)@:
            -- a class imported with all of its methods, or
            -- a datatype imported with all of its constructors.
     | IThingWith Name [CName]    -- ^ @T(C_1,...,C_n)@:
            -- a class imported with some of its methods, or
            -- a datatype imported with some of its constructors.
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Associativity of an operator.
data Assoc
     = AssocNone  -- ^ non-associative operator (declared with @infix@)
     | AssocLeft  -- ^ left-associative operator (declared with @infixl@).
     | AssocRight -- ^ right-associative operator (declared with @infixr@)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

type Deriving = (QName, [QName])

data Decl
     = TypeDecl     SrcLoc Name [Name] Type
     | DataDecl     SrcLoc DataOrNew Context Name [Name] [QualConDecl] [Deriving]
     | GDataDecl    SrcLoc DataOrNew Context Name [Name] (Maybe Kind) [GadtDecl] [Deriving]
     | TypeFamDecl  SrcLoc Name [Name] (Maybe Kind)
     | DataFamDecl  SrcLoc Context Name [Name] (Maybe Kind)
     | TypeInsDecl  SrcLoc Type Type
     | DataInsDecl  SrcLoc DataOrNew Type [QualConDecl] [Deriving]
     | GDataInsDecl SrcLoc DataOrNew Type (Maybe Kind) [GadtDecl] [Deriving]
     | InfixDecl    SrcLoc Assoc Int [Op]
     | ClassDecl    SrcLoc Context Name [Name] [FunDep] [ClassDecl]
     | InstDecl     SrcLoc Context QName [Type] [InstDecl]
     | DerivDecl    SrcLoc Context QName [Type]
     | DefaultDecl  SrcLoc [Type]
     | SpliceDecl   SrcLoc Splice
     | TypeSig      SrcLoc [Name] Type
     | FunBind      [Match]
     | PatBind      SrcLoc Pat (Maybe Type) Rhs {-where-} Binds
     | ForImp   SrcLoc CallConv Safety String Name Type
     | ForExp   SrcLoc CallConv          String Name Type
-- Pragmas
     | RulePragmaDecl   SrcLoc [Rule]
     | DeprPragmaDecl   SrcLoc [([Name], String)]
     | WarnPragmaDecl   SrcLoc [([Name], String)]
     | InlineSig        SrcLoc Bool Activation QName
     | SpecSig          SrcLoc                 QName [Type]
     | SpecInlineSig    SrcLoc Bool Activation QName [Type]
     | InstSig          SrcLoc Context         QName [Type]
     | UnknownDeclPragma SrcLoc String String
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data DataOrNew = DataType | NewType
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data Binds
    = BDecls [Decl]
    | IPBinds [IPBind]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data IPBind = IPBind SrcLoc IPName Exp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Clauses of a function binding.
data Match
     = Match SrcLoc Name [Pat] (Maybe Type) Rhs {-where-} Binds
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data QualConDecl
    = QualConDecl SrcLoc 
        {-forall-} [TyVarBind] {- . -} Context
        {- => -} ConDecl
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data GadtDecl 
    = GadtDecl SrcLoc Name Type
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Declaration of a data constructor.
data ConDecl
     = ConDecl Name [BangType]
                -- ^ ordinary data constructor
     | RecDecl Name [([Name],BangType)]
                -- ^ record constructor
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Declarations inside a class declaration
data ClassDecl
    = ClsDecl    Decl
    | ClsDataFam SrcLoc Context Name [Name] (Maybe Kind)
    | ClsTyFam   SrcLoc         Name [Name] (Maybe Kind)
    | ClsTyDef   SrcLoc Type    Type
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Declarations inside an instance declaration
data InstDecl
    = InsDecl   Decl
    | InsType   SrcLoc Type Type
    | InsData   SrcLoc DataOrNew Type [QualConDecl] [Deriving]
    | InsGData  SrcLoc DataOrNew Type (Maybe Kind) [GadtDecl] [Deriving]
    | InsInline SrcLoc Bool Activation QName
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | The type of a constructor argument or field, optionally including
-- a strictness annotation.
data BangType
     = BangedTy   Type  -- ^ strict component, marked with \"@!@\"
     | UnBangedTy Type  -- ^ non-strict component
     | UnpackedTy Type  -- ^ unboxed component
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | The right hand side of a function or pattern binding.
data Rhs
     = UnGuardedRhs Exp -- ^ unguarded right hand side (/exp/)
     | GuardedRhss  [GuardedRhs]
                -- ^ guarded right hand side (/gdrhs/)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif
-- | A guarded right hand side @|@ /exp/ @=@ /exp/.
-- The first expression will be Boolean-valued.
data GuardedRhs
     = GuardedRhs SrcLoc [Stmt] Exp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | A type qualified with a context.
--   An unqualified type has an empty context.

data Type
     = TyForall 
        (Maybe [TyVarBind])
        Context
        Type
     | TyFun   Type Type  -- ^ function type
     | TyTuple Boxed [Type]   -- ^ tuple type, possibly boxed
     | TyApp   Type Type  -- ^ application of a type constructor
     | TyVar   Name     -- ^ type variable
     | TyCon   QName        -- ^ named type or type constructor
     | TyPred  Asst         -- ^ assertion of an implicit parameter
     | TyInfix Type QName Type -- ^ infix type constructor
     | TyKind  Type Kind  -- ^ type with explicit kind signature
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data Boxed = Boxed | Unboxed
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data TyVarBind 
    = KindedVar Name Kind
    | UnkindedVar Name
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data Kind
    = KindStar
    | KindBang
    | KindFn Kind Kind
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif
    

-- | A functional dependency, given on the form
--   l1 l2 ... ln -> r2 r3 .. rn
data FunDep
    = FunDep [Name] [Name]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif


type Context = [Asst]

-- | Class assertions.
--   In Haskell 98, the argument would be a /tyvar/, but this definition
--   allows multiple parameters, and allows them to be /type/s.
--   Also extended with support for implicit parameters and equality constraints.
data Asst     = ClassA QName [Type]
        | IParam IPName Type
        | EqualP Type   Type
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | /literal/
-- Values of this type hold the abstract value of the literal, not the
-- precise string representation used.  For example, @10@, @0o12@ and @0xa@
-- have the same representation.
data Literal
    = Char    Char          -- ^ character literal
    | String  String        -- ^ string literal
    | Int     Integer       -- ^ integer literal
    | Frac    Rational      -- ^ floating point literal
    | PrimInt    Integer    -- ^ GHC unboxed integer literal
    | PrimWord   Integer    -- ^ GHC unboxed word literal
    | PrimFloat  Rational   -- ^ GHC unboxed float literal
    | PrimDouble Rational   -- ^ GHC unboxed double literal
    | PrimChar   Char       -- ^ GHC unboxed character literal
    | PrimString String     -- ^ GHC unboxed string literal
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Haskell expressions.
--
-- /Notes:/
--
-- * Because it is difficult for parsers to distinguish patterns from
--   expressions, they typically parse them in the same way and then check
--   that they have the appropriate form.  Hence the expression type
--   includes some forms that are found only in patterns.  After these
--   checks, these constructors should not be used.
--
-- * The parser does not take precedence and associativity into account,
--   so it will leave 'InfixApp's associated to the left.
--
-- * The 'Language.Haskell.Exts.Pretty.Pretty' instance for 'Exp' does not
--   add parentheses in printing.

data Exp
    = Var QName                 -- ^ variable
    | IPVar IPName              -- ^ implicit parameter variable
    | Con QName                 -- ^ data constructor
    | Lit Literal               -- ^ literal constant
    | InfixApp Exp QOp Exp  -- ^ infix application
    | App Exp Exp             -- ^ ordinary application
    | NegApp Exp                -- ^ negation expression @-@ /exp/
    | Lambda SrcLoc [Pat] Exp -- ^ lambda expression
    | Let Binds Exp           -- ^ local declarations with @let@
    | If Exp Exp Exp        -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    | Case Exp [Alt]          -- ^ @case@ /exp/ @of@ /alts/
    | Do [Stmt]                 -- ^ @do@-expression:
                                    -- the last statement in the list
                                    -- should be an expression.
    | MDo [Stmt]                -- ^ @mdo@-expression
    | Tuple [Exp]               -- ^ tuple expression
    | List [Exp]                -- ^ list expression
    | Paren Exp                 -- ^ parenthesized expression
    | LeftSection Exp QOp     -- ^ left section @(@/exp/ /qop/@)@
    | RightSection QOp Exp    -- ^ right section @(@/qop/ /exp/@)@
    | RecConstr QName [FieldUpdate]
                                    -- ^ record construction expression
    | RecUpdate Exp [FieldUpdate]
                                    -- ^ record update expression
    | EnumFrom Exp              -- ^ unbounded arithmetic sequence,
                                    -- incrementing by 1
    | EnumFromTo Exp Exp      -- ^ bounded arithmetic sequence,
                                    -- incrementing by 1
    | EnumFromThen Exp Exp    -- ^ unbounded arithmetic sequence,
                                    -- with first two elements given
    | EnumFromThenTo Exp Exp Exp
                                    -- ^ bounded arithmetic sequence,
                                    -- with first two elements given
    | ListComp Exp [Stmt]     -- ^ list comprehension
    | ExpTypeSig SrcLoc Exp Type
                                    -- ^ expression type signature
-- Template Haskell
    | VarQuote QName            -- ^ 'x
    | TypQuote QName            -- ^ ''T
    | BracketExp Bracket
    | SpliceExp Splice
    
-- Hsx
    | XTag SrcLoc XName [XAttr] (Maybe Exp) [Exp]
    | XETag SrcLoc XName [XAttr] (Maybe Exp)
    | XPcdata String
    | XExpTag Exp

-- Pragmas
    | CorePragma        String
    | SCCPragma         String
    | GenPragma         String (Int, Int) (Int, Int)
    | UnknownExpPragma  String String
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data XName 
    = XName String              -- <name ...
    | XDomName String String    -- <dom:name ...
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data XAttr = XAttr XName Exp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data Bracket
    = ExpBracket Exp
    | PatBracket Pat
    | TypeBracket Type
    | DeclBracket [Decl]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data Splice
    = IdSplice String
    | ParenSplice Exp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif


-- FFI stuff
data Safety
    = PlayRisky
    | PlaySafe Bool
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data CallConv
    = StdCall
    | CCall
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- Pragma stuff
data OptionPragma
    = LanguagePragma   SrcLoc [Name]
    | IncludePragma    SrcLoc String
    | CFilesPragma     SrcLoc String
    | OptionsPragma    SrcLoc (Maybe Tool) String
    | UnknownTopPragma SrcLoc String String
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data Tool = GHC | HUGS | NHC98 | YHC | HADDOCK | UnknownTool String
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data Activation
    = AlwaysActive
    | ActiveFrom  Int
    | ActiveUntil Int
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data Rule
    = Rule String Activation (Maybe [RuleVar]) Exp Exp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data RuleVar
    = RuleVar Name
    | TypedRuleVar Name Type
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data WarningText
    = DeprText String
    | WarnText String
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif


-- | A pattern, to be matched against a value.
data Pat
    = PVar Name                 -- ^ variable
    | PLit Literal              -- ^ literal constant
    | PNeg Pat                  -- ^ negated pattern
    | PNPlusK Name Integer      -- ^ n+k pattern
    | PInfixApp Pat QName Pat
                                    -- ^ pattern with infix data constructor
    | PApp QName [Pat]        -- ^ data constructor and argument
                                    -- patterns
    | PTuple [Pat]              -- ^ tuple pattern
    | PList [Pat]               -- ^ list pattern
    | PParen Pat                -- ^ parenthesized pattern
    | PRec QName [PatField]   -- ^ labelled pattern
    | PAsPat Name Pat         -- ^ @\@@-pattern
    | PWildCard                   -- ^ wildcard pattern (@_@)
    | PIrrPat Pat               -- ^ irrefutable pattern (@~@)
    | PatTypeSig SrcLoc Pat Type
                                    -- ^ pattern type signature
    | PViewPat Exp Pat          -- ^ view patterns of the form (e -> p)
-- HaRP
    | PRPat [RPat]              -- ^ regular pattern (HaRP)
-- Hsx
    | PXTag SrcLoc XName [PXAttr] (Maybe Pat) [Pat]
                                    -- ^ XML tag pattern
    | PXETag SrcLoc XName [PXAttr] (Maybe Pat)
                                    -- ^ XML singleton tag pattern
    | PXPcdata String
                                    -- ^ XML PCDATA pattern
    | PXPatTag Pat
                                    -- ^ XML embedded pattern
    | PXRPats [RPat]
                                    -- ^ XML regular list pattern
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | An XML attribute in an XML tag pattern 
data PXAttr = PXAttr XName Pat
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | A regular pattern operator (HaRP)
data RPatOp
        = RPStar
    | RPStarG
    | RPPlus
    | RPPlusG
    | RPOpt
    | RPOptG
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif
        
-- | An entity in a regular pattern (HaRP)
data RPat
    = RPOp RPat RPatOp
    | RPEither RPat RPat
    | RPSeq [RPat]
    | RPGuard Pat [Stmt]
    | RPCAs Name RPat
    | RPAs Name RPat
    | RPParen RPat
    | RPPat Pat
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | An /fpat/ in a labeled record pattern.
data PatField
    = PFieldPat QName Pat
    | PFieldPun Name
    | PFieldWildcard
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | This type represents both /stmt/ in a @do@-expression,
--   and /qual/ in a list comprehension, as well as /stmt/
--   in a pattern guard.
data Stmt
    = Generator SrcLoc Pat Exp
                -- ^ a generator /pat/ @<-@ /exp/
    | Qualifier Exp -- ^ an /exp/ by itself: in a @do@-expression,
                -- an action whose result is discarded;
                -- in a list comprehension, a guard expression
    | LetStmt Binds -- ^ local bindings
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | An /fbind/ in a labeled construction or update.
data FieldUpdate
    = FieldUpdate QName Exp
    | FieldPun Name
    | FieldWildcard
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | An /alt/ in a @case@ expression.
data Alt
    = Alt SrcLoc Pat GuardedAlts Binds
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data GuardedAlts
    = UnGuardedAlt Exp      -- ^ @->@ /exp/
    | GuardedAlts  [GuardedAlt] -- ^ /gdpat/
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | A guarded alternative @|@ /stmt/, ... , /stmt/ @->@ /exp/.
data GuardedAlt
    = GuardedAlt SrcLoc [Stmt] Exp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-----------------------------------------------------------------------------
-- Builtin names.

prelude_mod, main_mod :: ModuleName
prelude_mod       = ModuleName "Prelude"
main_mod          = ModuleName "Main"

main_name :: Name
main_name         = Ident "main"

unit_con_name :: QName
unit_con_name         = Special UnitCon

tuple_con_name :: Int -> QName
tuple_con_name i      = Special (TupleCon (i+1))

list_cons_name :: QName
list_cons_name        = Special Cons

unit_con :: Exp
unit_con          = Con unit_con_name

tuple_con :: Int -> Exp
tuple_con i       = Con (tuple_con_name i)

as_name, qualified_name, hiding_name, minus_name, pling_name, dot_name, star_name :: Name
as_name               = Ident "as"
qualified_name        = Ident "qualified"
hiding_name       = Ident "hiding"
minus_name        = Symbol "-"
pling_name        = Symbol "!"
dot_name          = Symbol "."
star_name             = Symbol "*"

export_name, safe_name, unsafe_name, threadsafe_name, stdcall_name, ccall_name :: Name
export_name     = Ident "export"
safe_name       = Ident "safe"
unsafe_name     = Ident "unsafe"
threadsafe_name     = Ident "threadsafe"
stdcall_name        = Ident "stdcall"
ccall_name      = Ident "ccall"

unit_tycon_name, fun_tycon_name, list_tycon_name :: QName
unit_tycon_name       = unit_con_name
fun_tycon_name        = Special FunCon
list_tycon_name       = Special ListCon

tuple_tycon_name :: Int -> QName
tuple_tycon_name i    = tuple_con_name i

unit_tycon, fun_tycon, list_tycon :: Type
unit_tycon        = TyCon unit_tycon_name
fun_tycon         = TyCon fun_tycon_name
list_tycon        = TyCon list_tycon_name

tuple_tycon :: Int -> Type
tuple_tycon i         = TyCon (tuple_tycon_name i)

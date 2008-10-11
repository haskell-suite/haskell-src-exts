{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Syntax
-- Original    :  Language.Haskell.Syntax
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
    HsModule(..), HsExportSpec(..),
    HsImportDecl(..), HsImportSpec(..), HsAssoc(..),
    -- * Declarations
    HsDecl(..), HsBinds(..), HsIPBind(..), 
    HsClassDecl(..), HsInstDecl(..),
    HsGadtDecl(..), HsConDecl(..), HsQualConDecl(..), HsBangType(..),
    HsMatch(..), HsRhs(..), HsGuardedRhs(..), DataOrNew(..),
    -- * Class Assertions and Contexts
    HsContext, HsFunDep(..), HsAsst(..),
    -- * Types
    HsType(..), HsBoxed(..), HsKind(..), HsTyVarBind(..),
    -- * Expressions
    HsExp(..), HsStmt(..), HsFieldUpdate(..),
    HsAlt(..), HsGuardedAlts(..), HsGuardedAlt(..), 
    -- * Patterns
    HsPat(..), HsPatField(..),
    -- * Literals
    HsLiteral(..),
    -- * Variables, Constructors and Operators
    Module(..), HsQName(..), HsName(..), HsQOp(..), HsOp(..),
    HsSpecialCon(..), HsCName(..), HsIPName(..),
    
    -- * Template Haskell
    -- HsReify(..), 
    HsBracket(..), HsSplice(..),
    
    -- * HaRP
    HsRPat(..), HsRPatOp(..),
    
    -- * Hsx
    HsXAttr(..), HsXName(..), HsPXAttr(..),

    -- * FFI
    HsSafety(..), HsCallConv(..),

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
#if __GLASGOW_HASKELL__ >= 610
import Data.Data
#else
import Data.Generics.Basics
import Data.Generics.Instances
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
newtype Module = Module String
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Constructors with special syntax.
-- These names are never qualified, and always refer to builtin type or
-- data constructors.

data HsSpecialCon
    = HsUnitCon     -- ^ unit type and data constructor @()@
    | HsListCon     -- ^ list type constructor @[]@
    | HsFunCon      -- ^ function type constructor @->@
    | HsTupleCon Int    -- ^ /n/-ary tuple type and data
                --   constructors @(,)@ etc
    | HsCons        -- ^ list data constructor @(:)@
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | This type is used to represent qualified variables, and also
-- qualified constructors.
data HsQName
    = Qual Module HsName    -- ^ name qualified with a module name
    | UnQual HsName     -- ^ unqualified name
    | Special HsSpecialCon  -- ^ built-in constructor with special syntax
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | This type is used to represent variables, and also constructors.
data HsName
    = HsIdent String    -- ^ /varid/ or /conid/.
    | HsSymbol String   -- ^ /varsym/ or /consym/
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | This type is used to represent implicit parameter names.
data HsIPName
    = HsIPDup String -- ?x
    | HsIPLin String -- %x
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Possibly qualified infix operators (/qop/), appearing in expressions.
data HsQOp
    = HsQVarOp HsQName  -- ^ variable operator (/qvarop/)
    | HsQConOp HsQName  -- ^ constructor operator (/qconop/)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Operators, appearing in @infix@ declarations.
data HsOp
    = HsVarOp HsName    -- ^ variable operator (/varop/)
    | HsConOp HsName    -- ^ constructor operator (/conop/)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A name (/cname/) of a component of a class or data type in an @import@
-- or export specification.
data HsCName
    = HsVarName HsName  -- ^ name of a method or field
    | HsConName HsName  -- ^ name of a data constructor
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A Haskell source module.
data HsModule = HsModule SrcLoc Module (Maybe [HsExportSpec])
                         [HsImportDecl] [HsDecl]
#ifdef __GLASGOW_HASKELL__
  deriving (Show,Typeable,Data)
#else
  deriving (Show)
#endif

-- | Export specification.
data HsExportSpec
     = HsEVar HsQName           -- ^ variable
     | HsEAbs HsQName           -- ^ @T@:
            -- a class or datatype exported abstractly,
            -- or a type synonym.
     | HsEThingAll HsQName          -- ^ @T(..)@:
            -- a class exported with all of its methods, or
            -- a datatype exported with all of its constructors.
     | HsEThingWith HsQName [HsCName]   -- ^ @T(C_1,...,C_n)@:
            -- a class exported with some of its methods, or
            -- a datatype exported with some of its constructors.
     | HsEModuleContents Module     -- ^ @module M@:
            -- re-export a module.
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Import declaration.
data HsImportDecl = HsImportDecl
    { importLoc :: SrcLoc       -- ^ position of the @import@ keyword.
    , importModule :: Module    -- ^ name of the module imported.
    , importQualified :: Bool   -- ^ imported @qualified@?
    , importAs :: Maybe Module  -- ^ optional alias name in an
                    -- @as@ clause.
    , importSpecs :: Maybe (Bool,[HsImportSpec])
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
data HsImportSpec
     = HsIVar HsName            -- ^ variable
     | HsIAbs HsName            -- ^ @T@:
            -- the name of a class, datatype or type synonym.
     | HsIThingAll HsName           -- ^ @T(..)@:
            -- a class imported with all of its methods, or
            -- a datatype imported with all of its constructors.
     | HsIThingWith HsName [HsCName]    -- ^ @T(C_1,...,C_n)@:
            -- a class imported with some of its methods, or
            -- a datatype imported with some of its constructors.
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Associativity of an operator.
data HsAssoc
     = HsAssocNone  -- ^ non-associative operator (declared with @infix@)
     | HsAssocLeft  -- ^ left-associative operator (declared with @infixl@).
     | HsAssocRight -- ^ right-associative operator (declared with @infixr@)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data HsDecl
     = HsTypeDecl     SrcLoc HsName [HsName] HsType
     | HsDataDecl     SrcLoc DataOrNew HsContext HsName [HsName] [HsQualConDecl] [HsQName]
     | HsGDataDecl    SrcLoc DataOrNew HsContext HsName [HsName] (Maybe HsKind) [HsGadtDecl] {-no deriving-}
     | HsTypeFamDecl  SrcLoc HsName [HsName] (Maybe HsKind)
     | HsDataFamDecl  SrcLoc HsContext HsName [HsName] (Maybe HsKind)
     | HsTypeInsDecl  SrcLoc HsType HsType
     | HsDataInsDecl  SrcLoc DataOrNew HsType [HsQualConDecl] [HsQName]
     | HsGDataInsDecl SrcLoc DataOrNew HsType (Maybe HsKind) [HsGadtDecl] {-no deriving-}
     | HsInfixDecl    SrcLoc HsAssoc Int [HsOp]
     | HsClassDecl    SrcLoc HsContext HsName [HsName] [HsFunDep] [HsClassDecl]
     | HsInstDecl     SrcLoc HsContext HsQName [HsType] [HsInstDecl]
     | HsDerivDecl    SrcLoc HsContext HsQName [HsType]
     | HsDefaultDecl  SrcLoc [HsType]
     | HsSpliceDecl   SrcLoc HsSplice
     | HsTypeSig      SrcLoc [HsName] HsType
     | HsFunBind      [HsMatch]
     | HsPatBind      SrcLoc HsPat HsRhs {-where-} HsBinds
     | HsForImp   SrcLoc HsCallConv HsSafety String HsName HsType
     | HsForExp   SrcLoc HsCallConv          String HsName HsType
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

data HsBinds
    = HsBDecls [HsDecl]
    | HsIPBinds [HsIPBind]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data HsIPBind = HsIPBind SrcLoc HsIPName HsExp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Clauses of a function binding.
data HsMatch
     = HsMatch SrcLoc HsName [HsPat] HsRhs {-where-} HsBinds
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data HsQualConDecl
    = HsQualConDecl SrcLoc 
        {-forall-} [HsTyVarBind] {- . -} HsContext
        {- => -} HsConDecl
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data HsGadtDecl 
    = HsGadtDecl SrcLoc HsName HsType
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Declaration of a data constructor.
data HsConDecl
     = HsConDecl HsName [HsBangType]
                -- ^ ordinary data constructor
     | HsRecDecl HsName [([HsName],HsBangType)]
                -- ^ record constructor
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Declarations inside a class declaration
data HsClassDecl
    = HsClsDecl    HsDecl
    | HsClsDataFam SrcLoc HsContext HsName [HsName] (Maybe HsKind)
    | HsClsTyFam   SrcLoc           HsName [HsName] (Maybe HsKind)
    | HsClsTyDef   SrcLoc HsType    HsType
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Declarations inside an instance declaration
data HsInstDecl
    = HsInsDecl   HsDecl
    | HsInsType   SrcLoc HsType HsType
    | HsInsData   SrcLoc DataOrNew HsType [HsQualConDecl] [HsQName]
    | HsInsGData  SrcLoc DataOrNew HsType (Maybe HsKind) [HsGadtDecl] {-no deriving-}
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | The type of a constructor argument or field, optionally including
-- a strictness annotation.
data HsBangType
     = HsBangedTy   HsType  -- ^ strict component, marked with \"@!@\"
     | HsUnBangedTy HsType  -- ^ non-strict component
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | The right hand side of a function or pattern binding.
data HsRhs
     = HsUnGuardedRhs HsExp -- ^ unguarded right hand side (/exp/)
     | HsGuardedRhss  [HsGuardedRhs]
                -- ^ guarded right hand side (/gdrhs/)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif
-- | A guarded right hand side @|@ /exp/ @=@ /exp/.
-- The first expression will be Boolean-valued.
data HsGuardedRhs
     = HsGuardedRhs SrcLoc [HsStmt] HsExp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | A type qualified with a context.
--   An unqualified type has an empty context.

data HsType
     = HsTyForall 
        (Maybe [HsTyVarBind])
        HsContext
        HsType
     | HsTyFun   HsType HsType  -- ^ function type
     | HsTyTuple HsBoxed [HsType]   -- ^ tuple type, possibly boxed
     | HsTyApp   HsType HsType  -- ^ application of a type constructor
     | HsTyVar   HsName     -- ^ type variable
     | HsTyCon   HsQName        -- ^ named type or type constructor
     | HsTyPred  HsAsst         -- ^ assertion of an implicit parameter
     | HsTyInfix HsType HsQName HsType -- ^ infix type constructor
     | HsTyKind  HsType HsKind  -- ^ type with explicit kind signature
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data HsBoxed = Boxed | Unboxed
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data HsTyVarBind 
    = HsKindedVar HsName HsKind
    | HsUnkindedVar HsName
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data HsKind
    = HsKindStar
    | HsKindBang
    | HsKindFn HsKind HsKind
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif
    

-- | A functional dependency, given on the form
--   l1 l2 ... ln -> r2 r3 .. rn
data HsFunDep
    = HsFunDep [HsName] [HsName]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif


type HsContext = [HsAsst]

-- | Class assertions.
--   In Haskell 98, the argument would be a /tyvar/, but this definition
--   allows multiple parameters, and allows them to be /type/s.
--   Also extended with support for implicit parameters and equality constraints.
data HsAsst     = HsClassA HsQName [HsType]
        | HsIParam HsIPName HsType
        | HsEqualP HsType   HsType
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | /literal/
-- Values of this type hold the abstract value of the literal, not the
-- precise string representation used.  For example, @10@, @0o12@ and @0xa@
-- have the same representation.
data HsLiteral
    = HsChar    Char        -- ^ character literal
    | HsString  String      -- ^ string literal
    | HsInt     Integer     -- ^ integer literal
    | HsFrac    Rational    -- ^ floating point literal
    | HsCharPrim    Char        -- ^ GHC unboxed character literal
    | HsStringPrim  String      -- ^ GHC unboxed string literal
    | HsIntPrim Integer     -- ^ GHC unboxed integer literal
    | HsFloatPrim   Rational    -- ^ GHC unboxed float literal
    | HsDoublePrim  Rational    -- ^ GHC unboxed double literal
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
--   so it will leave 'HsInfixApp's associated to the left.
--
-- * The 'Language.Haskell.Pretty.Pretty' instance for 'HsExp' does not
--   add parentheses in printing.

data HsExp
    = HsVar HsQName                 -- ^ variable
    | HsIPVar HsIPName              -- ^ implicit parameter variable
    | HsCon HsQName                 -- ^ data constructor
    | HsLit HsLiteral               -- ^ literal constant
    | HsInfixApp HsExp HsQOp HsExp  -- ^ infix application
    | HsApp HsExp HsExp             -- ^ ordinary application
    | HsNegApp HsExp                -- ^ negation expression @-@ /exp/
    | HsLambda SrcLoc [HsPat] HsExp -- ^ lambda expression
    | HsLet HsBinds HsExp           -- ^ local declarations with @let@
    | HsDLet [HsIPBind] HsExp       -- ^ local declarations of implicit parameters (hugs)
    | HsWith HsExp [HsIPBind]       -- ^ local declarations of implicit parameters
    | HsIf HsExp HsExp HsExp        -- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
    | HsCase HsExp [HsAlt]          -- ^ @case@ /exp/ @of@ /alts/
    | HsDo [HsStmt]                 -- ^ @do@-expression:
                                    -- the last statement in the list
                                    -- should be an expression.
    | HsMDo [HsStmt]                -- ^ @mdo@-expression
    | HsTuple [HsExp]               -- ^ tuple expression
    | HsList [HsExp]                -- ^ list expression
    | HsParen HsExp                 -- ^ parenthesized expression
    | HsLeftSection HsExp HsQOp     -- ^ left section @(@/exp/ /qop/@)@
    | HsRightSection HsQOp HsExp    -- ^ right section @(@/qop/ /exp/@)@
    | HsRecConstr HsQName [HsFieldUpdate]
                                    -- ^ record construction expression
    | HsRecUpdate HsExp [HsFieldUpdate]
                                    -- ^ record update expression
    | HsEnumFrom HsExp              -- ^ unbounded arithmetic sequence,
                                    -- incrementing by 1
    | HsEnumFromTo HsExp HsExp      -- ^ bounded arithmetic sequence,
                                    -- incrementing by 1
    | HsEnumFromThen HsExp HsExp    -- ^ unbounded arithmetic sequence,
                                    -- with first two elements given
    | HsEnumFromThenTo HsExp HsExp HsExp
                                    -- ^ bounded arithmetic sequence,
                                    -- with first two elements given
    | HsListComp HsExp [HsStmt]     -- ^ list comprehension
    | HsExpTypeSig SrcLoc HsExp HsType
                                    -- ^ expression type signature
    | HsAsPat HsName HsExp          -- ^ patterns only
    | HsWildCard                    -- ^ patterns only
    | HsIrrPat HsExp                -- ^ patterns only

-- Post-ops for parsing left sections and regular patterns. Not to be left in the final tree.
    | HsPostOp HsExp HsQOp          -- ^ post-ops

-- HaRP
    | HsSeqRP [HsExp]               -- ^ regular patterns only
    | HsGuardRP HsExp [HsStmt]      -- ^ regular patterns only
    | HsEitherRP HsExp HsExp        -- ^ regular patterns only
    | HsCAsRP HsName HsExp          -- ^ regular patterns only
    
-- Template Haskell
--    | HsReifyExp HsReify
    | HsVarQuote HsQName            -- ^ 'x
    | HsTypQuote HsQName            -- ^ ''T
    | HsBracketExp HsBracket
    | HsSpliceExp HsSplice
    
-- Hsx
    | HsXTag SrcLoc HsXName [HsXAttr] (Maybe HsExp) [HsExp]
    | HsXETag SrcLoc HsXName [HsXAttr] (Maybe HsExp)
    | HsXPcdata String
    | HsXExpTag HsExp
    | HsXRPats [HsExp]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data HsXName 
    = HsXName String        -- <name ...
    | HsXDomName String String  -- <dom:name ...
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data HsXAttr = HsXAttr HsXName HsExp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

{-
data HsReify 
    = HsReifyType HsQName
    | HsReifyDecl HsQName
    | HsReifyFixity HsQName
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif
-}
 
data HsBracket
    = HsExpBracket HsExp
    | HsPatBracket HsPat
    | HsTypeBracket HsType
    | HsDeclBracket [HsDecl]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data HsSplice
    = HsIdSplice String
    | HsParenSplice HsExp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif


-- FFI stuff
data HsSafety
    = PlayRisky
    | PlaySafe Bool
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data HsCallConv
    = StdCall
    | CCall
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif



-- | A pattern, to be matched against a value.
data HsPat
    = HsPVar HsName                 -- ^ variable
    | HsPLit HsLiteral              -- ^ literal constant
    | HsPNeg HsPat                  -- ^ negated pattern
    | HsPInfixApp HsPat HsQName HsPat
                                    -- ^ pattern with infix data constructor
    | HsPApp HsQName [HsPat]        -- ^ data constructor and argument
                                    -- patterns
    | HsPTuple [HsPat]              -- ^ tuple pattern
    | HsPList [HsPat]               -- ^ list pattern
    | HsPParen HsPat                -- ^ parenthesized pattern
    | HsPRec HsQName [HsPatField]   -- ^ labelled pattern
    | HsPAsPat HsName HsPat         -- ^ @\@@-pattern
    | HsPWildCard                   -- ^ wildcard pattern (@_@)
    | HsPIrrPat HsPat               -- ^ irrefutable pattern (@~@)
    | HsPatTypeSig SrcLoc HsPat HsType
                                    -- ^ pattern type signature
-- HaRP
    | HsPRPat [HsRPat]              -- ^ regular pattern (HaRP)
-- Hsx
    | HsPXTag SrcLoc HsXName [HsPXAttr] (Maybe HsPat) [HsPat]
                                    -- ^ XML tag pattern
    | HsPXETag SrcLoc HsXName [HsPXAttr] (Maybe HsPat)
                                    -- ^ XML singleton tag pattern
    | HsPXPcdata String
                                    -- ^ XML PCDATA pattern
    | HsPXPatTag HsPat
                                    -- ^ XML embedded pattern
    | HsPXRPats [HsRPat]
                                    -- ^ XML regular list pattern
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | An XML attribute in an XML tag pattern 
data HsPXAttr = HsPXAttr HsXName HsPat
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | A regular pattern operator (HaRP)
data HsRPatOp
        = HsRPStar
    | HsRPStarG
    | HsRPPlus
    | HsRPPlusG
    | HsRPOpt
    | HsRPOptG
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif
        
-- | An entity in a regular pattern (HaRP)
data HsRPat
    = HsRPOp HsRPat HsRPatOp
    | HsRPEither HsRPat HsRPat
    | HsRPSeq [HsRPat]
    | HsRPGuard HsPat [HsStmt]
    | HsRPCAs HsName HsRPat
    | HsRPAs HsName HsRPat
    | HsRPParen HsRPat
    | HsRPPat HsPat
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | An /fpat/ in a labeled record pattern.
data HsPatField
    = HsPFieldPat HsQName HsPat
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | This type represents both /stmt/ in a @do@-expression,
--   and /qual/ in a list comprehension, as well as /stmt/
--   in a pattern guard.
data HsStmt
    = HsGenerator SrcLoc HsPat HsExp
                -- ^ a generator /pat/ @<-@ /exp/
    | HsQualifier HsExp -- ^ an /exp/ by itself: in a @do@-expression,
                -- an action whose result is discarded;
                -- in a list comprehension, a guard expression
    | HsLetStmt HsBinds -- ^ local bindings
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | An /fbind/ in a labeled construction or update.
data HsFieldUpdate
    = HsFieldUpdate HsQName HsExp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | An /alt/ in a @case@ expression.
data HsAlt
    = HsAlt SrcLoc HsPat HsGuardedAlts HsBinds
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data HsGuardedAlts
    = HsUnGuardedAlt HsExp      -- ^ @->@ /exp/
    | HsGuardedAlts  [HsGuardedAlt] -- ^ /gdpat/
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | A guarded alternative @|@ /stmt/, ... , /stmt/ @->@ /exp/.
data HsGuardedAlt
    = HsGuardedAlt SrcLoc [HsStmt] HsExp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-----------------------------------------------------------------------------
-- Builtin names.

prelude_mod, main_mod :: Module
prelude_mod       = Module "Prelude"
main_mod          = Module "Main"

main_name :: HsName
main_name         = HsIdent "main"

unit_con_name :: HsQName
unit_con_name         = Special HsUnitCon

tuple_con_name :: Int -> HsQName
tuple_con_name i      = Special (HsTupleCon (i+1))

list_cons_name :: HsQName
list_cons_name        = Special HsCons

unit_con :: HsExp
unit_con          = HsCon unit_con_name

tuple_con :: Int -> HsExp
tuple_con i       = HsCon (tuple_con_name i)

as_name, qualified_name, hiding_name, minus_name, pling_name, dot_name, star_name :: HsName
as_name               = HsIdent "as"
qualified_name        = HsIdent "qualified"
hiding_name       = HsIdent "hiding"
minus_name        = HsSymbol "-"
pling_name        = HsSymbol "!"
dot_name          = HsSymbol "."
star_name             = HsSymbol "*"

export_name, safe_name, unsafe_name, threadsafe_name, stdcall_name, ccall_name :: HsName
export_name     = HsIdent "export"
safe_name       = HsIdent "safe"
unsafe_name     = HsIdent "unsafe"
threadsafe_name     = HsIdent "threadsafe"
stdcall_name        = HsIdent "stdcall"
ccall_name      = HsIdent "ccall"

unit_tycon_name, fun_tycon_name, list_tycon_name :: HsQName
unit_tycon_name       = unit_con_name
fun_tycon_name        = Special HsFunCon
list_tycon_name       = Special HsListCon

tuple_tycon_name :: Int -> HsQName
tuple_tycon_name i    = tuple_con_name i

unit_tycon, fun_tycon, list_tycon :: HsType
unit_tycon        = HsTyCon unit_tycon_name
fun_tycon         = HsTyCon fun_tycon_name
list_tycon        = HsTyCon list_tycon_name

tuple_tycon :: Int -> HsType
tuple_tycon i         = HsTyCon (tuple_tycon_name i)

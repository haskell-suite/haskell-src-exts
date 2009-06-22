{-
    This module is a temporary fix to use until Cabal
    supports XmlSyntax and RegularPatterns.
-}

module Language.Haskell.Exts.Extension (
    -- * Extensions
    Extension(..), classifyExtension, impliesExts,

    -- * Extension groups
    glasgowExts

    ) where

-- We want to import this list from cabal, but we're waiting for a version
-- with the xml-syntax and regular-patterns patch to be applied.
-- import Language.Haskell.Extension

{- This datatype should be imported from Cabal instead. -}
data Extension
  = OverlappingInstances
  | UndecidableInstances
  | IncoherentInstances
  | RecursiveDo
  | ParallelListComp
  | MultiParamTypeClasses
  | NoMonomorphismRestriction
  | FunctionalDependencies
  | Rank2Types
  | RankNTypes
  | PolymorphicComponents
  | ExistentialQuantification
  | ScopedTypeVariables
  | ImplicitParams
  | FlexibleContexts
  | FlexibleInstances
  | EmptyDataDecls
  | CPP

  | ExplicitForallTypes

  | KindSignatures
  | BangPatterns
  | TypeSynonymInstances
  | TemplateHaskell
  | ForeignFunctionInterface
  | Arrows
  | Generics
  | NoImplicitPrelude
  | NamedFieldPuns
  | PatternGuards
  | GeneralizedNewtypeDeriving

  | ExtensibleRecords
  | RestrictedTypeSynonyms
  | HereDocuments
  | MagicHash
  | TypeFamilies
  | StandaloneDeriving

  | UnicodeSyntax
  | PatternSignatures
  | UnliftedFFITypes
  | LiberalTypeSynonyms
  | TypeOperators
--PArr -- not ready yet, and will probably be renamed to ParallelArrays
  | RecordWildCards
  | RecordPuns
  | DisambiguateRecordFields
  | OverloadedStrings
  | GADTs
  | MonoPatBinds
  | RelaxedPolyRec
  | ExtendedDefaultRules
  | UnboxedTuples
  | DeriveDataTypeable
  | ConstrainedClassMethods

  -- | Allow imports to be qualified by the package name that the module
  -- is intended to be imported from, e.g.
  --
  -- > import "network" Network.Socket
  | PackageImports

  | ImpredicativeTypes
  | NewQualifiedOperators
  | PostfixOperators
  | QuasiQuotes
  | TransformListComp
  | ViewPatterns

  -- | Allow concrete XML syntax to be used in expressions and patterns,
  -- as per the Haskell Server Pages extension language:
  -- <http://www.haskell.org/haskellwiki/HSP>. The ideas behind it are
  -- discussed in the paper "Haskell Server Pages through Dynamic Loading"
  -- by Niklas Broberg, from Haskell Workshop '05.
  | XmlSyntax

  -- | Allow regular pattern matching over lists, as discussed in the
  -- paper "Regular Expression Patterns" by Niklas Broberg, Andreas Farre
  -- and Josef Svenningsson, from ICFP '04.
  | RegularPatterns
  deriving (Eq, Show, Read)
-- -}

impliesExts :: Extension -> [Extension]
impliesExts e@TypeFamilies = [e, KindSignatures]
impliesExts e | e `elem` [Rank2Types, RankNTypes, PolymorphicComponents, LiberalTypeSynonyms] = [e, ExplicitForallTypes]
impliesExts e = [e]

data ExtScheme = Any [Extension] | All [Extension]
  deriving (Eq,Show)

type MExtScheme = Maybe ExtScheme

-- | The list of extensions enabled by
--   GHC's portmanteau -fglasgow-exts flag.
glasgowExts :: [Extension]
glasgowExts = [
      ForeignFunctionInterface
    , UnliftedFFITypes
    , GADTs
    , ImplicitParams
    , ScopedTypeVariables
    , UnboxedTuples
    , TypeSynonymInstances
    , StandaloneDeriving
    , DeriveDataTypeable
    , FlexibleContexts
    , FlexibleInstances
    , ConstrainedClassMethods
    , MultiParamTypeClasses
    , FunctionalDependencies
    , MagicHash
    , PolymorphicComponents
    , ExistentialQuantification
    , UnicodeSyntax
    , PostfixOperators
    , PatternGuards
    , LiberalTypeSynonyms
    , RankNTypes
    , ImpredicativeTypes
    , TypeOperators
    , RecursiveDo
    , ParallelListComp
    , EmptyDataDecls
    , KindSignatures
    , GeneralizedNewtypeDeriving
    , TypeFamilies
    ]



-- | A clever version of read that returns an 'UnknownExtension'
--   if the string is not recognised.
classifyExtension :: String -> Extension
classifyExtension str
  = case readsPrec 0 str of
      [(e,"")]     -> e
      _            -> UnknownExtension str

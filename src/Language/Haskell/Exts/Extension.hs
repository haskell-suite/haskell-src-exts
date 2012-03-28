-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Extension
-- Copyright   :  (c) Niklas Broberg 2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  transient
-- Portability :  portable
--
-- This entire module should be replaced with
-- Language.Haskell.Extension from cabal, but we must
-- wait for a release of cabal that includes the
-- 'XmlSyntax' and 'RegularPatterns' extensions.
--
-----------------------------------------------------------------------------
module Language.Haskell.Exts.Extension (
    -- * Extensions
    Extension(..), classifyExtension, impliesExts,

    -- * Extension groups
    haskell98, haskell2010,

    glasgowExts, knownExtensions

    ) where


-- | This datatype is a copy of the one in Cabal's Language.Haskell.Extension module.
--   The intention is to eventually import it from Cabal, but we need to wait for
--   the next release of Cabal which includes XmlSyntax and RegularPatterns.
data Extension
  = OverlappingInstances
  | UndecidableInstances
  | IncoherentInstances
  | RecursiveDo
  | ParallelListComp
  | MultiParamTypeClasses
  | NoMonomorphismRestriction
  | FunctionalDependencies
  | ExplicitForAll
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
  | RecordWildCards
  | RecordPuns -- should be deprecated
  | DisambiguateRecordFields
  | OverloadedStrings
  | GADTs
  | MonoPatBinds
  | NoMonoPatBinds -- should be deprecated
  | RelaxedPolyRec
  | ExtendedDefaultRules
  | UnboxedTuples
  | DeriveDataTypeable
  | ConstrainedClassMethods
  | NPlusKPatterns

  | PackageImports
  | DoAndIfThenElse

  | ImpredicativeTypes
  | NewQualifiedOperators
  | PostfixOperators
  | QuasiQuotes
  | TransformListComp
  | ViewPatterns

  | XmlSyntax

  | RegularPatterns

  | TupleSections

  | UnknownExtension String
  deriving (Eq, Ord, Show, Read)


-- | Certain extensions imply other extensions, and this function
--   makes the implication explicit. This also handles deprecated
--   extensions, which imply their replacements.
--   The returned valued is the transitive closure of implied
--   extensions.
impliesExts :: [Extension] -> [Extension]
impliesExts = go
  where go [] = []
        go es = let xs = concatMap implE es
                    ys = filter (not . flip elem es) xs
                 in es ++ go ys

        implE e = case e of
                    TypeFamilies        -> [KindSignatures]
                    ScopedTypeVariables -> [TypeOperators, ExplicitForAll]
                    XmlSyntax           -> [RegularPatterns]
                    RegularPatterns     -> [PatternGuards]
                    RankNTypes          -> [Rank2Types, ExplicitForAll]
                    Rank2Types          -> [PolymorphicComponents, ExplicitForAll]
                    PolymorphicComponents   -> [ExplicitForAll]
                    LiberalTypeSynonyms -> [ExplicitForAll]
                    ExistentialQuantification -> [ExplicitForAll]
                    -- Deprecations
                    RecordPuns          -> [NamedFieldPuns]
                    PatternSignatures   -> [ScopedTypeVariables]
                    e                   -> []

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

haskell98 :: [Extension]
haskell98 = [NPlusKPatterns]

haskell2010 :: [Extension]
haskell2010 =
    [ DoAndIfThenElse
    , PatternGuards
    , ForeignFunctionInterface
    , EmptyDataDecls 
    ]

-- | List of all known extensions. Poor man's 'Enum' instance
--   (we can't enum with the 'UnknownExtension' constructor).
knownExtensions :: [Extension]
knownExtensions = 
    [ OverlappingInstances
    , UndecidableInstances
    , IncoherentInstances
    , RecursiveDo
    , ParallelListComp
    , MultiParamTypeClasses
    , NoMonomorphismRestriction
    , FunctionalDependencies
    , ExplicitForAll
    , Rank2Types
    , RankNTypes
    , PolymorphicComponents
    , ExistentialQuantification
    , ScopedTypeVariables
    , ImplicitParams
    , FlexibleContexts
    , FlexibleInstances
    , EmptyDataDecls
    , CPP
    , KindSignatures
    , BangPatterns
    , TypeSynonymInstances
    , TemplateHaskell
    , ForeignFunctionInterface
    , Arrows
    , Generics
    , NoImplicitPrelude
    , NamedFieldPuns
    , PatternGuards
    , GeneralizedNewtypeDeriving
    , ExtensibleRecords
    , RestrictedTypeSynonyms
    , HereDocuments
    , MagicHash
    , TypeFamilies
    , StandaloneDeriving
    , UnicodeSyntax
    , PatternSignatures
    , UnliftedFFITypes
    , LiberalTypeSynonyms
    , TypeOperators
    , RecordWildCards
    , RecordPuns -- should be deprecated
    , DisambiguateRecordFields
    , OverloadedStrings
    , GADTs
    , MonoPatBinds
    , NoMonoPatBinds -- should be deprecated
    , RelaxedPolyRec
    , ExtendedDefaultRules
    , UnboxedTuples
    , DeriveDataTypeable
    , ConstrainedClassMethods
    , PackageImports
    , ImpredicativeTypes
    , NewQualifiedOperators
    , PostfixOperators
    , QuasiQuotes
    , TransformListComp
    , ViewPatterns
    , XmlSyntax
    , RegularPatterns
    , TupleSections
    , NPlusKPatterns
    , DoAndIfThenElse
    ]



-- | A clever version of read that returns an 'UnknownExtension'
--   if the string is not recognised.
classifyExtension :: String -> Extension
classifyExtension str
  = case readsPrec 0 str of
      [(e,"")]     -> e
      _            -> UnknownExtension str

{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
module InfixTypeImportExport (type (:+:)(..)) where

import TypeOperatorDefinitions (type (:+:)(..), type (:+:)((:*:)))

data a :+: b = Mk a b

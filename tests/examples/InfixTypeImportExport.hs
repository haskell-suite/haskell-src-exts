{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
module InfixTypeImportExport
    ( type (:+:)(..)
    , type (***)(..)
    , type MyType(..)
    , (:+:)(..)
    ) where

import TypeOperatorDefinitions
    ( type (:+:)(..)
    , type (:+:)((:*:))
    , type (***)(..)
    , type (<)((:*:))
    , type MyType(..)
    , (:+:)(..)
    )

data a :+: b = Mk a b
data a *** b = Prod a b
data MyType a = MyType a

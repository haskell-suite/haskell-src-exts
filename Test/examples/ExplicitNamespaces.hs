{-# LANGUAGE ExplicitNamespaces #-}
import Data.Array.Repa ( type (:.)((:.)) )
import Data.Array.Repa ( type {- comment here -} (:.)((:.)) )
import Data.Array.Repa (    type {- comment2 here -} (:.)((:.))  {- and here -} )

-- these probably should be rejected?
import Data.Array.Repa ( type type (:.)((:.)) )
import Data.Array.Repa ( type type type (:.)((:.)) )

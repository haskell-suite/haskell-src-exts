{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, GADTs #-}
module Foo where

-- Test cases
data Vec (n :: Nat) where { VCons :: (m ~ (n-1)) => Double -> Vec m -> Vec n }
data Vec (n :: Nat) where { VCons :: ((n-1) ~ m) => Double -> Vec m -> Vec n }
data Vec (n :: Nat) where { VCons :: ((m+1) ~ n) => Double -> Vec m -> Vec n }
data Vec (n :: Nat) where { VCons :: (n ~ (m+1)) => Double -> Vec m -> Vec n }

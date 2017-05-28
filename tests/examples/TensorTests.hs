{-# LANGUAGE ConstraintKinds, FlexibleContexts, DataKinds, NoImplicitPrelude,
             RebindableSyntax, ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module TensorTests (tensorTests) where

type TMRParams = ( '(,) <$> Tensors) <*> MRCombos
tmrParams :: Proxy TMRParams
tmrParams = Proxy

--type ExtParams = ( '(,) <$> Tensors) <*> MRExtCombos
type TrEmParams = ( '(,) <$> Tensors) <*> MM'RCombos
tremParams :: Proxy TrEmParams
tremParams = Proxy

type NormParams = ( '(,) <$> '[RT]) <*> (Filter Liftable MRCombos)

--data Liftable :: TyFun (Factored, *) Bool -> *
type instance Apply Liftable '(m,zq) = Int64 :== (LiftOf zq)


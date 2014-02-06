{-# LANGUAGE DataKinds #-}

data Foo a = Foo

type FooB = Foo "a"

type FooC = Foo 1

instance Show (Foo "bar") where

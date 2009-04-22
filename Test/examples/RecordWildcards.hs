{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

data Foo = Foo {a :: Int, b :: Int}

foo Foo{b, ..} = a

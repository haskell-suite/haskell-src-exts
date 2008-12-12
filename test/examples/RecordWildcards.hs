{-# LANGUAGE RecordWildCards #-}

data Foo = Foo {a :: Int}

foo Foo{..} = a

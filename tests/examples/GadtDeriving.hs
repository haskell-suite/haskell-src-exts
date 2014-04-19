{-# LANGUAGE GADTs #-}

data Foo where
    Foo :: Int -> Foo
    deriving (Eq,Ord,Typeable)

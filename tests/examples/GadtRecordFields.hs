{-# LANGUAGE GADTs #-}

data Ty where
  TCon :: { field1 :: Int, field2 :: Bool } -> Ty
  TCon2 :: Ty

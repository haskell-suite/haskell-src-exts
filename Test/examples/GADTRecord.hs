{-# LANGUAGE GADTs #-}

data T where
  T :: { field :: Int } -> T

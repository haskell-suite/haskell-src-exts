{-# LANGUAGE GADTs #-}

data X where
  X :: !Int -> X

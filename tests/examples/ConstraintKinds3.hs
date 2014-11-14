{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class Foo c a where

  bar :: (c a) => a -> a


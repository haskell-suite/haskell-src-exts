{-# LANGUAGE ScopedTypeVariables #-}
module ForallInInstance where

instance forall a. MyClass a => MyClass [a] where

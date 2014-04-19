{-# LANGUAGE RankNTypes #-}
module GenericTree where

import Data.Typeable

dynRep :: (Typeable a) => a -> (TypeRep, forall b. (Typeable b) => b -> (Maybe b))
dynRep a = (typeOf a, \_ -> cast a)

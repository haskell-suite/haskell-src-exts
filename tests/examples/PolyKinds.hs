{-# LANGUAGE PolyKinds #-}
data Proxy t = Proxy

class Typeable t where
  typeOf :: Proxy t -> TypeRep

instance Typeable Int  where typeOf _ = TypeRep
instance Typeable []   where typeOf _ = TypeRep

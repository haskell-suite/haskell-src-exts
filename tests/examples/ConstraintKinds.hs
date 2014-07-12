{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
class Foo a where

data Bar a
type Bazable a b = (Bar a ~ Maybe b)

baz :: Bazable a b => a -> a
baz = id

{-# LANGUAGE ConstraintKinds #-}
class Foo a where 

type Bar a
type Bazable a b = (Bar a ~ Maybe b)

baz :: Bazable a b => a -> a
baz = id

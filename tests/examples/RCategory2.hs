{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

import GHC.Prim

class RCategory cat where
  type RCategoryCtxt cat a b :: Constraint

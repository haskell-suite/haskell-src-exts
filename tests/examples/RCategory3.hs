{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

instance RCategory (->) where
  type RCategoryCtxt (->) a a = ()


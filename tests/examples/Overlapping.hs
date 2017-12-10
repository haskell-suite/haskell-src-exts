{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Overlapping where

class C a b where
  f :: a -> b -> Bool

instance C a b where
  f _ _ = False

instance {-# OVERLAPPING #-} C a a where
  f _ _ = True

instance {-# OVERLAPS #-} C a a where
  f _ _ = True

instance {-# OVERLAPPABLE #-} C a a where
  f _ _ = True

-- >>> f 'a' 'b'
-- True
--
-- >>> f 'a' "starfish"
-- False

\begin{code}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
module Control.RCategory where
  

import qualified Prelude

import GHC.Prim

infixr 9 .
infixr 1 >>>, <<<

-- | A class for categories.
--   id and (.) must form a monoid.
class RCategory cat where
  type RCategoryCtxt cat a b :: Constraint
  
  -- | the identity morphism
  id :: RCategoryCtxt cat a a 
     => cat a a

  -- | morphism composition
  (.) :: (RCategoryCtxt cat b c, RCategoryCtxt cat a b, RCategoryCtxt cat a c) 
      => cat b c -> cat a b -> cat a c

{-# RULES
"identity/left" forall p .
                id . p = p
"identity/right"        forall p .
                p . id = p
 #-}

instance RCategory (->) where
  type RCategoryCtxt (->) a a = ()
  id = Prelude.id
  (.) = (Prelude..)

-- | Right-to-left composition
(<<<) :: (RCategoryCtxt cat a c, RCategoryCtxt cat a b, RCategoryCtxt cat b c, RCategory cat)
      => cat b c -> cat a b -> cat a c
(<<<) = (.)

-- | Left-to-right composition
(>>>) :: (RCategoryCtxt cat a c, RCategoryCtxt cat a b, RCategoryCtxt cat b c, RCategory cat)
      => cat a b -> cat b c -> cat a c
f >>> g = g . f
\end{code}
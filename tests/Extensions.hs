{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Extensions (extensionProperties) where

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import Language.Haskell.Exts
import Data.Function
import Data.List

instance Monad m => Serial m Language where
  series = generate (const knownLanguages)

instance Monad m => Serial m Extension where
  series = generate (const knownExtensions)

instance Monad m => Serial m KnownExtension where
  series = generate $ const [ e | EnableExtension e <- knownExtensions ]

infix 3 ~~
(~~) :: Monad m => [Extension] -> [Extension] -> Property m
xts1 ~~ xts2 = forAll $ \lang -> ((==) `on` sort . toExtensionList lang) xts1 xts2

-- arbitrary examples
e1 :: KnownExtension
e1 = OverlappingInstances

e2 :: KnownExtension
e2 = UndecidableInstances

extensionProperties :: TestTree
extensionProperties =
  localOption (SmallCheckDepth 2) $ testGroup "Properties of LANGUAGE extensions" $
  [ testProperty "identity" $ \x -> x ~~ x
  , testProperty "idempotence" $ \x -> x ++ x ~~ x
  -- Actually, let's not exhaustively check n^2 cases
  -- , testProperty "right bias" $ \x y -> x ++ y ++ x ~~ y ++ x
  , testProperty "right bias - override to disabled" $
      [EnableExtension e1, DisableExtension e1] ~~ [DisableExtension e1]
  , testProperty "right bias - override to enabled" $
      [DisableExtension e1, EnableExtension e1] ~~ [EnableExtension e1]
  , testProperty "right bias - interleaved override to disabled" $
      [EnableExtension e1, EnableExtension e2, DisableExtension e1]
        ~~ [EnableExtension e2, DisableExtension e1]
  , testProperty "right bias - interleaved override to enabled" $
      [DisableExtension e1, EnableExtension e2, EnableExtension e1]
        ~~ [EnableExtension e2, EnableExtension e1]
  , testProperty "closedness of implication" $ \x -> impliesExts (impliesExts x) == impliesExts x
  , testProperty "closedness of toExtensionList" $ \l x -> let es = toExtensionList l x in es == impliesExts es
  , testProperty "opposite extensions 1" $ \x -> [EnableExtension x, DisableExtension x] ~~ [DisableExtension x]
  , testProperty "opposite extensions 2" $ \x -> [DisableExtension x, EnableExtension x] ~~ [EnableExtension x]
  ]

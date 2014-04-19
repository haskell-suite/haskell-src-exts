{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving #-}
module Extensions (extensionProperties) where

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series
import Language.Haskell.Exts.Annotated
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

extensionProperties =
  localOption (SmallCheckDepth 2) $ testGroup "Properties of LANGUAGE extensions" $
  [ testProperty "identity" $ \x -> x ~~ x
  , testProperty "idempotence" $ \x -> x ++ x ~~ x
  , testProperty "right bias" $ \x y -> x ++ y ++ x ~~ y ++ x
  , testProperty "closedness of implication" $ \x -> impliesExts (impliesExts x) == impliesExts x
  , testProperty "closedness of toExtensionList" $ \l x -> let es = toExtensionList l x in es == impliesExts es
  , testProperty "opposite extensions 1" $ \x -> [EnableExtension x, DisableExtension x] ~~ [DisableExtension x]
  , testProperty "opposite extensions 2" $ \x -> [DisableExtension x, EnableExtension x] ~~ [EnableExtension x]
  ]

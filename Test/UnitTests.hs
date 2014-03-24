{-# LANGUAGE StandaloneDeriving #-}
module UnitTests where

import Test.Tasty
import Test.Tasty.HUnit
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc

deriving instance Eq a => Eq (ParseResult a)

unitTests = testGroup "Unit tests"
  [ -- http://trac.haskell.org/haskell-src-exts/ticket/189
    testCase "Ambiguous fixities" $ parseExp "(+ 1) . head &&& tail" @?= ParseFailed noLoc "Ambiguous infix expression"
  ]

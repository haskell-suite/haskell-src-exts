module Invariants (invariantTests) where


import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Language.Haskell.Exts


invariantTests :: TestTree
invariantTests = testGroup "Unit tests"
    [ testCase "Pretty printing with layout = PPInLine" $ do
        let decl = "h x = f (filter ('a' ==) x)"
        let ParseOk x = parseDecl decl
        assertEqual "ParseDecl is inverse of prettyPrint"
            decl (prettyPrintWithMode (defaultMode { layout = PPInLine }) x)
    ]
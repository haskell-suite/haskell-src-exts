import Distribution.Simple
import Test.Runner ( go )
main = defaultMainWithHooks $ simpleUserHooks { runTests = \args _ _ _ -> go args }

import Distribution.Simple
--import Test.Runner ( go )
main = defaultMain --WithHooks $ simpleUserHooks { runTests = \args _ _ _ -> go args }

{-# LANGUAGE QuasiQuotes #-}

[undefined|
this quasiquote is acceptable to ghc's
parser. Previously hse would reject this
because -XTemplateHaskell is not enabled
to allow "expressions" at top-level
|]

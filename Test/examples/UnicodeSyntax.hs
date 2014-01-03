{-# LANGUAGE UnicodeSyntax, ExplicitForAll #-}
module UnicodeSyntax where

import System.Environment (getArgs)

main ∷ IO ()
main = do
  as ← getArgs
  print $ test 0

test ∷ Int → Bool
test x = x*5 == x+8

id1 ∷ ∀ a . a → a
id1 x = x

{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Prelude.Unicode

dumy ∷ Double → Double → Double
dumy a b =
    let a² = a ⋅ a
        b² = b ⋅ b
    in sqrt $ a² + b²

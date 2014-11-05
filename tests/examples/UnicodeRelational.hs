{-# LANGUAGE UnicodeSyntax #-}

import Prelude.Unicode

testAnd ∷ Int → Int → Bool
testAnd a b =
    let b₁ = a ≤ 3
        b₂ = b ≥ 5
    in b₁ ∧ b₂

testOr ∷ Int → Int → Int → Int → Int → Bool
testOr a b c d e =
    let b₁ = a ≤ 3
        b₂ = b ≢ 5
        b₃ = c ≮ 5
        b₄ = d ≯ 6
        b₅ = e ∈ [1,2,3,4]
        b₆ = e ∉ [5,6,7,8]
    in b₁ ∨ b₂ ∨ b₃ ∨ b₄ ∨ b₅ ∨ b₆

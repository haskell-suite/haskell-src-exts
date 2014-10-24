{-# LANGUAGE UnicodeSyntax #-}
module Main where

data Point = Point
    { pointX, pointY ∷ Double
    , name           ∷ String
    } deriving (Show)

distance ∷ Point → Point → Double
distance x y =
    let t₁ = (pointX x - pointX y)^2
        t₂ = (pointY x - pointY y)^2
    in sqrt $ t₁ + t₂

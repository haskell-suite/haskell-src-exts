{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.Environment

main :: IO ()
main = do
  as ← getArgs
  print $ as
  print $ test                0
  print $ test2               0
  print $ testRewrite         0
  print $ testRewriteReverse  0
  print $ testRewrite2        0
  print $ testRewriteReverse2 0

test :: a → Bool
test x = pi
  where
    f  = replicate 2000 x
    i  = repeat         x
    pf = f |> 300
    pi = i |> 300

test2 :: a → (Bool,Bool)
test2 x = (pf,pi)
  where
    f  = replicate 2000 x
    i  = repeat         x
    pf = f |> 300
    pi = i |> 300

testRewrite :: a → Bool
testRewrite x = pi
  where
    f  = replicate 2000 x
    i  = repeat         x
    lf = length f
    li = length i
    pf = lf > 300
    pi = li > 300

testRewriteReverse :: a → Bool
testRewriteReverse x = pi
  where
    f  = replicate 2000 x
    i  = repeat         x
    lf = length f
    li = length i
    pf = 300 ≤ lf
    pi = 300 ≤ li

testRewrite2 :: a → (Bool,Bool)
testRewrite2 x = (length i > 300,300 > length i)
  where
--    f  = replicate 2000 x
    i  = repeat         x
--    lf = length f
--    li = length i
--    pf = lf > 300
--    pi = li > 300

testRewriteReverse2 :: a → (Bool,Bool)
testRewriteReverse2 x = (2000 < length i,length i > 20)
  where
    f  = replicate 2000 x
    i  = repeat         x
    lf = length f
    li = length i
    pf = 2000 == lf
    pi = lf ≥ li


lengthOP :: (Num a, Ord a) ⇒ Bool → (a → a → Bool) → [b] → a → Bool
lengthOP v (⊜) []  n = 0 ⊜ n
lengthOP v (⊜) xxs n = co xxs 0
  where
    co (_:xs) c | n > c     = co xs (c+1)
                | otherwise = v
    co []     c = c ⊜ n

(≣) = (==)
(≤) = (<=)
(≥) = (>=)

(|≣)  = lengthOP False (≣)
(|<)  = lengthOP False (<)
(|≤)  = lengthOP False (≤)
(|>)  = lengthOP True  (>)
(|≥)  = lengthOP True  (≥)

(|=)  = lengthOP False (==)
(|==) = lengthOP False (==)
(|<=) = lengthOP False (<=)
(|>=) = lengthOP False (>=)


-- ≣≤≥
(≣|) = flip (|≣)
(<|) = flip (|≥)
(≤|) = flip (|>)
(>|) = flip (|≤)
(≥|) = flip (|<)

{-# RULES
-- length
"xs |≣ n" forall xs n.  (length xs) == n = xs |≣ n
"xs |< n" forall xs n.  (length xs) <  n = xs |< n
"xs |≤ n" forall xs n.  (length xs) <= n = xs |≤ n
"xs |> n" forall xs n.  (length xs) >  n = xs |> n
"xs |≥ n" forall xs n.  (length xs) >= n = xs |≥ n

"n ≣| xs" forall xs n.  n == (length xs) = xs |≣ n
"n <| xs" forall xs n.  n <  (length xs) = xs |≥ n
"n ≤| xs" forall xs n.  n <= (length xs) = xs |> n
"n >| xs" forall xs n.  n >  (length xs) = xs |≤ n
"n ≥| xs" forall xs n.  n >= (length xs) = xs |< n
  #-}

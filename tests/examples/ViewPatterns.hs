{-# LANGUAGE ViewPatterns #-}
f (id -> Just _) "" = ""

g (id -> True) = False


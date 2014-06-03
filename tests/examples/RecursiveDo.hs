{-# LANGUAGE RecursiveDo #-}
justOnes = do
    rec xs <- Just (1:xs)
    return (map negate xs)


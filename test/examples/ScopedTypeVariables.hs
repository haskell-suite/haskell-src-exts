{-# LANGUAGE ScopedTypeVariables #-}

test :: IO Char
test = do
    x :: Char <- getChar
    return x

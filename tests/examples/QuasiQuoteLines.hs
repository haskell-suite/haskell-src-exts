{-# LANGUAGE QuasiQuotes #-}

import Here

str :: String
str = [here|test 
test 
test test |]


main :: IO()
main = do putStrLn str

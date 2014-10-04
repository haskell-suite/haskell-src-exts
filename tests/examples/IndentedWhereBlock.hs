module Graph where

 countryLookUp :: String -> Graph -> Maybe Int
 countryLookUp country graph = indexOf country graph where

 indexOf :: String -> Graph -> Maybe Int
 indexOf _ Empty = Nothing

{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}

ins :: (:=>) b h => b :- h

data (:><:) a b = (:><:) a b

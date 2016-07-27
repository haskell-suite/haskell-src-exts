{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

data a :+: b = Proxy

foo = id @(Int :+: Int)

_ @> m = m

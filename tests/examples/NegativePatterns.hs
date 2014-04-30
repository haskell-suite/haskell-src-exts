f 1 = -1
f (-1) = 1
f ( - 2) = 2
f (  -  3) = 3

data Z a = Higher a a | Same a a | Lower a a

infixr 7 `Higher`
infixr 6 `Same`
infixr 5 `Lower`

g :: Z Int -> ()
g (   -1 `Higher`  x) = ()
g (  -  2 `Same`   x) = ()
g ( -    3 `Lower` x) = ()

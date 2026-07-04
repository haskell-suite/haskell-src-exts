-- Missing NumericUnderscores extension. Should fail.
f :: Integer -> ()
f 1_000 = ()
f _   = ()

-- Missing hex float literals extension. Should fail.
f :: Float -> ()
f 0xFF.FFp12 = ()
f _   = ()

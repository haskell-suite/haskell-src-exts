-- Missing binary literals extension. Should fail.
f :: Int -> ()
f 0b0 = ()
f _   = ()

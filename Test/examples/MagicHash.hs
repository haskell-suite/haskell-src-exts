{-# LANGUAGE MagicHash #-}

import GHC.Exts

putByteArray :: Int# -> Int#
putByteArray s# = s# +# 1#

floatFoo :: Float# -> Float
floatFoo f# = F# (f# `plusFloat#` 1.2e2#)

charFun :: Char# -> Bool
charFun c# = c# `gtChar#` 'a'#
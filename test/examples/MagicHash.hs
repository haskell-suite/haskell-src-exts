{-# LANGUAGE MagicHash #-}

import GHC.Exts

putByteArray :: Int# -> Int#
putByteArray s# = s# +# 1#

{-# LANGUAGE Arrows #-}
module ArrowLayout where

exp = proc () -> do
  rec let e = 1 + i
      i <- integral -< e
  returnA -< e

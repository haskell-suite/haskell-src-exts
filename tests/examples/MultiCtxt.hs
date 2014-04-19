{-# LANGUAGE RankNTypes #-}

module MultiCtxt where

multipleCtx :: Eq a => (Show a => a)
multipleCtx = undefined

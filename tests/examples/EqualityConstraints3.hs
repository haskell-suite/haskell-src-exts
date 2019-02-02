{-# LANGUAGE GADTs, FlexibleContexts #-}

one :: (~) a Int => a
one = 1

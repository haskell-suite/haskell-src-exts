{-# LANGUAGE GADTs #-}

one :: (~) a Int => a
one = 1

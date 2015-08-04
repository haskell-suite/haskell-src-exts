{-|
Module      : W
Description : Short description
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module HaddockComments where

-- | Function1 comment
fun1
   :: Int      -- ^ The 'Int' argument
   -> Float    -- ^ The 'Float' argument
   -> IO ()    -- ^ The return value
fun1 = undefined


-- not a haddock comment
fun2 = undefined

fun3 :: Int -> Int
-- ^ Function3 comment
fun3 = undefined

{-|
  The 'square' function squares an integer.
  It takes one argument, of type 'Int'.
-}
square :: Int -> Int
square x = x * x -- beware!

class C a where
   -- | This is the documentation for the 'f' method
   f :: a -> Int
   -- | This is the documentation for the 'g' method
   g :: Int -> a

-- | Data type comment
-- With a second line
data MyData =
  -- | Constructor1 comment
  Cons1
    { cons1Field1 :: Int -- ^ Constructor 1 field 1 comment
                         -- spanning two lines
      -- | Constructor 1 field 2 comment
    , cons1Field2 :: Int
    , cons1Field3 :: String -- Not a haddock comment
    }
  | Cons2 -- ^ Constructor 2 comment
      Int -- ^ Last

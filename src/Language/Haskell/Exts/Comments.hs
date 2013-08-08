{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Language.Haskell.Exts.Comments where

import Language.Haskell.Exts.SrcLoc

#ifdef __GLASGOW_HASKELL__
#ifdef BASE4
import Data.Data
#else
import Data.Generics (Data(..),Typeable(..))
#endif
#endif

-- | A Haskell comment. The 'Bool' is 'True' if the comment is multi-line, i.e. @{- -}@.
data Comment = Comment Bool SrcSpan String
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Language.Haskell.Exts.Annotated.Comments where

import Language.Haskell.Exts.Annotated.SrcLoc

#ifdef __GLASGOW_HASKELL__
#ifdef BASE4
import Data.Data
#else
import Data.Generics (Data(..),Typeable(..))
#endif
#endif

data Comment = Comment Bool SrcSpan String
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

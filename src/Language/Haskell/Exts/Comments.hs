{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Exts.Comments where

import Language.Haskell.Exts.SrcLoc
import Data.Data

-- | A Haskell comment. The 'Bool' is 'True' if the comment is multi-line, i.e. @{- -}@.
data Comment = Comment Bool SrcSpan String
  deriving (Eq,Show,Typeable,Data)

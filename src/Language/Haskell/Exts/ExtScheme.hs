{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.ExtScheme
-- Copyright   :  (c) Niklas Broberg 2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Internal scheme for handling extensions in a
-- convenient fashion.
--
-----------------------------------------------------------------------------
module Language.Haskell.Exts.ExtScheme where

import Language.Haskell.Exts.Extension

data ExtScheme = Any [KnownExtension] | All [KnownExtension]
  deriving (Eq,Show)

type MExtScheme = Maybe ExtScheme

class Enabled a where
  isEnabled :: a -> [KnownExtension] -> Bool

instance Enabled KnownExtension where
  isEnabled = elem

instance Enabled ExtScheme where
  isEnabled (Any exts) enabled = any (`elem` enabled) exts
  isEnabled (All exts) enabled = all (`elem` enabled) exts

instance Enabled a => Enabled (Maybe a) where
  isEnabled Nothing  = const True
  isEnabled (Just a) = isEnabled a

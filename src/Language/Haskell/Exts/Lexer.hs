-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Lexer
-- Copyright   :  (c) The GHC Team, 1997-2000
--                (c) Niklas Broberg, 2004-2012
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, niklas.broberg@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Lexer for Haskell with extensions.
--
-----------------------------------------------------------------------------

module Language.Haskell.Exts.Lexer
    (
      lexTokenStream, lexTokenStreamWithMode,

      Token(..), Loc(..),

      showToken

    ) where

import Language.Haskell.Exts.InternalLexer
import Language.Haskell.Exts.ParseMonad
import Language.Haskell.Exts.SrcLoc

-- | Lex a string into a list of Haskell 2010 source tokens.
lexTokenStream :: String -> ParseResult [Loc Token]
lexTokenStream = lexTokenStreamWithMode defaultParseMode

-- | Lex a string into a list of Haskell source tokens, using an explicit mode.
lexTokenStreamWithMode :: ParseMode -> String -> ParseResult [Loc Token]
lexTokenStreamWithMode mode = runParserWithMode mode lexIt
  where lexIt :: P [Loc Token]
        lexIt = runL go return
        go :: Lex [Loc Token] [Loc Token]
        go = do
          ltok <- topLexer
          case ltok of
            Loc _ EOF -> return []
            _ -> do ts <- go
                    return (ltok:ts)

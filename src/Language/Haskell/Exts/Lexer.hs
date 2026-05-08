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
      lexTokenStreamText, lexTokenStreamTextWithMode,

      Token(..), Loc(..),

      showToken

    ) where

import Language.Haskell.Exts.InternalLexer
import Language.Haskell.Exts.ParseMonad
import Language.Haskell.Exts.SrcLoc
import Data.Text (Text)

-- | Lex a string into a list of Haskell 2010 source tokens.
lexTokenStream :: String -> ParseResult [Loc Token]
lexTokenStream = lexTokenStreamWithMode defaultParseMode

-- | Lex a string into a list of Haskell source tokens, using an explicit mode.
lexTokenStreamWithMode :: ParseMode -> String -> ParseResult [Loc Token]
lexTokenStreamWithMode mode = runParserWithMode mode lexIt

-- | 'Text'-input variant of 'lexTokenStream'.  Skips the eager 'T.pack'
-- that the 'String' entry point performs at the boundary.
lexTokenStreamText :: Text -> ParseResult [Loc Token]
lexTokenStreamText = lexTokenStreamTextWithMode defaultParseMode

-- | 'Text'-input variant of 'lexTokenStreamWithMode'.
lexTokenStreamTextWithMode :: ParseMode -> Text -> ParseResult [Loc Token]
lexTokenStreamTextWithMode mode = fmap fst . runParserWithModeCommentsText mode lexIt

lexIt :: P [Loc Token]
lexIt = runL go return
  where go :: Lex [Loc Token] [Loc Token]
        go = do ltok <- topLexer
                case ltok of
                  Loc _ EOF -> return []
                  _         -> do ts <- go
                                  return (ltok:ts)

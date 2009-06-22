-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.ParseMonad
-- Copyright   :  (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Monads for the Haskell parser and lexer.
--
-----------------------------------------------------------------------------

module Language.Haskell.Exts.ParseMonad(
        -- * Parsing
        P, ParseResult(..), atSrcLoc, LexContext(..),
        ParseMode(..), defaultParseMode,
        runParserWithMode, runParser,
        getSrcLoc, pushCurrentContext, popContext,
        getExtensions,
        -- * Lexing
        Lex(runL), getInput, discard, lexNewline, lexTab, lexWhile,
        alternative, checkBOL, setBOL, startToken, getOffside,
        pushContextL, popContextL, getExtensionsL,
        -- * Harp/Hsx
        ExtContext(..),
        pushExtContextL, popExtContextL, getExtContext,
        pullCtxtFlag, flagDo,
        getModuleName
    ) where

import Language.Haskell.Exts.Syntax(SrcLoc(..))
import Language.Haskell.Exts.Extension (Extension)
import Language.Haskell.Exts.Fixity (Fixity, preludeFixities)

import Data.List ( intersperse )
import Control.Applicative
import Control.Monad (when)
import Data.Monoid

-- | The result of a parse.
data ParseResult a
    = ParseOk a  -- ^ The parse succeeded, yielding a value.
    | ParseFailed SrcLoc String
                -- ^ The parse failed at the specified
                -- source location, with an error message.
    deriving Show

-- | Retrieve the result of a successful parse, throwing an
--   error if the parse is actually not successful.
fromParseResult :: ParseResult a -> a
fromParseResult (ParseOk a) = a
fromParseResult (ParseFailed loc str) = error $ "fromParseResult: Parse failed: " ++ str

instance Functor ParseResult where
  fmap f (ParseOk x)           = ParseOk $ f x
  fmap f (ParseFailed loc msg) = ParseFailed loc msg

instance Applicative ParseResult where
  pure = ParseOk
  ParseOk f           <*> x = f <$> x
  ParseFailed loc msg <*> _ = ParseFailed loc msg

instance Monad ParseResult where
  return = ParseOk
  ParseOk x           >>= f = f x
  ParseFailed loc msg >>= _ = ParseFailed loc msg

instance Monoid m => Monoid (ParseResult m) where
  mempty = ParseOk mempty
  ParseOk x `mappend` ParseOk y = ParseOk $ x `mappend` y
  ParseOk x `mappend` err       = err
  err       `mappend` _         = err -- left-biased


-- internal version
data ParseStatus a = Ok ParseState a | Failed SrcLoc String
    deriving Show

data LexContext = NoLayout | Layout Int
    deriving (Eq,Ord,Show)

data ExtContext = CodeCtxt | HarpCtxt | TagCtxt | ChildCtxt
        | CloseTagCtxt | CodeTagCtxt
    deriving (Eq,Ord,Show)

type CtxtFlag = (Bool,Bool)
-- (True,_) = We're in a do context.
-- (_, True)= Next token must be a virtual closing brace.

type ParseState = ([LexContext],[ExtContext],CtxtFlag)

indentOfParseState :: ParseState -> Int
indentOfParseState (Layout n:_,_,_) = n
indentOfParseState _                = 0

-- | Static parameters governing a parse.
-- More to come later, e.g. literate mode, language extensions.

data ParseMode = ParseMode {
        -- | original name of the file being parsed
        parseFilename :: String,
        -- | list of extensions enabled
        extensions :: [Extension],
        -- | if @True@, the parser won't look for further extensions
        -- in LANGUAGE pragmas in source files
        ignoreLanguagePragmas :: Bool,
        -- | list of fixities to be aware of
        fixities :: [Fixity]
        }

-- | Default parameters for a parse,
-- currently just a marker for an unknown filename.

defaultParseMode :: ParseMode
defaultParseMode = ParseMode {
        parseFilename = "<unknown>.hs",
        extensions = [],
        ignoreLanguagePragmas = False,
        fixities = preludeFixities
        }

-- | Monad for parsing

newtype P a = P { runP ::
                String      -- input string
             -> Int     -- current column
             -> Int     -- current line
             -> SrcLoc      -- location of last token read
             -> ParseState  -- layout info.
             -> ParseMode   -- parse parameters
             -> ParseStatus a
        }

runParserWithMode :: ParseMode -> P a -> String -> ParseResult a
runParserWithMode mode (P m) s = case m s 0 1 start ([],[],(False,False)) mode of
    Ok _ a -> ParseOk a
    Failed loc msg -> ParseFailed loc msg
    where start = SrcLoc {
        srcFilename = parseFilename mode,
        srcLine = 1,
        srcColumn = 1
    }

runParser :: P a -> String -> ParseResult a
runParser = runParserWithMode defaultParseMode

instance Monad P where
    return a = P $ \_i _x _y _l s _m -> Ok s a
    P m >>= k = P $ \i x y l s mode ->
        case m i x y l s mode of
            Failed loc msg -> Failed loc msg
            Ok s' a -> runP (k a) i x y l s' mode
    fail s = P $ \_r _col _line loc _stk _m -> Failed loc s

atSrcLoc :: P a -> SrcLoc -> P a
P m `atSrcLoc` loc = P $ \i x y _l -> m i x y loc

getSrcLoc :: P SrcLoc
getSrcLoc = P $ \_i _x _y l s _m -> Ok s l

getModuleName :: P String
getModuleName = P $ \_i _x _y _l s m ->
    let fn = parseFilename m
        mn = concat $ intersperse "." $ splitPath fn

        splitPath :: String -> [String]
        splitPath ""   = []
        splitPath str  = let (l,str') = break ('\\'==) str
                          in case str' of
                              []      -> [removeSuffix l]
                              (_:str'') -> l : splitPath str''

        removeSuffix l = reverse $ tail $ dropWhile ('.'/=) $ reverse l

     in Ok s mn

-- Enter a new layout context.  If we are already in a layout context,
-- ensure that the new indent is greater than the indent of that context.
-- (So if the source loc is not to the right of the current indent, an
-- empty list {} will be inserted.)

pushCurrentContext :: P ()
pushCurrentContext = do
    lc <- getSrcLoc
    indent <- currentIndent
    dob <- pullDoStatus
    let loc = srcColumn lc
    when (dob && loc < indent
           || not dob && loc <= indent) $ pushCtxtFlag
    pushContext (Layout loc)

currentIndent :: P Int
currentIndent = P $ \_r _x _y loc stk _mode -> Ok stk (indentOfParseState stk)

pushContext :: LexContext -> P ()
pushContext ctxt =
--trace ("pushing lexical scope: " ++ show ctxt ++"\n") $
    P $ \_i _x _y _l (s, e, p) _m -> Ok (ctxt:s, e, p) ()

popContext :: P ()
popContext = P $ \_i _x _y _l stk _m ->
      case stk of
        (_:s, e, p) -> --trace ("popping lexical scope, context now "++show s ++ "\n") $
                       Ok (s, e, p) ()
        ([],_,_)    -> error "Internal error: empty context in popContext"


-- HaRP/Hsx
pushExtContext :: ExtContext -> P ()
pushExtContext ctxt = P $ \_i _x _y _l (s, e, p) _m -> Ok (s, ctxt:e, p) ()

popExtContext :: P ()
popExtContext = P $ \_i _x _y _l (s, e, p) _m ->
    case e of
     (_:e') ->
       Ok (s, e', p) ()
     [] -> error "Internal error: empty context in popExtContext"


-- Extension-aware lexing/parsing
getExtensions :: P [Extension]
getExtensions = P $ \_i _x _y _l s m ->
    Ok s $ extensions m

pushCtxtFlag :: P ()
pushCtxtFlag =
    P $ \_i _x _y _l (s, e, (d,c)) _m -> case c of
        False -> Ok (s, e, (d,True)) ()
        _     -> error "Internal error: context flag already pushed"

pullDoStatus :: P Bool
pullDoStatus = P $ \_i _x _y _l (s, e, (d,c)) _m -> Ok (s,e,(False,c)) d


----------------------------------------------------------------------------
-- Monad for lexical analysis:
-- a continuation-passing version of the parsing monad

newtype Lex r a = Lex { runL :: (a -> P r) -> P r }

instance Monad (Lex r) where
    return a = Lex $ \k -> k a
    Lex v >>= f = Lex $ \k -> v (\a -> runL (f a) k)
    Lex v >> Lex w = Lex $ \k -> v (\_ -> w k)
    fail s = Lex $ \_ -> fail s

-- Operations on this monad

getInput :: Lex r String
getInput = Lex $ \cont -> P $ \r -> runP (cont r) r

-- | Discard some input characters (these must not include tabs or newlines).

discard :: Int -> Lex r ()
discard n = Lex $ \cont -> P $ \r x -> runP (cont ()) (drop n r) (x+n)

-- | Discard the next character, which must be a newline.

lexNewline :: Lex a ()
lexNewline = Lex $ \cont -> P $ \(_:r) _x y -> runP (cont ()) r 1 (y+1)

-- | Discard the next character, which must be a tab.

lexTab :: Lex a ()
lexTab = Lex $ \cont -> P $ \(_:r) x -> runP (cont ()) r (nextTab x)

nextTab :: Int -> Int
nextTab x = x + (tAB_LENGTH - (x-1) `mod` tAB_LENGTH)

tAB_LENGTH :: Int
tAB_LENGTH = 8 :: Int

-- Consume and return the largest string of characters satisfying p

lexWhile :: (Char -> Bool) -> Lex a String
lexWhile p = Lex $ \cont -> P $ \r x ->
    let (cs,rest) = span p r in
    runP (cont cs) rest (x + length cs)

-- An alternative scan, to which we can return if subsequent scanning
-- is unsuccessful.

alternative :: Lex a v -> Lex a (Lex a v)
alternative (Lex v) = Lex $ \cont -> P $ \r x y ->
    runP (cont (Lex $ \cont' -> P $ \_r _x _y ->
        runP (v cont') r x y)) r x y

-- The source location is the coordinates of the previous token,
-- or, while scanning a token, the start of the current token.

-- col is the current column in the source file.
-- We also need to remember between scanning tokens whether we are
-- somewhere at the beginning of the line before the first token.
-- This could be done with an extra Bool argument to the P monad,
-- but as a hack we use a col value of 0 to indicate this situation.

-- Setting col to 0 is used in two places: just after emitting a virtual
-- close brace due to layout, so that next time through we check whether
-- we also need to emit a semi-colon, and at the beginning of the file,
-- by runParser, to kick off the lexer.
-- Thus when col is zero, the true column can be taken from the loc.

checkBOL :: Lex a Bool
checkBOL = Lex $ \cont -> P $ \r x y loc ->
        if x == 0 then runP (cont True) r (srcColumn loc) y loc
            else runP (cont False) r x y loc

setBOL :: Lex a ()
setBOL = Lex $ \cont -> P $ \r _ -> runP (cont ()) r 0

-- Set the loc to the current position

startToken :: Lex a ()
startToken = Lex $ \cont -> P $ \s x y _ stk mode ->
    let loc = SrcLoc {
        srcFilename = parseFilename mode,
        srcLine = y,
        srcColumn = x
    } in
    runP (cont ()) s x y loc stk mode

-- Current status with respect to the offside (layout) rule:
-- LT: we are to the left of the current indent (if any)
-- EQ: we are at the current indent (if any)
-- GT: we are to the right of the current indent, or not subject to layout

getOffside :: Lex a Ordering
getOffside = Lex $ \cont -> P $ \r x y loc stk ->
        runP (cont (compare x (indentOfParseState stk))) r x y loc stk

pushContextL :: LexContext -> Lex a ()
pushContextL ctxt = Lex $ \cont -> P $ \r x y loc (stk, e, pst) ->
        runP (cont ()) r x y loc (ctxt:stk, e, pst)

popContextL :: String -> Lex a ()
popContextL fn = Lex $ \cont -> P $ \r x y loc stk -> case stk of
        (_:ctxt, e, pst) -> runP (cont ()) r x y loc (ctxt, e, pst)
        ([], _, _)       -> error ("Internal error: empty context in " ++ fn)

pullCtxtFlag :: Lex a Bool
pullCtxtFlag = Lex $ \cont -> P $ \r x y loc (ct, e, (d,c)) ->
        runP (cont c) r x y loc (ct, e, (d,False))


flagDo :: Lex a ()
flagDo = Lex $ \cont -> P $ \r x y loc (ct, e, (d,c)) ->
        runP (cont ()) r x y loc (ct, e, (True,c))


-- Harp/Hsx

getExtContext :: Lex a (Maybe ExtContext)
getExtContext = Lex $ \cont -> P $ \r x y loc stk@(_, e, _) ->
        let me = case e of
              [] -> Nothing
              (c:_) -> Just c
        in runP (cont me) r x y loc stk

pushExtContextL :: ExtContext -> Lex a ()
pushExtContextL ec = Lex $ \cont -> P $ \r x y loc (s, e, p) ->
        runP (cont ()) r x y loc (s, ec:e, p)

popExtContextL :: String -> Lex a ()
popExtContextL fn = Lex $ \cont -> P $ \r x y loc stk@(s,e,p) -> case e of
            (_:ec) -> runP (cont ()) r x y loc (s,ec,p)
            []       -> error ("Internal error: empty tag context in " ++ fn)


-- Extension-aware lexing

getExtensionsL :: Lex a [Extension]
getExtensionsL = Lex $ \cont -> P $ \r x y loc s m ->
        runP (cont $ extensions m) r x y loc s m

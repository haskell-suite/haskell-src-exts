-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.ParseMonad
-- Original    :  Language.Haskell.ParseMonad
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
		-- * Lexing
		Lex(runL), getInput, discard, lexNewline, lexTab, lexWhile,
		alternative, checkBOL, setBOL, startToken, getOffside,
		pushContextL, popContextL,
		-- * Harp/Hsx
		ExtContext(..),
		pushExtContextL, popExtContextL, getExtContext,
		getModuleName
	) where

import Language.Haskell.Exts.Syntax(SrcLoc(..))

import Data.List ( intersperse )

-- | The result of a parse.
data ParseResult a
	= ParseOk a		-- ^ The parse succeeded, yielding a value.
	| ParseFailed SrcLoc String
				-- ^ The parse failed at the specified
				-- source location, with an error message.
	deriving Show

-- internal version
data ParseStatus a = Ok ParseState a | Failed SrcLoc String
	deriving Show

data LexContext = NoLayout | Layout Int
	deriving (Eq,Ord,Show)
	
data ExtContext = CodeCtxt | HarpCtxt | TagCtxt | ChildCtxt 
		| CloseTagCtxt | CodeTagCtxt
	deriving (Eq,Ord,Show)

type ParseState = ([LexContext],[ExtContext])

indentOfParseState :: ParseState -> Int
indentOfParseState (Layout n:_,_) = n
indentOfParseState _            = 0

-- | Static parameters governing a parse.
-- More to come later, e.g. literate mode, language extensions.

data ParseMode = ParseMode {
		-- | original name of the file being parsed
		parseFilename :: String
		}

-- | Default parameters for a parse,
-- currently just a marker for an unknown filename.

defaultParseMode :: ParseMode
defaultParseMode = ParseMode {
		parseFilename = "<unknown>.hs"
		}

-- | Monad for parsing

newtype P a = P { runP ::
		        String		-- input string
		     -> Int		-- current column
		     -> Int		-- current line
		     -> SrcLoc		-- location of last token read
		     -> ParseState	-- layout info.
		     -> ParseMode	-- parse parameters
		     -> ParseStatus a
		}

runParserWithMode :: ParseMode -> P a -> String -> ParseResult a
runParserWithMode mode (P m) s = case m s 0 1 start ([],[]) mode of
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
	loc <- getSrcLoc
	indent <- currentIndent
	pushContext (Layout (srcColumn loc))

currentIndent :: P Int
currentIndent = P $ \_r _x _y loc stk _mode -> Ok stk (indentOfParseState stk)

pushContext :: LexContext -> P ()
pushContext ctxt =
--trace ("pushing lexical scope: " ++ show ctxt ++"\n") $
	P $ \_i _x _y _l (s, e) _m -> Ok (ctxt:s, e) ()

popContext :: P ()
popContext = P $ \_i _x _y _l stk _m ->
      case stk of
   	(_:s, e) -> --trace ("popping lexical scope, context now "++show s ++ "\n") $
            Ok (s, e) ()
        ([],_)    -> error "Internal error: empty context in popContext"


-- HaRP/Hsx
pushExtContext :: ExtContext -> P ()
pushExtContext ctxt = P $ \_i _x _y _l (s, e) _m -> Ok (s, ctxt:e) ()

popExtContext :: P ()
popExtContext = P $ \_i _x _y _l (s, e) _m ->
	case e of
	 (_:e') -> 
	   Ok (s, e') ()
	 [] -> error "Internal error: empty context in popExtContext"


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
pushContextL ctxt = Lex $ \cont -> P $ \r x y loc (stk, e) ->
		runP (cont ()) r x y loc (ctxt:stk, e)

popContextL :: String -> Lex a ()
popContextL fn = Lex $ \cont -> P $ \r x y loc stk -> case stk of
		(_:ctxt, e) -> runP (cont ()) r x y loc (ctxt, e)
		([], _)     -> error ("Internal error: empty context in " ++ fn)

-- Harp/Hsx

getExtContext :: Lex a (Maybe ExtContext)
getExtContext = Lex $ \cont -> P $ \r x y loc stk@(_, e) ->
		let me = case e of
			  [] -> Nothing
			  (c:_) -> Just c
		in runP (cont me) r x y loc stk

pushExtContextL :: ExtContext -> Lex a ()
pushExtContextL ec = Lex $ \cont -> P $ \r x y loc (s, e) ->
		runP (cont ()) r x y loc (s, ec:e)

popExtContextL :: String -> Lex a ()
popExtContextL fn = Lex $ \cont -> P $ \r x y loc stk@(s,e) -> case e of
			(_:ec) -> runP (cont ()) r x y loc (s,ec)
			[]       -> error ("Internal error: empty tag context in " ++ fn)

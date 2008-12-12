-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Lexer
-- Copyright   :  (c) The GHC Team, 1997-2000
--        (c) Niklas Broberg, 2004
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, d00nibro@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Lexer for Haskell, with some extensions.
--
-----------------------------------------------------------------------------

-- ToDo: Introduce different tokens for decimal, octal and hexadecimal (?)
-- ToDo: FloatTok should have three parts (integer part, fraction, exponent) (?)
-- ToDo: Use a lexical analyser generator (lx?)

module Language.Haskell.Exts.Lexer (Token(..), lexer) where

import Language.Haskell.Exts.ParseMonad

import Data.Char
import Data.Ratio

data Token
        = VarId String
        | QVarId (String,String)
        | IDupVarId (String)        -- duplicable implicit parameter
        | ILinVarId (String)        -- linear implicit parameter
        | ConId String
        | QConId (String,String)
        | DVarId [String]       -- to enable varid's with '-' in them
        | VarSym String
        | ConSym String
        | QVarSym (String,String)
        | QConSym (String,String)
        | IntTok Integer
        | FloatTok Rational
        | Character Char
        | StringTok String

-- Symbols

        | LeftParen
        | RightParen
        | LeftHashParen
        | RightHashParen
        | SemiColon
        | LeftCurly
        | RightCurly
        | VRightCurly           -- a virtual close brace
        | LeftSquare
        | RightSquare
        | Comma
        | Underscore
        | BackQuote

-- Reserved operators

        | Dot           -- reserved for use with 'forall x . x'
        | DotDot
        | Colon
        | DoubleColon
        | Equals
        | Backslash
        | Bar
        | LeftArrow
        | RightArrow
        | At
        | Tilde
        | DoubleArrow
        | Minus
        | Exclamation
        | Star
    
-- Template Haskell
        | THExpQuote        -- [| or [e|
        | THPatQuote        -- [p|
        | THDecQuote        -- [d|
        | THTypQuote        -- [t|         
        | THCloseQuote      -- |]
        | THIdEscape (String)   -- dollar x
        | THParenEscape     -- dollar ( 
        | THVarQuote        -- 'x (but without the x)
        | THTyQuote         -- ''T (but without the T)

-- HaRP
        | RPGuardOpen       -- (|
        | RPGuardClose      -- |)
        | RPCAt             -- @:

-- Hsx
        | XCodeTagOpen      -- <%
        | XCodeTagClose     -- %>
        | XStdTagOpen       -- <
        | XStdTagClose      -- >
        | XCloseTagOpen     -- </
        | XEmptyTagClose    -- />
        | XPCDATA String
        | XRPatOpen             -- <[
        | XRPatClose            -- ]>
        
-- Pragmas

        | PragmaEnd                     -- #-}
        | PragmaUnknown (String,String)   -- Any pragma not recognized
        | RULES
        | INLINE Bool
        | SPECIALISE
        | SPECIALISE_INLINE Bool
        | SOURCE
        | DEPRECATED
        | WARNING
        | SCC
        | GENERATED
        | CORE
        | UNPACK
        | OPTIONS (Maybe String,String)
        | CFILES  String
        | LANGUAGE
        | INCLUDE String
-- These are not yet implemented
--        | LINE

-- Reserved Ids

        | KW_As
        | KW_Case
        | KW_Class
        | KW_Data
        | KW_Default
        | KW_Deriving
        | KW_Do
        | KW_MDo
        | KW_Else
        | KW_Family     -- indexed type families
        | KW_Forall     -- universal/existential types
        | KW_Hiding
        | KW_If
        | KW_Import
        | KW_In
        | KW_Infix
        | KW_InfixL
        | KW_InfixR
        | KW_Instance
        | KW_Let
        | KW_Module
        | KW_NewType
        | KW_Of
        | KW_Then
        | KW_Type
        | KW_Where
        | KW_Qualified

                -- FFI
        | KW_Foreign
        | KW_Export
        | KW_Safe
        | KW_Unsafe
        | KW_Threadsafe
        | KW_StdCall
        | KW_CCall

        | EOF
        deriving (Eq,Show)

reserved_ops :: [(String,Token)]
reserved_ops = [
 ( ".",  Dot ),
 ( "..", DotDot ),
 ( ":",  Colon ),
 ( "::", DoubleColon ),
 ( "=",  Equals ),
 ( "\\", Backslash ),
 ( "|",  Bar ),
 ( "<-", LeftArrow ),
 ( "->", RightArrow ),
 ( "@",  At ),
 ( "~",  Tilde ),
 ( "=>", DoubleArrow ),
 ( "*",  Star )
 ]

special_varops :: [(String,Token)]
special_varops = [
 ( "-",  Minus ),           --ToDo: shouldn't be here
 ( "!",  Exclamation )      --ditto
 ]

reserved_ids :: [(String,Token)]
reserved_ids = [
 ( "_",         Underscore ),
 ( "case",      KW_Case ),
 ( "class",     KW_Class ),
 ( "data",      KW_Data ),
 ( "default",   KW_Default ),
 ( "deriving",  KW_Deriving ),
 ( "do",        KW_Do ),
 ( "else",      KW_Else ),
 ( "family",    KW_Family ),        -- indexed type families
 ( "forall",    KW_Forall ),        -- universal/existential quantification
 ( "if",        KW_If ),
 ( "import",    KW_Import ),
 ( "in",        KW_In ),
 ( "infix",     KW_Infix ),
 ( "infixl",    KW_InfixL ),
 ( "infixr",    KW_InfixR ),
 ( "instance",  KW_Instance ),
 ( "let",       KW_Let ),
 ( "mdo",       KW_MDo ),
 ( "module",    KW_Module ),
 ( "newtype",   KW_NewType ),
 ( "of",        KW_Of ),
 ( "then",      KW_Then ),
 ( "type",      KW_Type ),
 ( "where",     KW_Where ),

-- FFI
 ( "foreign",   KW_Foreign )
 ]


special_varids :: [(String,Token)]
special_varids = [
 ( "as",        KW_As ),
 ( "qualified", KW_Qualified ),
 ( "hiding",    KW_Hiding ),

-- FFI
 ( "export",     KW_Export),
 ( "safe",       KW_Safe),
 ( "unsafe",     KW_Unsafe),
 ( "threadsafe", KW_Threadsafe),
 ( "stdcall",    KW_StdCall),
 ( "ccall",      KW_CCall)
 ]

pragmas :: [(String,Token)]
pragmas = [
 ( "rules",             RULES           ),
 ( "inline",            INLINE True     ),
 ( "noinline",          INLINE False    ),
 ( "notinline",         INLINE False    ),
 ( "specialise",        SPECIALISE      ),
 ( "specialize",        SPECIALISE      ),
 ( "source",            SOURCE          ),
 ( "deprecated",        DEPRECATED      ),
 ( "warning",           WARNING         ),
 ( "scc",               SCC             ),
 ( "generated",         GENERATED       ),
 ( "core",              CORE            ),
 ( "unpack",            UNPACK          ),
 ( "language",          LANGUAGE        ),
 ( "options",           OPTIONS undefined ), -- we'll tweak it before use - promise!
 ( "cfiles",            CFILES  undefined ), -- same here...
 ( "include",           INCLUDE undefined )  -- ...and here!
 ]

isIdent, isHSymbol :: Char -> Bool
isIdent   c = isAlpha c || isDigit c || c == '\'' || c == '_'

isHSymbol c = c `elem` ":!#%&*./?@\\-" || ((isSymbol c || isPunctuation c) && not (c `elem` "(),;[]`{}_\"'"))

matchChar :: Char -> String -> Lex a ()
matchChar c msg = do
    s <- getInput
    if null s || head s /= c then fail msg else discard 1

-- The top-level lexer.
-- We need to know whether we are at the beginning of the line to decide
-- whether to insert layout tokens.

lexer :: (Token -> P a) -> P a
lexer = runL $ do
    bol <- checkBOL
    (bol, ws) <- lexWhiteSpace bol
    -- take care of whitespace in PCDATA
    ec <- getExtContext
    case ec of
     -- if there was no linebreak, and we are lexing PCDATA,
     -- then we want to care about the whitespace
     Just ChildCtxt | not bol && ws -> return $ XPCDATA " "
     _ -> do startToken
             if bol then lexBOL else lexToken 

lexWhiteSpace :: Bool -> Lex a (Bool, Bool)
lexWhiteSpace bol = do
    s <- getInput
    case s of
        '{':'-':'#':_ -> do
            return (bol, False)
        '{':'-':_ -> do
            discard 2
            bol <- lexNestedComment bol
            (bol, _) <- lexWhiteSpace bol
            return (bol, True)
        '-':'-':s | all (== '-') (takeWhile isHSymbol s) -> do
            lexWhile (== '-')
            lexWhile (/= '\n')
            s' <- getInput
            case s' of
                [] -> fail "Unterminated end-of-line comment"
                _ -> do
                    lexNewline
                    lexWhiteSpace True
                    return (True, True)
        '\n':_ -> do
            lexNewline
            lexWhiteSpace True
            return (True, True)
        '\t':_ -> do
            lexTab
            (bol, _) <- lexWhiteSpace bol
            return (bol, True)      
        c:_ | isSpace c -> do
            discard 1
            (bol, _) <- lexWhiteSpace bol
            return (bol, True)
        _ -> return (bol, False)

lexNestedComment :: Bool -> Lex a Bool
lexNestedComment bol = do
    s <- getInput
    case s of
        '-':'}':_ -> discard 2 >> return bol
        '{':'-':_ -> do
            discard 2
            bol <- lexNestedComment bol -- rest of the subcomment
            lexNestedComment bol        -- rest of this comment
        '\t':_    -> lexTab >> lexNestedComment bol
        '\n':_    -> lexNewline >> lexNestedComment True
        _:_       -> discard 1 >> lexNestedComment bol
        []        -> fail "Unterminated nested comment"

-- When we are lexing the first token of a line, check whether we need to
-- insert virtual semicolons or close braces due to layout.

lexBOL :: Lex a Token
lexBOL = do
    pos <- getOffside
    case pos of
        LT -> do
                -- trace "layout: inserting '}'\n" $
            -- Set col to 0, indicating that we're still at the
            -- beginning of the line, in case we need a semi-colon too.
            -- Also pop the context here, so that we don't insert
            -- another close brace before the parser can pop it.
            setBOL
            popContextL "lexBOL"
            return VRightCurly
        EQ ->
            -- trace "layout: inserting ';'\n" $
            return SemiColon
        GT -> lexToken

lexToken :: Lex a Token
lexToken = do
    ec <- getExtContext
    case ec of
     Just HarpCtxt     -> lexHarpToken
     Just TagCtxt      -> lexTagCtxt
     Just CloseTagCtxt -> lexCloseTagCtxt
     Just ChildCtxt    -> lexChildCtxt
     Just CodeTagCtxt  -> lexCodeTagCtxt
     _         -> lexStdToken


lexChildCtxt :: Lex a Token
lexChildCtxt = do
    s <- getInput
    case s of
        '<':'%':_ -> do discard 2
                        pushExtContextL CodeTagCtxt
                        return XCodeTagOpen
        '<':'/':_ -> do discard 2
                        popExtContextL "lexChildCtxt"
                        pushExtContextL CloseTagCtxt
                        return XCloseTagOpen
        '<':'[':_ -> do discard 2
                        pushExtContextL HarpCtxt
                        return XRPatOpen
        '<':_     -> do discard 1
                        pushExtContextL TagCtxt
                        return XStdTagOpen
        _     -> lexPCDATA


lexPCDATA :: Lex a Token
lexPCDATA = do
    s <- getInput
    case s of
        [] -> return EOF
        _  -> case s of
            '\n':_ -> do
                x <- lexNewline >> lexPCDATA
                case x of
                 XPCDATA p -> return $ XPCDATA $ '\n':p
                 EOF -> return EOF
            '<':_ -> return $ XPCDATA ""
            _ -> do let pcd = takeWhile (\c -> not $ elem c "<\n") s
                        l = length pcd
                    discard l
                    x <- lexPCDATA
                    case x of
                     XPCDATA pcd' -> return $ XPCDATA $ pcd ++ pcd'
                     EOF -> return EOF


lexCodeTagCtxt :: Lex a Token
lexCodeTagCtxt = do
    s <- getInput
    case s of
        '%':'>':_ -> do discard 2
                        popExtContextL "lexCodeTagContext"
                        return XCodeTagClose
        _     -> lexStdToken

lexCloseTagCtxt :: Lex a Token
lexCloseTagCtxt = do
    s <- getInput
    case s of
        '>':_     -> do discard 1
                        popExtContextL "lexCloseTagCtxt"
                        return XStdTagClose
        _     -> lexStdToken

lexTagCtxt :: Lex a Token
lexTagCtxt = do
    s <- getInput
    case s of
        '/':'>':_ -> do discard 2
                        popExtContextL "lexTagCtxt: Empty tag"
                        return XEmptyTagClose
        '>':_     -> do discard 1
                        popExtContextL "lexTagCtxt: Standard tag"
                        pushExtContextL ChildCtxt
                        return XStdTagClose
        _     -> lexStdToken

lexHarpToken :: Lex a Token
lexHarpToken = do
    s <- getInput
    case s of
        ']':'>':_ -> do discard 2
                        popExtContextL "lexHarpToken"
                        return XRPatClose
        _     -> lexStdToken

lexStdToken :: Lex a Token
lexStdToken = do
    s <- getInput
    case s of
        [] -> return EOF

        '0':c:d:_ | toLower c == 'o' && isOctDigit d -> do
                        discard 2
                        n <- lexOctal
                        return (IntTok n)
                  | toLower c == 'x' && isHexDigit d -> do
                        discard 2
                        n <- lexHexadecimal
                        return (IntTok n)
    
        -- implicit parameters
        '?':c:_ | isLower c -> do
                        discard 1
                        id <- lexWhile isIdent
                        return $ IDupVarId id

        '%':c:_ | isLower c -> do
                        discard 1
                        id <- lexWhile isIdent
                        return $ ILinVarId id
        -- end implicit parameters

        -- harp
        '(':'|':c:_  | isHSymbol c -> discard 1 >> return LeftParen
        '(':'|':_ -> do discard 2
                        return RPGuardOpen
        '|':')':_ -> do discard 2
                        return RPGuardClose
        '@':':':_ -> do discard 2
                        return RPCAt
    
        -- template haskell
        '[':'|':_ -> do
                discard 2
                return $ THExpQuote
    
        '[':c:'|':_ | c == 'e' -> do
                        discard 3
                        return $ THExpQuote
                    | c == 'p' -> do
                        discard 3
                        return THPatQuote
                    | c == 'd' -> do
                        discard 3
                        return THDecQuote
                    | c == 't' -> do
                        discard 3
                        return THTypQuote
                
        '|':']':_ -> do discard 2
                        return THCloseQuote
              
        '$':c:_ | isLower c -> do
                        discard 1
                        id <- lexWhile isIdent
                        return $ THIdEscape id
                | c == '(' -> do
                        discard 2
                        return THParenEscape
        -- end template haskell
    
        -- hsx
        '<':'%':_ -> do discard 2
                        pushExtContextL CodeTagCtxt
                        return XCodeTagOpen
        '<':c:_ | isAlpha c -> do discard 1
                                  pushExtContextL TagCtxt
                                  return XStdTagOpen
        -- end hsx
    
        '(':'#':_ -> do discard 2 >> return LeftHashParen
    
        '#':')':_ -> do discard 2 >> return RightHashParen
        
        -- pragmas
        
        '{':'-':'#':_ -> do discard 3 >> lexPragmaStart
        
        '#':'-':'}':_ -> do discard 3 >> return PragmaEnd
    
        c:_ | isDigit c -> lexDecimalOrFloat

            | isUpper c -> lexConIdOrQual ""

            | isLower c || c == '_' -> do
                    idents <- lexIdents
                    case idents of
                     [ident] -> return $ case lookup ident (reserved_ids ++ special_varids) of
                                          Just keyword -> keyword
                                          Nothing -> VarId ident
                     _ -> return $ DVarId idents

            | isHSymbol c -> do
                    sym <- lexWhile isHSymbol
                    return $ case lookup sym (reserved_ops ++ special_varops) of
                              Just t  -> t
                              Nothing -> case c of
                                          ':' -> ConSym sym
                                          _   -> VarSym sym

            | otherwise -> do
                    discard 1
                    case c of

                        -- First the special symbols
                        '(' ->  return LeftParen
                        ')' ->  return RightParen
                        ',' ->  return Comma
                        ';' ->  return SemiColon
                        '[' ->  return LeftSquare
                        ']' ->  return RightSquare
                        '`' ->  return BackQuote
                        '{' -> do
                            pushContextL NoLayout
                            return LeftCurly
                        '}' -> do
                            popContextL "lexStdToken"
                            return RightCurly

                        '\'' -> lexCharacter
                        '"' ->  lexString

                        _ ->    fail ("Illegal character \'" ++ show c ++ "\'\n")

      where lexIdents :: Lex a [String]
            lexIdents = do
                ident <- lexWhile isIdent
                s <- getInput
                case s of
                 '-':c:_ | isAlpha c -> do 
                        discard 1
                        idents <- lexIdents
                        return $ ident : idents
                 '#':_ -> do
                        discard 1
                        return [ident ++ "#"]
                 _ -> return [ident]


lexPragmaStart :: Lex a Token
lexPragmaStart = do
    lexWhile isSpace
    pr <- lexWhile isAlphaNum
    case lookup (map toLower pr) pragmas of
     Just SPECIALISE -> do
            s <- getInput
            case dropWhile isSpace $ map toLower s of
             'i':'n':'l':'i':'n':'e':_ -> do
                      lexWhile isSpace
                      discard 6
                      return $ SPECIALISE_INLINE True
             'n':'o':'i':'n':'l':'i':'n':'e':_ -> do
                        lexWhile isSpace
                        discard 8
                        return $ SPECIALISE_INLINE False
             'n':'o':'t':'i':'n':'l':'i':'n':'e':_ -> do
                        lexWhile isSpace
                        discard 9
                        return $ SPECIALISE_INLINE False
             _ -> return SPECIALISE
              
     Just (OPTIONS _) -> do     -- see, I promised we'd mask out the 'undefined'
            s <- getInput
            case s of
             '_':_  -> do
                discard 1
                com <- lexWhile isIdent
                rest <- lexRawPragma
                return $ OPTIONS (Just com, rest)
             x:_ | isSpace x -> do
                rest <- lexRawPragma
                return $ OPTIONS (Nothing, rest)
             _ -> fail "Malformed Options pragma"
     Just (CFILES _) -> do
            rest <- lexRawPragma
            return $ CFILES rest
     Just (INCLUDE _) -> do
            rest <- lexRawPragma
            return $ INCLUDE rest
     Just p ->  return p

     _      -> do rawStr <- lexRawPragma
                  return $ PragmaUnknown (pr, rawStr)

lexRawPragma :: Lex a String
lexRawPragma = do
    rpr <- lexRawPragmaAux
    return $ dropWhile isSpace rpr
 where lexRawPragmaAux = do
        rpr <- lexWhile (/='#')
        s <- getInput
        case s of
         '#':'-':'}':_  -> return rpr
         _ -> do 
            discard 1
            rpr' <- lexRawPragma
            return $ rpr ++ '#':rpr'

lexDecimalOrFloat :: Lex a Token
lexDecimalOrFloat = do
    ds <- lexWhile isDigit
    rest <- getInput
    case rest of
        ('.':d:_) | isDigit d -> do
                discard 1
                frac <- lexWhile isDigit
                let num = parseInteger 10 (ds ++ frac)
                    decimals = toInteger (length frac)
                exponent <- do
                    rest2 <- getInput
                    case rest2 of
                        'e':_ -> lexExponent
                        'E':_ -> lexExponent
                        _     -> return 0
                return (FloatTok ((num%1) * 10^^(exponent - decimals)))
        e:_ | toLower e == 'e' -> do
                exponent <- lexExponent
                return (FloatTok ((parseInteger 10 ds%1) * 10^^exponent))
        _ -> return (IntTok (parseInteger 10 ds))

    where
    lexExponent :: Lex a Integer
    lexExponent = do
        discard 1   -- 'e' or 'E'
        r <- getInput
        case r of
         '+':d:_ | isDigit d -> do
            discard 1
            lexDecimal
         '-':d:_ | isDigit d -> do
            discard 1
            n <- lexDecimal
            return (negate n)
         d:_ | isDigit d -> lexDecimal
         _ -> fail "Float with missing exponent"

lexConIdOrQual :: String -> Lex a Token
lexConIdOrQual qual = do
        con <- lexWhile isIdent
        let conid | null qual = ConId con
                  | otherwise = QConId (qual,con)
            qual' | null qual = con
                  | otherwise = qual ++ '.':con
        just_a_conid <- alternative (return conid)
        rest <- getInput
        case rest of
          '.':c:_
             | isLower c || c == '_' -> do  -- qualified varid?
                    discard 1
                    ident <- lexWhile isIdent
                    s <- getInput
                    ident' <- case s of
                               '#':_ -> discard 1 >> return (ident ++ "#")
                               _ -> return ident
                    case lookup ident' reserved_ids of
                       -- cannot qualify a reserved word
                       Just _  -> just_a_conid
                       Nothing -> return (QVarId (qual', ident'))

             | isUpper c -> do      -- qualified conid?
                    discard 1
                    lexConIdOrQual qual'

             | isHSymbol c -> do    -- qualified symbol?
                    discard 1
                    sym <- lexWhile isHSymbol
                    case lookup sym reserved_ops of
                        -- cannot qualify a reserved operator
                        Just _  -> just_a_conid
                        Nothing -> return $ case c of
                                              ':' -> QConSym (qual', sym)
                                              _   -> QVarSym (qual', sym)

          '#':c:_ 
            | isSpace c -> do
                discard 1
                case conid of
                 ConId con -> return $ ConId $ con ++ "#"
                 QConId (q,con) -> return $ QConId (q,con ++ "#")
          _ ->  return conid -- not a qualified thing

lexCharacter :: Lex a Token
lexCharacter = do   -- We need to keep track of not only character constants but also TH 'x and ''T
        -- We've seen ' so far
        s <- getInput
        case s of
         '\'':_ -> discard 1 >> return THTyQuote
         '\\':_ -> do 
                    c <- lexEscape 
                    matchQuote
                    return (Character c)
         c:'\'':_ -> discard 2 >> return (Character c)
         _ -> return THVarQuote                    

    where matchQuote = matchChar '\'' "Improperly terminated character constant"


lexString :: Lex a Token
lexString = loop ""
    where
    loop s = do
        r <- getInput
        case r of
            '\\':'&':_ -> do
                    discard 2
                    loop s
            '\\':c:_ | isSpace c -> do
                        discard 1
                        lexWhiteChars
                        matchChar '\\' "Illegal character in string gap"
                        loop s
                     | otherwise -> do
                        ce <- lexEscape
                        loop (ce:s)
            '"':_ -> do
                discard 1
                return (StringTok (reverse s))
            c:_ -> do
                discard 1
                loop (c:s)
            [] ->   fail "Improperly terminated string"

    lexWhiteChars :: Lex a ()
    lexWhiteChars = do
        s <- getInput
        case s of
            '\n':_ -> do
                    lexNewline
                    lexWhiteChars
            '\t':_ -> do
                    lexTab
                    lexWhiteChars
            c:_ | isSpace c -> do
                    discard 1
                    lexWhiteChars
            _ -> return ()

lexEscape :: Lex a Char
lexEscape = do
    discard 1
    r <- getInput
    case r of

-- Production charesc from section B.2 (Note: \& is handled by caller)

        'a':_           -> discard 1 >> return '\a'
        'b':_           -> discard 1 >> return '\b'
        'f':_           -> discard 1 >> return '\f'
        'n':_           -> discard 1 >> return '\n'
        'r':_           -> discard 1 >> return '\r'
        't':_           -> discard 1 >> return '\t'
        'v':_           -> discard 1 >> return '\v'
        '\\':_          -> discard 1 >> return '\\'
        '"':_           -> discard 1 >> return '\"'
        '\'':_          -> discard 1 >> return '\''

-- Production ascii from section B.2

        '^':c:_         -> discard 2 >> cntrl c
        'N':'U':'L':_   -> discard 3 >> return '\NUL'
        'S':'O':'H':_   -> discard 3 >> return '\SOH'
        'S':'T':'X':_   -> discard 3 >> return '\STX'
        'E':'T':'X':_   -> discard 3 >> return '\ETX'
        'E':'O':'T':_   -> discard 3 >> return '\EOT'
        'E':'N':'Q':_   -> discard 3 >> return '\ENQ'
        'A':'C':'K':_   -> discard 3 >> return '\ACK'
        'B':'E':'L':_   -> discard 3 >> return '\BEL'
        'B':'S':_       -> discard 2 >> return '\BS'
        'H':'T':_       -> discard 2 >> return '\HT'
        'L':'F':_       -> discard 2 >> return '\LF'
        'V':'T':_       -> discard 2 >> return '\VT'
        'F':'F':_       -> discard 2 >> return '\FF'
        'C':'R':_       -> discard 2 >> return '\CR'
        'S':'O':_       -> discard 2 >> return '\SO'
        'S':'I':_       -> discard 2 >> return '\SI'
        'D':'L':'E':_   -> discard 3 >> return '\DLE'
        'D':'C':'1':_   -> discard 3 >> return '\DC1'
        'D':'C':'2':_   -> discard 3 >> return '\DC2'
        'D':'C':'3':_   -> discard 3 >> return '\DC3'
        'D':'C':'4':_   -> discard 3 >> return '\DC4'
        'N':'A':'K':_   -> discard 3 >> return '\NAK'
        'S':'Y':'N':_   -> discard 3 >> return '\SYN'
        'E':'T':'B':_   -> discard 3 >> return '\ETB'
        'C':'A':'N':_   -> discard 3 >> return '\CAN'
        'E':'M':_       -> discard 2 >> return '\EM'
        'S':'U':'B':_   -> discard 3 >> return '\SUB'
        'E':'S':'C':_   -> discard 3 >> return '\ESC'
        'F':'S':_       -> discard 2 >> return '\FS'
        'G':'S':_       -> discard 2 >> return '\GS'
        'R':'S':_       -> discard 2 >> return '\RS'
        'U':'S':_       -> discard 2 >> return '\US'
        'S':'P':_       -> discard 2 >> return '\SP'
        'D':'E':'L':_   -> discard 3 >> return '\DEL'

-- Escaped numbers

        'o':c:_ | isOctDigit c -> do
                    discard 1
                    n <- lexOctal
                    checkChar n
        'x':c:_ | isHexDigit c -> do
                    discard 1
                    n <- lexHexadecimal
                    checkChar n
        c:_ | isDigit c -> do
                    n <- lexDecimal
                    checkChar n

        _       -> fail "Illegal escape sequence"

    where
    checkChar n | n <= 0x01FFFF = return (chr (fromInteger n))
    checkChar _                 = fail "Character constant out of range"

-- Production cntrl from section B.2

    cntrl :: Char -> Lex a Char
    cntrl c | c >= '@' && c <= '_' = return (chr (ord c - ord '@'))
    cntrl _                        = fail "Illegal control character"

-- assumes at least one octal digit
lexOctal :: Lex a Integer
lexOctal = do
    ds <- lexWhile isOctDigit
    return (parseInteger 8 ds)

-- assumes at least one hexadecimal digit
lexHexadecimal :: Lex a Integer
lexHexadecimal = do
    ds <- lexWhile isHexDigit
    return (parseInteger 16 ds)

-- assumes at least one decimal digit
lexDecimal :: Lex a Integer
lexDecimal = do
    ds <- lexWhile isDigit
    return (parseInteger 10 ds)

-- Stolen from Hugs's Prelude
parseInteger :: Integer -> String -> Integer
parseInteger radix ds =
    foldl1 (\n d -> n * radix + d) (map (toInteger . digitToInt) ds)

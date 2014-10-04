> {
> {-# OPTIONS_HADDOCK hide #-}
> -----------------------------------------------------------------------------
> -- |
> -- Module      :  Language.Haskell.Exts.Annotated.Parser
> -- Copyright   :  (c) Niklas Broberg 2004-2009,
> --                Original (c) Simon Marlow, Sven Panne 1997-2000
> -- License     :  BSD-style (see the file LICENSE.txt)
> --
> -- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
> -- Stability   :  stable
> -- Portability :  portable
> --
> --
> -----------------------------------------------------------------------------
>
> module Language.Haskell.Exts.InternalParser (
>               mparseModule,
>               mparseExp,
>               mparsePat,
>               mparseDecl,
>               mparseType,
>               mparseStmt,
>               ngparseModulePragmas,
>               ) where
>
> import Language.Haskell.Exts.Annotated.Syntax hiding ( Type(..), Exp(..), Asst(..), XAttr(..), FieldUpdate(..) )
> import Language.Haskell.Exts.Annotated.Syntax ( Type, Exp, Asst )
> import Language.Haskell.Exts.ParseMonad
> import Language.Haskell.Exts.InternalLexer
> import Language.Haskell.Exts.ParseUtils
> import Language.Haskell.Exts.Annotated.Fixity
> import Language.Haskell.Exts.SrcLoc
> import Language.Haskell.Exts.Comments ( Comment )
> import Language.Haskell.Exts.Extension

> import Control.Monad ( liftM, (<=<) )
import Debug.Trace (trace)

> }

-----------------------------------------------------------------------------
This module comprises a parser for Haskell 98 with the following extensions

* Multi-parameter type classes with functional dependencies
* Implicit parameters
* Pattern guards
* Mdo notation
* FFI
* HaRP
* HSP

Most of the code is blatantly stolen from the GHC module Language.Haskell.Parser.
Some of the code for extensions is greatly influenced by GHC's internal parser
library, ghc/compiler/parser/Parser.y.
-----------------------------------------------------------------------------
Conflicts: 7 shift/reduce

2 for ambiguity in 'case x of y | let z = y in z :: Bool -> b'  [State 99, 186]
        (don't know whether to reduce 'Bool' as a btype or shift the '->'.
         Similarly lambda and if. The default resolution in favour of the
         shift means that a guard can never end with a type signature.
         In mitigation: it's a rare case and no Haskell implementation
         allows these, because it would require unbounded lookahead.)
        There are 2 conflicts rather than one because contexts are parsed
        as btypes (cf ctype).

1 for ambiguity in 'let ?x ...'                     [State 604]
        the parser can't tell whether the ?x is the lhs of a normal binding or
        an implicit binding. Fortunately resolving as shift gives it the only
        sensible meaning, namely the lhs of an implicit binding.

1 for ambiguity using hybrid modules                [State 159]
        For HSP pages that start with a <% %> block, the parser cannot tell whether
        to reduce a srcloc or shift the starting <%. Since any other body could not
        start with <%, shifting is the only sensible thing to do.

1 for ambiguity using toplevel xml modules          [State 158]
        For HSP xml pages starting with a <, the parser cannot tell whether to shift
        that < or reduce an implicit 'open'. Since no other body could possibly start
        with <, shifting is the only sensible thing to do.

1 for ambiguity in '{-# RULES "name" [ ... #-}'     [State 177]
        we don't know whether the '[' starts the activation or not: it
        might be the start of the declaration with the activation being
        empty. Resolving with shift means the declaration cannot start with '['.

1 for ambiguity in '{-# RULES "name" forall = ... #-}' 	[State 544]
	since 'forall' is a valid variable name, we don't know whether
	to treat a forall on the input as the beginning of a quantifier
	or the beginning of the rule itself.  Resolving to shift means
	it's always treated as a quantifier, hence the above is disallowed.
	This saves explicitly defining a grammar for the rule lhs that
	doesn't include 'forall'.

-----------------------------------------------------------------------------

> %token
>       VARID    { Loc _ (VarId _) }       -- 1
>       QVARID   { Loc _ (QVarId _) }
>       IDUPID   { Loc _ (IDupVarId _) }       -- duplicable implicit parameter ?x
>       ILINID   { Loc _ (ILinVarId _) }       -- linear implicit parameter %x
>       CONID    { Loc _ (ConId _) }
>       QCONID   { Loc _ (QConId _) }
>       DVARID   { Loc _ (DVarId _) }          -- VARID containing dashes
>       VARSYM   { Loc _ (VarSym _) }
>       CONSYM   { Loc _ (ConSym _) }
>       QVARSYM  { Loc _ (QVarSym _) } -- 10
>       QCONSYM  { Loc _ (QConSym _) }      
>       INT      { Loc _ (IntTok _) }
>       RATIONAL { Loc _ (FloatTok _) }
>       CHAR     { Loc _ (Character _) }
>       STRING   { Loc _ (StringTok _) }

>       PRIMINT     { Loc _ (IntTokHash _) }
>       PRIMWORD    { Loc _ (WordTokHash _) }
>       PRIMFLOAT   { Loc _ (FloatTokHash _) }
>       PRIMDOUBLE  { Loc _ (DoubleTokHash _) }
>       PRIMCHAR    { Loc _ (CharacterHash _) } -- 20
>       PRIMSTRING  { Loc _ (StringHash _) }  

Symbols

>       '('     { Loc $$ LeftParen }
>       ')'     { Loc $$ RightParen }
>       '(#'    { Loc $$ LeftHashParen }
>       '#)'    { Loc $$ RightHashParen }
>       ';'     { Loc $$ SemiColon }
>       '{'     { Loc $$ LeftCurly }
>       '}'     { Loc $$ RightCurly }      -- 30
>       vccurly { Loc $$ VRightCurly }                 -- a virtual close brace
>       '['     { Loc $$ LeftSquare }
>       ']'     { Loc $$ RightSquare }
>       '[:'    { Loc $$ ParArrayLeftSquare }
>       ':]'    { Loc $$ ParArrayRightSquare }
>       ','     { Loc $$ Comma }
>       '_'     { Loc $$ Underscore }
>       '`'     { Loc $$ BackQuote }

Reserved operators

>       '.'     { Loc $$ Dot }
>       '..'    { Loc $$ DotDot }
>       ':'     { Loc $$ Colon }
>       '::'    { Loc $$ DoubleColon }      -- 40
>       '='     { Loc $$ Equals }           
>       '\\'    { Loc $$ Backslash }
>       '|'     { Loc $$ Bar }
>       '<-'    { Loc $$ LeftArrow }
>       '->'    { Loc $$ RightArrow }
>       '@'     { Loc $$ At }
>       '~'     { Loc $$ Tilde }
>       '=>'    { Loc $$ DoubleArrow }
>       '-'     { Loc $$ Minus }
>       '!'     { Loc $$ Exclamation }  -- 50
>       '*'     { Loc $$ Star }           

Arrows

>       '-<'    { Loc $$ LeftArrowTail }
>       '>-'    { Loc $$ RightArrowTail }
>       '-<<'   { Loc $$ LeftDblArrowTail }
>       '>>-'   { Loc $$ RightDblArrowTail }

Harp

>       '(|'    { Loc $$ RPGuardOpen }
>       '|)'    { Loc $$ RPGuardClose }
>       '@:'    { Loc $$ RPCAt }

Template Haskell

>       IDSPLICE        { Loc _ (THIdEscape _) }   -- $x
>       '$('            { Loc $$ THParenEscape } -- 60
>       '[|'            { Loc $$ THExpQuote }     
>       '[p|'           { Loc $$ THPatQuote }
>       '[t|'           { Loc $$ THTypQuote }
>       '[d|'           { Loc $$ THDecQuote }
>       '|]'            { Loc $$ THCloseQuote }
>       VARQUOTE        { Loc $$ THVarQuote }      -- 'x
>       TYPQUOTE        { Loc $$ THTyQuote }       -- ''T
>       QUASIQUOTE      { Loc _ (THQuasiQuote _) }

Hsx

>       PCDATA          { Loc _ (XPCDATA _) }
>       '<'             { Loc $$ XStdTagOpen }   -- 70
>       '</'            { Loc $$ XCloseTagOpen }  
>       '<%'            { Loc $$ XCodeTagOpen }
>       '<%>'           { Loc $$ XChildTagOpen }
>       '>'             { Loc $$ XStdTagClose }
>       '/>'            { Loc $$ XEmptyTagClose }
>       '%>'            { Loc $$ XCodeTagClose }
>       '<['            { Loc $$ XRPatOpen }
>       ']>'            { Loc $$ XRPatClose }

FFI

>       'foreign'       { Loc $$ KW_Foreign }
>       'export'        { Loc $$ KW_Export }    -- 80
>       'safe'          { Loc $$ KW_Safe }      
>       'unsafe'        { Loc $$ KW_Unsafe }
>       'threadsafe'    { Loc $$ KW_Threadsafe }
>       'interruptible' { Loc $$ KW_Interruptible }
>       'stdcall'       { Loc $$ KW_StdCall }
>       'ccall'         { Loc $$ KW_CCall }
>       'cplusplus'     { Loc $$ KW_CPlusPlus }
>       'dotnet'        { Loc $$ KW_DotNet }
>       'jvm'           { Loc $$ KW_Jvm }
>       'js'            { Loc $$ KW_Js }          -- 90
>       'capi'          { Loc $$ KW_CApi }      

Reserved Ids

>       'as'            { Loc $$ KW_As }
>       'by'            { Loc $$ KW_By }       -- transform list comprehensions
>       'case'          { Loc $$ KW_Case }
>       'class'         { Loc $$ KW_Class }
>       'data'          { Loc $$ KW_Data }
>       'default'       { Loc $$ KW_Default }
>       'deriving'      { Loc $$ KW_Deriving }
>       'do'            { Loc $$ KW_Do }
>       'else'          { Loc $$ KW_Else }     -- 100
>       'family'        { Loc $$ KW_Family }   -- indexed type families
>       'forall'        { Loc $$ KW_Forall }   -- universal/existential qualification
>       'group'         { Loc $$ KW_Group }    -- transform list comprehensions
>       'hiding'        { Loc $$ KW_Hiding }
>       'if'            { Loc $$ KW_If }
>       'import'        { Loc $$ KW_Import }
>       'in'            { Loc $$ KW_In }
>       'infix'         { Loc $$ KW_Infix }
>       'infixl'        { Loc $$ KW_InfixL }
>       'infixr'        { Loc $$ KW_InfixR }        -- 110
>       'instance'      { Loc $$ KW_Instance }
>       'let'           { Loc $$ KW_Let }
>       'mdo'           { Loc $$ KW_MDo }
>       'module'        { Loc $$ KW_Module }         -- 114
>       'newtype'       { Loc $$ KW_NewType }
>       'of'            { Loc $$ KW_Of }
>       'proc'          { Loc $$ KW_Proc }     -- arrows
>       'rec'           { Loc $$ KW_Rec }      -- arrows or RecursiveDo
>       'then'          { Loc $$ KW_Then }
>       'type'          { Loc $$ KW_Type }     -- 120
>       'using'         { Loc $$ KW_Using }    -- transform list comprehensions
>       'where'         { Loc $$ KW_Where }
>       'qualified'     { Loc $$ KW_Qualified }

Pragmas

>       '{-# INLINE'            { Loc _ (INLINE _) }
>       '{-# INLINE CONLIKE'    { Loc $$ INLINE_CONLIKE }
>       '{-# SPECIALISE'        { Loc $$ SPECIALISE }
>       '{-# SPECIALISE INLINE' { Loc _ (SPECIALISE_INLINE _) }
>       '{-# SOURCE'            { Loc $$ SOURCE }
>       '{-# RULES'             { Loc $$ RULES }
>       '{-# CORE'              { Loc $$ CORE }         -- 130
>       '{-# SCC'               { Loc $$ SCC }       
>       '{-# GENERATED'         { Loc $$ GENERATED }
>       '{-# DEPRECATED'        { Loc $$ DEPRECATED }
>       '{-# WARNING'           { Loc $$ WARNING }
>       '{-# UNPACK'            { Loc $$ UNPACK }
>       '{-# OPTIONS'           { Loc _ (OPTIONS _) }
       '{-# CFILES'            { Loc _ (CFILES  _) }
       '{-# INCLUDE'           { Loc _ (INCLUDE _) }
>       '{-# LANGUAGE'          { Loc $$ LANGUAGE }      -- 137
>       '{-# ANN'               { Loc $$ ANN }
>       '{-# MINIMAL'           { Loc $$ MINIMAL }
>       '{-# NO_OVERLAP'        { Loc $$ NO_OVERLAP }
>       '{-# OVERLAP'           { Loc $$ OVERLAP }
>       '{-# INCOHERENT'        { Loc $$ INCOHERENT }
>       '#-}'                   { Loc $$ PragmaEnd }      -- 139


> %monad { P }
> %lexer { lexer } { Loc _ EOF }
> %error { parseError }
> %name mparseModule page
> %name mparseExp trueexp
> %name mparsePat pat
> %name mparseDecl topdecl
> %name mparseType truectype
> %name mparseStmt stmt
> %partial ngparseModulePragmas toppragmas
> %tokentype { Loc Token }
> %expect 7
> %%

-----------------------------------------------------------------------------
Testing multiple modules in one file

> modules :: { [Module L] }
>         : toppragmas modules1         { let (os,ss,l) = $1 in map (\x -> x os ss l) $2 }

> modules1 :: { [[ModulePragma L] -> [S] -> L -> Module L] }
>         : module modules1             { $1 : $2 }
>         | module                      { [$1] }

-----------------------------------------------------------------------------
HSP Pages

Any HSP-specific parts requiring the XmlSyntax extension enabled will
be governed by the lexing, since all productions require at least one
special lexeme.

TODO: Yuck, this is messy, needs fixing in the AST!

> page :: { Module L }
>       : toppragmas topxml                            {% checkPageModule $2 $1 }
>       | toppragmas '<%' module '%>' topxml           {% let (os,ss,l) = $1 in checkHybridModule $5 ($3 os ss l) $2 $4 }
>       | toppragmas module                            { let (os,ss,l) = $1 in $2 os ss l }

> topxml :: { PExp L }
>       : '<' name attrs mattr '>' children '</' name '>'        {% do { n <- checkEqNames $2 $8;
>                                                                        let { cn = reverse $6;
>                                                                              as = reverse $3; };
>                                                                        return $ XTag ($1 <^^> $9 <** [$1,$5,$7,$9]) n as $4 cn } }
>       | '<' name attrs mattr '/>'                              { XETag ($1 <^^> $5 <** [$1,$5]) $2 (reverse $3) $4 }


> toppragmas :: { ([ModulePragma L],[S],L) }
>           : open toppragmasaux close          { let (os,ss,ml) = $2 in (os,$1:ss++[$3],$1 <^^> $3) }

> toppragmasaux :: { ([ModulePragma L],[S],Maybe L) }
>               : toppragma ';' toppragmasaux         { let (os,ss,ml) = $3 in ($1 : os, $2 : ss, Just $ ann $1 <++> nIS $2 <+?> ml) }
>               | {- nothing -}                         { ([],[],Nothing) }

> toppragma :: { ModulePragma L }
>           : '{-# LANGUAGE' conids optsemis '#-}'   { LanguagePragma ($1 <^^> $4 <** ($1:snd $2 ++ reverse $3 ++ [$4])) (fst $2) }
>           | '{-# OPTIONS' optsemis '#-}'           { let Loc l (OPTIONS (mc, s)) = $1
>                                                       in OptionsPragma (l <^^> $3 <** (l:reverse $2 ++ [$3])) (readTool mc) s }
>           | '{-# ANN' annotation '#-}'             { AnnModulePragma ($1 <^^> $3 <** [$1,$3]) $2 }


> conids    :: { ([Name L],[S]) }
>          : conid ',' conids                  { ($1 : fst $3, $2 : snd $3) }
>          | conid                             { ([$1],[]) }

-----------------------------------------------------------------------------
Module Header

> module :: { [ModulePragma L] -> [S] -> L -> Module L }
>       : optmodulehead body
>               { let (is,ds,ss1,inf) = $2
>                  in \os ss l -> Module (l <++> inf <** (ss ++ ss1)) $1 os is ds }

> optmodulehead :: { Maybe (ModuleHead L) }
>       : 'module' modid maybemodwarning maybeexports 'where'   { Just $ ModuleHead ($1 <^^> $5 <** [$1,$5]) $2 $3 $4 }
>       | {- empty -}                                           { Nothing }

> maybemodwarning ::  { Maybe (WarningText L) }
>       : '{-# DEPRECATED' STRING '#-}'         { let Loc l (StringTok (s,_)) = $2 in Just $ DeprText ($1 <^^> $3 <** [$1,l,$3]) s }
>       | '{-# WARNING'    STRING '#-}'         { let Loc l (StringTok (s,_)) = $2 in Just $ WarnText ($1 <^^> $3 <** [$1,l,$3]) s }
>       | {- empty -}                           { Nothing }

> body :: { ([ImportDecl L],[Decl L],[S],L) }
>       : '{'  bodyaux '}'                      { let (is,ds,ss) = $2 in (is,ds,$1:ss ++ [$3], $1 <^^> $3) }
>       | open bodyaux close                    { let (is,ds,ss) = $2 in (is,ds,$1:ss ++ [$3], $1 <^^> $3) }

> bodyaux :: { ([ImportDecl L],[Decl L],[S]) }
>       : optsemis impdecls semis topdecls      { (reverse (fst $2), fst $4, reverse $1 ++ snd $2 ++ reverse $3 ++ snd $4) }
>       | optsemis                topdecls      { ([], fst $2, reverse $1 ++ snd $2) }
>       | optsemis impdecls optsemis            { (reverse (fst $2), [], reverse $1 ++ snd $2 ++ reverse $3) }
>       | optsemis                              { ([], [], reverse $1) }

> semis :: { [S] }
>       : optsemis ';'                          { $2 : $1 }

> optsemis :: { [S] }
>       : semis                                 { $1 }
>       | {- empty -}                           { [] }

-----------------------------------------------------------------------------
The Export List

> maybeexports :: { Maybe (ExportSpecList L) }
>       :  exports                              { Just $1 }
>       |  {- empty -}                          { Nothing }

> exports :: { ExportSpecList L }
>       : '(' exportlist optcomma ')'           { ExportSpecList ($1 <^^> $4 <** ($1:reverse (snd $2) ++ $3 ++ [$4])) (reverse (fst $2)) }
>       | '(' optcomma ')'                      { ExportSpecList ($1 <^^> $3 <** ($1:$2++[$3])) [] }

> optcomma :: { [S] }
>       : ','                                   { [$1] }
>       | {- empty -}                           { [  ] }

> exportlist :: { ([ExportSpec L],[S]) }
>       :  exportlist ',' export                { ($3 : fst $1, $2 : snd $1) }
>       |  export                               { ([$1],[])  }

> export :: { ExportSpec L }
>       :  qvar                                 { EVar (ann $1) (NoNamespace (ann $1)) $1 }
>       |  'type' qvar                          {% do { checkEnabled ExplicitNamespaces;
>                                                       return (EVar (nIS $1 <++> ann $2 <** [$1, srcInfoSpan (ann $2)]) (TypeNamespace (nIS $1 <** [$1])) $2) } }
>       |  qtyconorcls                          { EAbs (ann $1) $1 }
>       |  qtyconorcls '(' '..' ')'             { EThingAll  (ann $1 <++> nIS $4 <** [$2,$3,$4]) $1 }
>       |  qtyconorcls '(' ')'                  { EThingWith (ann $1 <++> nIS $3 <** [$2,$3])    $1 [] }
>       |  qtyconorcls '(' cnames ')'           { EThingWith (ann $1 <++> nIS $4 <** ($2:reverse (snd $3) ++ [$4])) $1 (reverse (fst $3)) }
>       |  'module' modid                       { EModuleContents (nIS $1 <++> ann $2 <** [$1]) $2 }

-----------------------------------------------------------------------------
Import Declarations

> impdecls :: { ([ImportDecl L],[S]) }
>       : impdecls semis impdecl                { ($3 : fst $1, snd $1 ++ reverse $2) }
>       | impdecl                               { ([$1],[]) }

> impdecl :: { ImportDecl L }
>       : 'import' optsrc optsafe optqualified maybepkg modid maybeas maybeimpspec
>                               { let { (mmn,ss,ml) = $7 ;
>                                       l = nIS $1 <++> ann $6 <+?> ml <+?> (fmap ann) $8 <** ($1:snd $2 ++ snd $3 ++ snd $4 ++ snd $5 ++ ss)}
>                                  in ImportDecl l $6 (fst $4) (fst $2) (fst $3) (fst $5) mmn $8 }

> optsrc :: { (Bool,[S]) }
>       : '{-# SOURCE' '#-}'                    { (True,[$1,$2]) }
>       | {- empty -}                           { (False,[]) }

> optsafe :: { (Bool,[S]) }
>       : 'safe'                           {% do { checkEnabledOneOf [Safe, SafeImports, Trustworthy] ;
>                                                  return (True, [$1]) } }
>       | {- empty -}                      { (False, []) }

> optqualified :: { (Bool,[S]) }
>       : 'qualified'                           { (True,[$1]) }
>       | {- empty -}                           { (False, []) }

Requires the PackageImports extension enabled.
> maybepkg :: { (Maybe String,[S]) }
>       : STRING                                {% do { checkEnabled PackageImports ;
>                                                       let { Loc l (StringTok (s,_)) = $1 } ;
>                                                       return $ (Just s,[l]) } }
>       | {- empty -}                           { (Nothing,[]) }

> maybeas :: { (Maybe (ModuleName L),[S],Maybe L) }
>       : 'as' modid                            { (Just $2,[$1],Just (nIS $1 <++> ann $2)) }
>       | {- empty -}                           { (Nothing,[],Nothing) }


> maybeimpspec :: { Maybe (ImportSpecList L) }
>       : impspec                               { Just $1 }
>       | {- empty -}                           { Nothing }

> impspec :: { ImportSpecList L }
>       : opthiding '(' importlist optcomma ')' { let {(b,ml,s) = $1 ;
>                                                       l = (ml <?+> ($2 <^^> $5)) <** (s ++ $2:reverse (snd $3) ++ $4 ++ [$5])}
>                                                  in ImportSpecList l b (reverse (fst $3)) }
>       | opthiding '(' optcomma ')'            { let {(b,ml,s) = $1 ; l = (ml <?+> ($2 <^^> $4)) <** (s ++ $2:$3 ++ [$4])}
>                                                  in ImportSpecList l b [] }

> opthiding :: { (Bool, Maybe L,[S]) }
>       : 'hiding'                              { (True,Just (nIS $1),[$1]) }
>       | {- empty -}                           { (False,Nothing,[])  }

> importlist :: { ([ImportSpec L],[S]) }
>       :  importlist ',' importspec            { ($3 : fst $1, $2 : snd $1) }
>       |  importspec                           { ([$1],[])  }

> importspec :: { ImportSpec L }
>       :  var                                  { IVar (ann $1) (NoNamespace (ann $1)) $1 }
>       |  'type' var                           {% do { checkEnabled ExplicitNamespaces;
>                                                       return (IVar (nIS $1 <++> ann $2 <** [$1, srcInfoSpan (ann $2)]) (TypeNamespace (nIS $1 <** [$1])) $2) } }
>       |  tyconorcls                           { IAbs (ann $1) $1 }
>       |  tyconorcls '(' '..' ')'              { IThingAll  (ann $1 <++> nIS $4 <** [$2,$3,$4]) $1 }
>       |  tyconorcls '(' ')'                   { IThingWith (ann $1 <++> nIS $3 <** [$2,$3])    $1 [] }
>       |  tyconorcls '(' cnames ')'            { IThingWith (ann $1 <++> nIS $4 <** ($2:reverse (snd $3) ++ [$4])) $1 (reverse (fst $3)) }

> cnames :: { ([CName L],[S]) }
>       :  cnames ',' cname                     { ($3 : fst $1, $2 : snd $1) }
>       |  cname                                { ([$1],[])  }

> cname :: { CName L }
>       :  var                                  { VarName (ann $1) $1 }
>       |  con                                  { ConName (ann $1) $1 }

-----------------------------------------------------------------------------
Fixity Declarations

> fixdecl :: { Decl L }
>       : infix prec ops                        { let (ops,ss,l) = $3
>                                                  in InfixDecl (ann $1 <++> l <** (snd $2 ++ reverse ss)) $1 (fst $2) (reverse ops) }

> prec :: { (Maybe Int, [S]) }
>       : {- empty -}                           { (Nothing, []) }
>       | INT                                   {% let Loc l (IntTok (i,_)) = $1 in checkPrec i >>= \i -> return (Just i, [l]) }

> infix :: { Assoc L }
>       : 'infix'                               { AssocNone  $ nIS $1 }
>       | 'infixl'                              { AssocLeft  $ nIS $1 }
>       | 'infixr'                              { AssocRight $ nIS $1 }

> ops   :: { ([Op L],[S],L) }
>       : ops ',' op                            { let (ops,ss,l) = $1 in ($3 : ops, $2 : ss, l <++> ann $3) }
>       | op                                    { ([$1],[],ann $1) }

-----------------------------------------------------------------------------
Top-Level Declarations

Note: The report allows topdecls to be empty. This would result in another
shift/reduce-conflict, so we don't handle this case here, but in bodyaux.

> topdecls :: { ([Decl L],[S]) }
>       : topdecls1 optsemis            {% checkRevDecls (fst $1) >>= \ds -> return (ds, snd $1 ++ reverse $2) }

> topdecls1 :: { ([Decl L],[S]) }
>       : topdecls1 semis topdecl       { ($3 : fst $1, snd $1 ++ reverse $2) }
>       | topdecl                       { ([$1],[]) }

> topdecl :: { Decl L }
>       : 'type' dtype '=' truectype
>                {% do { dh <- checkSimpleType $2;
>                        let {l = nIS $1 <++> ann $4 <** [$1,$3]};
>                        return (TypeDecl l dh $4) } }

Requires the TypeFamilies extension enabled, but the lexer will handle
that through the 'family' keyword.
>       | 'type' 'family' type optkind where_type_family
>                {% do { dh <- checkSimpleType $3;
>                        let {l = nIS $1 <++> ann $3 <+?> (fmap ann) (fst $4) <** ($1:$2:snd $4)};
>                        case $5 of {
>                          Nothing    -> return (TypeFamDecl l dh (fst $4));
>                          Just (x,a) -> return (ClosedTypeFamDecl (l <** [a]) dh (fst $4) x); }}}

Here there is no special keyword so we must do the check.
>       | 'type' 'instance' truedtype '=' truectype
>                {% do { -- no checkSimpleType $4 since dtype may contain type patterns
>                        checkEnabled TypeFamilies ;
>                        let {l = nIS $1 <++> ann $5 <** [$1,$2,$4]};
>                        return (TypeInsDecl l $3 $5) } }
>       | data_or_newtype ctype constrs0 deriving
>                {% do { (cs,dh) <- checkDataHeader $2;
>                        let { (qds,ss,minf) = $3;
>                              l = $1 <> $2 <+?> minf <+?> fmap ann $4 <** ss};
>                        checkDataOrNew $1 qds;
>                        return (DataDecl l $1 cs dh (reverse qds) $4) } }

Requires the GADTs extension enabled, handled in gadtlist.
>       | data_or_newtype ctype optkind gadtlist deriving
>                {% do { (cs,dh) <- checkDataHeader $2;
>                        let { (gs,ss,minf) = $4;
>                              l = ann $1 <+?> minf <+?> fmap ann $5 <** (snd $3 ++ ss)};
>                        checkDataOrNewG $1 gs;
>                        case (gs, fst $3) of
>                         ([], Nothing) -> return (DataDecl l $1 cs dh [] $5)
>                         _ -> checkEnabled GADTs >> return (GDataDecl l $1 cs dh (fst $3) (reverse gs) $5) } }

Same as above, lexer will handle it through the 'family' keyword.
>       | 'data' 'family' ctype optkind
>                {% do { (cs,dh) <- checkDataHeader $3;
>                        let {l = nIS $1 <++> ann $3 <+?> (fmap ann) (fst $4) <** ($1:$2:snd $4)};
>                        return (DataFamDecl l cs dh (fst $4)) } }

Here we must check for TypeFamilies.
>       | data_or_newtype 'instance' truectype constrs0 deriving
>                {% do { -- (cs,c,t) <- checkDataHeader $4;
>                        checkEnabled TypeFamilies ;
>                        let { (qds,ss,minf) = $4 ;
>                              l = $1 <> $3 <+?> minf <+?> fmap ann $5 <** $2:ss };
>                        checkDataOrNew $1 qds;
>                        return (DataInsDecl l $1 $3 (reverse qds) $5) } }

This style requires both TypeFamilies and GADTs, the latter is handled in gadtlist.
>       | data_or_newtype 'instance' truectype optkind gadtlist deriving
>                {% do { -- (cs,c,t) <- checkDataHeader $4;
>                        checkEnabled TypeFamilies ;
>                        let {(gs,ss,minf) = $5;
>                             l = ann $1 <+?> minf <+?> fmap ann $6 <** ($2:snd $4 ++ ss)};
>                        checkDataOrNewG $1 gs;
>                        return (GDataInsDecl l $1 $3 (fst $4) (reverse gs) $6) } }
>       | 'class' ctype fds optcbody
>                {% do { (cs,dh) <- checkClassHeader $2;
>                        let {(fds,ss1,minf1) = $3;(mcs,ss2,minf2) = $4} ;
>                        let { l = nIS $1 <++> ann $2 <+?> minf1 <+?> minf2 <** ($1:ss1 ++ ss2)} ;
>                        return (ClassDecl l cs dh fds mcs) } }
>       | 'instance' optoverlap ctype optvaldefs
>                {% do { ih <- checkInstHeader $3;
>                        let {(mis,ss,minf) = $4};
>                        return (InstDecl (nIS $1 <++> ann $3 <+?> minf <** ($1:ss)) $2 ih mis) } }

Requires the StandaloneDeriving extension enabled.
>       | 'deriving' 'instance' optoverlap ctype
>                {% do { checkEnabled StandaloneDeriving ;
>                        ih <- checkInstHeader $4;
>                        let {l = nIS $1 <++> ann $4 <** [$1,$2]};
>                        return (DerivDecl l $3 ih) } }
>       | 'default' '(' typelist ')'
>                { DefaultDecl ($1 <^^> $4 <** ($1:$2 : snd $3 ++ [$4])) (fst $3) }

Requires the TemplateHaskell extension, but the lexer will handle that
through the '$(' lexeme.
CHANGE: Arbitrary top-level expressions are considered implicit splices
>       | exp0             {% do
>               checkToplevel $1
>               checkExpr $1 >>= \e -> return (SpliceDecl (ann e) e) 
>                  }

       | '$(' trueexp ')'  { let l = $1 <^^> $3 <** [$1,$3] in SpliceDecl l $ ParenSplice l $2 }

These require the ForeignFunctionInterface extension, handled by the
lexer through the 'foreign' (and 'export') keyword.
>       | 'foreign' 'import' callconv safety fspec
>                { let (s,n,t,ss) = $5 in ForImp (nIS $1 <++> ann t <** ($1:$2:ss)) $3 $4 s n t }
>       | 'foreign' 'export' callconv fspec
>                { let (s,n,t,ss) = $4 in ForExp (nIS $1 <++> ann t <** ($1:$2:ss)) $3    s n t }

>       | '{-# RULES'      rules      '#-}'     { RulePragmaDecl ($1 <^^> $3 <** [$1,$3]) $ reverse $2 }
>       | '{-# DEPRECATED' warndeprs  '#-}'     { DeprPragmaDecl ($1 <^^> $3 <** ($1:snd $2++[$3])) $ reverse (fst $2) }
>       | '{-# WARNING'    warndeprs  '#-}'     { WarnPragmaDecl ($1 <^^> $3 <** ($1:snd $2++[$3])) $ reverse (fst $2) }
>       | '{-# ANN'        annotation '#-}'     { AnnPragma      ($1 <^^> $3 <** [$1,$3]) $2 }
>       | decl          { $1 }

> optoverlap :: { Maybe (Overlap L) }
>  : '{-# OVERLAP'    '#-}'    { Just (Overlap (nIS $1)) }
>  | '{-# INCOHERENT' '#-}'    { Just (Incoherent (nIS $1)) }
>  | '{-# NO_OVERLAP' '#-}'    { Just (NoOverlap (nIS $1))  }
>  | {- empty -}               { Nothing }

Parsing the body of a closed type family, partially stolen from the source of GHC.
> where_type_family :: { Maybe ([TypeEqn L], S) }
>         : {- empty -}                  { Nothing }
>         | 'where' ty_fam_inst_eqn_list { Just ($2, $1) }

> ty_fam_inst_eqn_list :: { [TypeEqn L] }
>         : '{'  ty_fam_inst_eqns '}'     { $2 }
>         | open ty_fam_inst_eqns close   { $2 }

> ty_fam_inst_eqns :: { [TypeEqn L] }
>         : ty_fam_inst_eqns ';' ty_fam_inst_eqn   { $1 ++ [$3] }
>         | ty_fam_inst_eqns ';'                   { $1 }
>         | ty_fam_inst_eqn                        { [$1] }

> ty_fam_inst_eqn :: { TypeEqn L }
>         : truedtype '=' truectype
>                 {% do { checkEnabled TypeFamilies ;
>                         return (TypeEqn (ann $1 <++> ann $3 <** [$2]) $1 $3) } }

> data_or_newtype :: { DataOrNew L }
>       : 'data'    { DataType $ nIS $1 }
>       | 'newtype' { NewType  $ nIS $1 }

> typelist :: { ([Type L],[S]) }
>       : types                         {% do { ts <- mapM checkType (fst $1);
>                                               return $ (reverse ts, reverse (snd $1)) } }
>       | truetype                      { ([$1],[]) }
>       | {- empty -}                   { ([],[]) }

> decls :: { ([Decl L],[S]) }
>       : optsemis decls1 optsemis      {% checkRevDecls (fst $2) >>= \ds -> return (ds, reverse $1 ++ snd $2 ++ reverse $3) }
>       | optsemis                      { ([],reverse $1) }

> decls1 :: { ([Decl L],[S]) }
>       : decls1 semis decl             { ($3 : fst $1, snd $1 ++ reverse $2) }
>       | decl                          { ([$1],[]) }

> decl :: { Decl L }
>       : signdecl                      { $1 }
>       | fixdecl                       { $1 }
>       | valdef                        { $1 }

> decllist :: { Binds L }
>       : '{'  decls '}'                { BDecls ($1 <^^> $3 <** ($1:snd $2++[$3])) (fst $2) }
>       | open decls close              { let l' = if null (fst $2) then nIS $3 else (ann . last $ fst $2)
>                                          in BDecls (nIS $1 <++> l' <** ($1:snd $2++[$3])) (fst $2) }

> signdecl :: { Decl L }
>       : signdecl0                     { $1 }
>       | specinldecl                   { $1 }

> signdecl0 :: { Decl L }
>       : exp0b '::' truectype                           {% do { v <- checkSigVar $1;
>                                                                return $ TypeSig ($1 <> $3 <** [$2]) [v] $3 } }
>       | exp0b ',' vars '::' truectype                  {% do { v <- checkSigVar $1;
>                                                                let {(vs,ss,_) = $3 ; l = $1 <> $5 <** ($2 : reverse ss ++ [$4]) } ;
>                                                                return $ TypeSig l (v : reverse vs) $5 } }

> specinldecl :: { Decl L }
>       : '{-# INLINE' activation qvar '#-}'             { let Loc l (INLINE s) = $1 in InlineSig (l <^^> $4 <** [l,$4]) s $2 $3 }
>       | '{-# INLINE CONLIKE' activation qvar '#-}'     { InlineConlikeSig ($1 <^^> $4 <** [$1,$4]) $2 $3 }
>       | '{-# SPECIALISE' activation qvar '::' sigtypes '#-}'      
>             { SpecSig ($1 <^^> $6 <** ($1: $4 : snd $5 ++ [$6])) $2 $3 (fst $5) }
>       | '{-# SPECIALISE INLINE' activation qvar '::' sigtypes '#-}'
>             { let Loc l (SPECIALISE_INLINE s) = $1
>                in SpecInlineSig (l <^^> $6 <** (l:$4:snd $5++[$6])) s $2 $3 (fst $5) }
>       | '{-# SPECIALISE' 'instance' ctype '#-}'        {% do { ih <- checkInstHeader $3;
>                                                                let {l = $1 <^^> $4 <** [$1,$2,$4]};
>                                                                return $ InstSig l ih } }
>       | '{-# MINIMAL' name_boolformula '#-}'           { MinimalPragma ($1 <^^> $3 <** [$1,$3]) $2 }

> sigtypes :: { ([Type L],[S]) }
>       : sigtype                           { ([$1],[]) }
>       | sigtype ',' sigtypes              { ($1 : fst $3, $2 : snd $3) }

> sigtype :: { Type L }
>       : ctype                             {% checkType $ mkTyForall (ann $1) Nothing Nothing $1 }

> name_boolformula :: { Maybe (BooleanFormula L) }
>        : name_boolformula1         { Just $1 }
>        | {- empty -}               { Nothing }

> name_boolformula1 :: { BooleanFormula L }
>        : name_boolformula_and                       { $1 }
>        | name_boolformula_and '|' name_boolformula1 { OrFormula (ann $1 <++>  ann $3 <** [$2]) [$1,$3] }

> name_boolformula_and :: { BooleanFormula L }
>        : name_boolformula_atom                             { $1 }
>        | name_boolformula_atom ',' name_boolformula_and    { AndFormula (ann $1 <++> ann $3 <** [$2]) [$1,$3] }

> name_boolformula_atom :: { BooleanFormula L }
>        : '(' name_boolformula1 ')' { ParenFormula ($1 <^^> $3 <** [$1,$3]) $2 }
>        | var                       { VarFormula (ann $1) $1 }

Binding can be either of implicit parameters, or it can be a normal sequence
of declarations. The two kinds cannot be mixed within the same block of
binding.

> binds :: { Binds L }
>       : decllist                      { $1 }
>       | '{' ipbinds '}'               { IPBinds ($1 <^^> $3 <** snd $2) (fst $2) }
>       | open ipbinds close            { let l' =  ann . last $ fst $2
>                                          in IPBinds (nIS $1 <++> l' <** snd $2) (fst $2) }

ATTENTION: Dirty Hackery Ahead! If the second alternative of vars is var
instead of qvar, we get another shift/reduce-conflict. Consider the
following programs:

   { (+) :: ... }          only var
   { (+) x y  = ... }      could (incorrectly) be qvar

We re-use expressions for patterns, so a qvar would be allowed in patterns
instead of a var only (which would be correct). But deciding what the + is,
would require more lookahead. So let's check for ourselves...

> vars  :: { ([Name L],[S],L) }
>       : vars ',' var                  { let (ns,ss,l) = $1 in ($3 : ns, $2 : ss, l <++> ann $3) }
>       | qvar                          {% do { n <- checkUnQual $1;
>                                               return ([n],[],ann n) } }

-----------------------------------------------------------------------------
FFI

These will only be called on in the presence of a 'foreign' keyword,
so no need to check for extensions.

> callconv :: { CallConv L }
>          : 'stdcall'                  { StdCall   (nIS $1) }
>          | 'ccall'                    { CCall     (nIS $1) }
>          | 'cplusplus'                { CPlusPlus (nIS $1) }
>          | 'dotnet'                   { DotNet    (nIS $1) }
>          | 'jvm'                      { Jvm       (nIS $1) }
>          | 'js'                       { Js        (nIS $1) }
>          | 'capi'                     { CApi      (nIS $1) }


> safety :: { Maybe (Safety L) }
>        : 'safe'                       { Just $ PlaySafe  (nIS $1) False }
>        | 'unsafe'                     { Just $ PlayRisky (nIS $1) }
>        | 'threadsafe'                 { Just $ PlaySafe  (nIS $1) True }
>        | 'interruptible'              { Just $ PlayInterruptible (nIS $1) }
>        | {- empty -}                  { Nothing }

> fspec :: { (Maybe String, Name L, Type L, [S]) }
>       : STRING var_no_safety '::' truedtype               { let Loc l (StringTok (s,_)) = $1 in (Just s, $2, $4, [l,$3]) }
>       |        var_no_safety '::' truedtype               { (Nothing, $1, $3, [$2]) }

-----------------------------------------------------------------------------
Pragmas

> rules :: { [Rule L] }
>       : rules ';'rule         { $3 : $1 }
>       | rules ';'             { $1 }
>       | rule                  { [$1] }
>       | {- empty -}           { [] }

> rule :: { Rule L }
>      : STRING activation ruleforall exp0 '=' trueexp      {% do { let {Loc l (StringTok (s,_)) = $1};
>                                                                   e <- checkRuleExpr $4;
>                                                                   return $ Rule (nIS l <++> ann $6 <** l:snd $3 ++ [$5]) s $2 (fst $3) e $6 } }

> activation :: { Maybe (Activation L) }
>        : {- empty -}          { Nothing }
>        | '[' INT ']'          { let Loc l (IntTok (i,_)) = $2 in Just $ ActiveFrom  ($1 <^^> $3 <** [$1,l,$3])    (fromInteger i) }
>        | '[' '~' INT ']'      { let Loc l (IntTok (i,_)) = $3 in Just $ ActiveUntil ($1 <^^> $4 <** [$1,$2,l,$4]) (fromInteger i) }

> ruleforall :: { (Maybe [RuleVar L],[S]) }
>       : {- empty -}                           { (Nothing,[]) }
>       | 'forall' rulevars '.'                 { (Just $2,[$1,$3]) }

> rulevars :: { [RuleVar L] }
>       : rulevar                       { [$1] }
>       | rulevar rulevars              { $1 : $2 }

> rulevar :: { RuleVar L }
>       : varid                             { RuleVar (ann $1) $1 }
>       | '(' varid '::' truectype ')'      { TypedRuleVar ($1 <^^> $5 <** [$1,$3,$5]) $2 $4 }

> warndeprs :: { ([([Name L],String)],[S]) }
>   : warndeprs ';' warndepr            { (fst $3 : fst $1, snd $1 ++ ($2:snd $3)) }
>   | warndeprs ';'                     { (fst $1, snd $1 ++ [$2]) }
>   | warndepr                          { ([fst $1],snd $1) }
>   | {- empty -}                       { ([],[]) }

> warndepr :: { (([Name L], String),[S]) }
>       : namevars STRING               { let Loc l (StringTok (s,_)) = $2 in ((fst $1,s),snd $1 ++ [l]) }

> namevars :: { ([Name L],[S]) }
>           : namevar                   { ([$1],[]) }
>           | namevar ',' namevars      { ($1 : fst $3, $2 : snd $3) }

> namevar :: { Name L }
>         : con                         { $1 }
>         | var                         { $1 }

> annotation :: { Annotation L }
>       : 'type' conid aexp             {% checkExpr $3 >>= \e -> return (TypeAnn   (nIS $1 <++> ann e <** [$1]) $2 e) }
>       | 'module' aexp                 {% checkExpr $2 >>= \e -> return (ModuleAnn (nIS $1 <++> ann e <** [$1])    e) }
>       | namevar aexp                  {% checkExpr $2 >>= \e -> return (Ann ($1 <> e) $1 e) }

-----------------------------------------------------------------------------
Types

Type equality contraints need the TypeFamilies extension.

> truedtype :: { Type L }
>       : dtype                         {% checkType $1 }

> dtype :: { PType L }
>       : btype                         { $1 }
>       | btype qtyconop dtype          { TyInfix ($1 <> $3) $1 $2 $3 }
>       | btype qtyvarop dtype          { TyInfix ($1 <> $3) $1 $2 $3 } -- FIXME
>       | btype '->' ctype              { TyFun ($1 <> $3 <** [$2]) $1 $3 }
>       | btype '~' btype               {% do { checkEnabledOneOf [TypeFamilies, GADTs] ;
>                                               let {l = $1 <> $3 <** [$2]};
>                                               return $ TyPred l $ EqualP l $1 $3 } }

Implicit parameters can occur in normal types, as well as in contexts.

> truetype :: { Type L }
>       : type                          {% checkType $1 }

> type :: { PType L }
>       : ivar '::' dtype               { let l = ($1 <> $3 <** [$2]) in TyPred l $ IParam l $1 $3 }
>       | dtype                         { $1 }

> truebtype :: { Type L }
>       : btype                         {% checkType $1 }

> btype :: { PType L }
>       : btype atype                   { TyApp ($1 <> $2) $1 $2 }
>       | atype                         { $1 }

UnboxedTuples requires the extension, but that will be handled through
the (# and #) lexemes. Kinds will be handled at the kind rule.

> atype :: { PType L }
>       : gtycon                        { TyCon   (ann $1) $1 }
>       | tyvar                         { TyVar   (ann $1) $1 }
>       | strict_mark atype             { let (bangOrPack, locs) = $1
>                                           in let annot = if bangOrPack then (BangedTy (nIS (last locs) <** locs)) else UnpackedTy (nIS (head locs) <++> nIS (last locs) <** locs)
>                                            in bangType (nIS (head locs) <++> ann $2) annot $2 }
>       | '(' types ')'                 { TyTuple ($1 <^^> $3 <** ($1:reverse ($3:snd $2))) Boxed   (reverse (fst $2)) }
>       | '(#' types1 '#)'              { TyTuple ($1 <^^> $3 <** ($1:reverse ($3:snd $2))) Unboxed (reverse (fst $2)) }
>       | '[' type ']'                  { TyList  ($1 <^^> $3 <** [$1,$3]) $2 }
>       | '[:' type ':]'                { TyParArray  ($1 <^^> $3 <** [$1,$3]) $2 }
>       | '(' ctype ')'                 { TyParen ($1 <^^> $3 <** [$1,$3]) $2 }
>       | '(' ctype '::' kind ')'       { TyKind  ($1 <^^> $5 <** [$1,$3,$5]) $2 $4 }
>       | '$(' trueexp ')'              { let l = ($1 <^^> $3 <** [$1,$3]) in TySplice l $ ParenSplice l $2 }
>       | IDSPLICE                      { let Loc l (THIdEscape s) = $1 in TySplice (nIS l) $ IdSplice (nIS l) s }
>       | ptype                         { % checkEnabled DataKinds >> return (TyPromoted (ann $1) $1) }

> ptype :: { Promoted L }
>       : VARQUOTE '[' ptypes1 ']'      { PromotedList  ($1 <^^> $4 <** ($1: reverse($4:snd $3))) True  (reverse (fst $3)) }
>       | VARQUOTE '['         ']'      { PromotedList  ($1 <^^> $3 <** [$1, $3])                 True  []                 }
>       |          '[' ptypes  ']'      { PromotedList  ($1 <^^> $3 <** ($1: reverse($3:snd $2))) False (reverse (fst $2)) }
>       | VARQUOTE '(' ptypes1 ')'      { PromotedTuple ($1 <^^> $4 <** ($1: reverse($4:snd $3)))       (reverse (fst $3)) }
>       | VARQUOTE '('         ')'      { PromotedUnit  ($1 <^^> $3 ) }
>       | VARQUOTE gconsym              { PromotedCon ((noInfoSpan $1 <++> ann $2) <** [$1]) True  $2 }
>       | VARQUOTE qtyconorcls          { PromotedCon ((noInfoSpan $1 <++> ann $2) <** [$1]) True  $2 }
>       | INT                           { let Loc l (IntTok  (i,raw)) = $1 in PromotedInteger (nIS l) i raw }
>       | STRING                        { let Loc l (StringTok (s,raw)) = $1 in PromotedString (nIS l) s raw }

> strict_mark :: { (Bool, [S]) }
>        : '!'                           { (True, [$1]) }
>        | '{-# UNPACK' '#-}' '!'        { (False, [$1,$2,$3]) }

Leading quotes can be left off of promoted types when they make up another promoted
type...

> ptype1 :: { Promoted L }
>       : ptype                         { $1 }
>       | qtyconorcls                   { PromotedCon (ann $1) False $1 }


> ptypes :: { ([Promoted L],[S]) }
>        : ptypes1 ',' ptype1           { ($3 : fst $1, $2 : snd $1)  }

> ptypes1 :: { ([Promoted L],[S]) }
>       : ptype1                        { ([$1],[]) }
>       | ptypes1 ',' ptype1            { ($3 : fst $1, $2 : snd $1) }


> gtycon :: { QName L }
>       : otycon                        { $1 }
>       | '(' ')'                       { unit_tycon_name              ($1 <^^> $2 <** [$1,$2]) }
>       | '(' '->' ')'                  { fun_tycon_name               ($1 <^^> $3 <** [$1,$2,$3]) }
>       | '[' ']'                       { list_tycon_name              ($1 <^^> $2 <** [$1,$2]) }
>       | '(' commas ')'                { tuple_tycon_name             ($1 <^^> $3 <** ($1:reverse $2 ++ [$3])) Boxed (length $2) }
>       | '(#' '#)'                     { unboxed_singleton_tycon_name ($1 <^^> $2 <** [$1,$2]) }
>       | '(#' commas '#)'              { tuple_tycon_name             ($1 <^^> $3 <** ($1:reverse $2 ++ [$3])) Unboxed (length $2) }

> otycon :: { QName L }
>       : qconid                        { $1 }
>       | '(' gconsym ')'               { updateQNameLoc ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }
>       | '(' qvarsym ')'               { updateQNameLoc ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }

These are for infix types

> qtyconop :: { QName L }
>       : qconop                        { $1 }


(Slightly edited) Comment from GHC's hsparser.y:
"context => type" vs  "type" is a problem, because you can't distinguish between

        foo :: (Baz a, Baz a)
        bar :: (Baz a, Baz a) => [a] -> [a] -> [a]

with one token of lookahead.  The HACK is to parse the context as a btype
(more specifically as a tuple type), then check that it has the right form
C a, or (C1 a, C2 b, ... Cn z) and convert it into a context.  Blaach!

Forall-quantified types require some extension to enable them, which
is any of the keyword-enabling ones, except ExistentialQuantification.

> truectype :: { Type L }
>       : ctype                         {% checkType $1 }

> ctype :: { PType L }
>       : 'forall' ktyvars '.' ctype    { mkTyForall (nIS $1 <++> ann $4 <** [$1,$3]) (Just (reverse (fst $2))) Nothing $4 }
>       | context ctype                 { mkTyForall ($1 <> $2) Nothing (Just $1) $2 }
>       | type                          { $1 }

Equality constraints require the TypeFamilies extension.

> context :: { PContext L }
>       : btype '=>'                    {% checkPContext $ (amap (\l -> l <++> nIS $2 <** (srcInfoPoints l ++ [$2]))) $1 }
>       | btype '~' btype '=>'          {% do { checkEnabledOneOf [TypeFamilies, GADTs];
>                                               let {l = $1 <> $3 <** [$2,$4]};
>                                               checkPContext (TyPred l $ EqualP l $1 $3) } }

> types :: { ([PType L],[S]) }
>       : types1 ',' ctype              { ($3 : fst $1, $2 : snd $1)  }

> types1 :: { ([PType L],[S]) }
>       : ctype                         { ([$1],[]) }
>       | types1 ',' ctype              { ($3 : fst $1, $2 : snd $1) }

> ktyvars :: { ([TyVarBind L],Maybe L) }
>       : ktyvars ktyvar                { ($2 : fst $1, Just (snd $1 <?+> ann $2)) }
>       | {- empty -}                   { ([],Nothing) }

> ktyvar :: { TyVarBind L }
>       : tyvar                         { UnkindedVar (ann $1) $1 }
>       | '(' tyvar '::' kind ')'       { KindedVar ($1 <^^> $5 <** [$1,$3,$5]) $2 $4 }

> tyvars :: { ([Name L],Maybe L) }
>       : tyvars tyvar                  { ($2 : fst $1, Just (snd $1 <?+> ann $2)) }
>       | {- empty -}                   { ([], Nothing) }

> tyvars1 :: { ([Name L],L) }
>       : tyvars tyvar                  { ($2 : fst $1, snd $1 <?+> ann $2) }


-----------------------------------------------------------------------------
Functional Dependencies

These require the FunctionalDependencies extension to be enabled.

> fds :: { ([FunDep L],[S],Maybe L) }
>       : {- empty -}                   { ([],[], Nothing) }
>       | '|' fds1                      {% do { checkEnabled FunctionalDependencies ;
>                                               let {(fds,ss,l) = $2} ;
>                                               return (reverse fds, $1 : reverse ss, Just (nIS $1 <++> l)) } }

> fds1 :: { ([FunDep L],[S],L) }
>       : fds1 ',' fd                   { let (fds,ss,l) = $1 in ($3 : fds, $2 : ss, l <++> ann $3) }
>       | fd                            { ([$1],[],ann $1) }

> fd :: { FunDep L }
>       : tyvars '->' tyvars1            { FunDep (snd $1 <?+> nIS $2 <++> snd $3 <** [$2]) (reverse (fst $1)) (reverse (fst $3)) }

-----------------------------------------------------------------------------
Datatype declarations

GADTs - require the GADTs extension enabled, but we handle that at the calling site.

 gadtlist :: { ([GadtDecl L],[S],L) }
       : gadtlist1                 {% >> return $1 }

> gadtlist :: { ([GadtDecl L],[S],Maybe L) }
>       : 'where' '{' gadtconstrs1 '}'                  {% return (fst $3, $1 : $2 : snd $3 ++ [$4], Just $ $1 <^^> $4) }
>       | 'where' open gadtconstrs1 close               {% return (fst $3, $1 : $2 : snd $3 ++ [$4], Just $ $1 <^^> $4) }
>       | {- empty -}                                   {% checkEnabled EmptyDataDecls >> return ([],[],Nothing) }

> gadtconstrs1 :: { ([GadtDecl L],[S]) }
>       : optsemis gadtconstrs optsemis         { (fst $2, reverse $1 ++ snd $2 ++ reverse $3)  }

> gadtconstrs :: { ([GadtDecl L],[S]) }
>       : gadtconstrs semis gadtconstr          { ($3 ++ fst $1, snd $1 ++ reverse $2) }
>       | gadtconstr                            { ($1,[]) }

> gadtconstr :: { [GadtDecl L] }
>       : qcon '::' truectype            {% do { c <- checkUnQual $1;
>                                                return [GadtDecl ($1 <> $3 <** [$2]) c Nothing $3] } }
>       | qcon '::' '{' fielddecls '}' '->' truectype
>                                       {% do { c <- checkUnQual $1;
>                                               return [GadtDecl ($1 <> $7 <** [$2,$3,$5,$6] ++ snd $4) c (Just (reverse $ fst $4)) $7] } }

To allow the empty case we need the EmptyDataDecls extension.
> constrs0 :: { ([QualConDecl L],[S],Maybe L) }
       : {- empty -}                   {% checkEnabled EmptyDataDecls >> return ([],[],Nothing) }
>       : '=' constrs                   { let (ds,ss,l) = $2 in (ds, $1 : reverse ss, Just $ nIS $1 <++> l) }

> constrs :: { ([QualConDecl L],[S],L) }
>       : constrs '|' constr            { let (ds,ss,l) = $1 in ($3 : ds, $2 : ss, l <++> ann $3) }
>       | constr                        { ([$1],[],ann $1) }

> constr :: { QualConDecl L }
>       : forall context constr1        {% do { checkEnabled ExistentialQuantification ;
>                                                ctxt <- checkContext (Just $2) ;
>                                                let {(mtvs,ss,ml) = $1} ;
>                                                return $ QualConDecl (ml <?+> ann $3 <** ss) mtvs ctxt $3 } }
>       | forall constr1                 { let (mtvs, ss, ml) = $1 in QualConDecl (ml <?+> ann $2 <** ss) mtvs Nothing $2 }

> forall :: { (Maybe [TyVarBind L], [S], Maybe L) }
>       : 'forall' ktyvars '.'          {% checkEnabled ExistentialQuantification >> return (Just (fst $2), [$1,$3], Just $ $1 <^^> $3) }
>       | {- empty -}                   { (Nothing, [], Nothing) }

To avoid conflicts when introducing type operators, we need to parse record constructors
as qcon and then check separately that they are truly unqualified.

> constr1 :: { ConDecl L }
>       : scontype                      { let (n,ts,l) = $1 in ConDecl l n ts }
>       | truebtype conop truebtype     { InfixConDecl ($1 <> $3) $1 $2 $3 }
>       | qcon '{' '}'                  {% do { c <- checkUnQual $1; return $ RecDecl (ann $1 <++> nIS $3 <** [$2,$3]) c [] } }
>       | qcon '{' fielddecls '}'       {% do { c <- checkUnQual $1;
>                                               return $ RecDecl (ann $1 <++> nIS $4 <** ($2:reverse (snd $3) ++ [$4])) c (reverse (fst $3)) } }

> scontype :: { (Name L, [Type L], L) }
>       : btype                         {% do { (c,ts) <- splitTyConApp $1;
>                                               return (c, ts, ann $1) } }

> fielddecls :: { ([FieldDecl L],[S]) }
>       : fielddecls ',' fielddecl      { ($3 : fst $1, $2 : snd $1) }
>       | fielddecl                     { ([$1],[]) }

> fielddecl :: { FieldDecl L }
>       : vars '::' truectype               { let (ns,ss,l) = $1 in FieldDecl (l <++> ann $3 <** (reverse ss ++ [$2])) (reverse ns) $3 }

> deriving :: { Maybe (Deriving L) }
>       : {- empty -}                   { Nothing }
>       | 'deriving' qtycls1            { let l = nIS $1 <++> ann $2 <** [$1] in Just $ Deriving l [IRule (ann $2) Nothing Nothing $2] }
>       | 'deriving' '('          ')'   { Just $ Deriving ($1 <^^> $3 <** [$1,$2,$3]) [] }
>       | 'deriving' '(' dclasses ')'   { Just $ Deriving ($1 <^^> $4 <** $1:$2: reverse (snd $3) ++ [$4]) (reverse (fst $3)) }

> dclasses :: { ([InstRule L],[S]) }
>       : types1                        {% checkDeriving (fst $1) >>= \ds -> return (ds, snd $1) }

> qtycls1 :: { InstHead L }
>       : qconid                        { IHCon (ann $1) $1 }


-----------------------------------------------------------------------------
Kinds

> kind :: { Kind L }
>       : kind1                 {% checkEnabled KindSignatures >> return $1 }

> kind1 :: { Kind L }
>       : akind                 { $1 }
>       | akind kind1           { KindApp ($1 <> $2) $1 $2 }
>       | akind '->' kind1      { KindFn ($1 <> $3 <** [$2]) $1 $3 }

> akind :: { Kind L }
>       : '*'                   { KindStar  (nIS $1) }
>       | '!'                   { KindBang  (nIS $1) }
>       | '(' kind1 ')'         { KindParen ($1 <^^> $3 <** [$1,$3]) $2 }
>       | pkind                 {% checkKind $1 >> return $1 }
>       | qvarid                {% checkEnabled PolyKinds >> return (KindVar (ann $1) $1) }

KindParen covers 1-tuples, KindVar l  while KindTuple is for pairs

> pkind :: { Kind L }
>         : qtyconorcls         { KindVar (ann $1) $1 }
>         | '(' ')'             { let l = $1 <^^> $2 in KindVar l (unit_tycon_name l) }
>         | '(' kinds  ')'      { KindTuple ($1 <^^> $3 <** ($1:reverse ($3:snd $2))) (reverse (fst $2)) }
>         | '[' kinds1 ']'      { KindList  ($1 <^^> $3 <** ($1:reverse ($3:snd $2))) (reverse (fst $2)) }


repeat of what is done for types

> kinds :: { ([Kind L],[S]) }
>       : kinds1 ',' akind              { ($3 : fst $1, $2 : snd $1)  }

> kinds1 :: { ([Kind L],[S]) }
>       : akind                         { ([$1],[]) }
>       | kinds1 ',' akind              { ($3 : fst $1, $2 : snd $1) }


> optkind :: { (Maybe (Kind L), [S]) }
>       : {-empty-}             { (Nothing,[]) }
>       | '::' kind             { (Just $2,[$1]) }
-----------------------------------------------------------------------------
Class declarations

TODO: Lots of stuff to pass around here.

No implicit parameters in the where clause of a class declaration.
> optcbody :: { (Maybe [ClassDecl L],[S],Maybe L) }
>       : 'where' '{'  cldecls '}'      {% checkClassBody (fst $3) >>= \vs -> return (Just vs, $1:$2: snd $3 ++ [$4], Just ($1 <^^> $4)) }
>       | 'where' open cldecls close    {% do { vs <- checkClassBody (fst $3);
>                                               let { l' = if null (fst $3) then nIS $4 else (ann . last $ fst $3) };
>                                               return (Just vs, $1:$2: snd $3 ++ [$4], Just (nIS $1 <++> l')) } }
>       | {- empty -}                   { (Nothing,[],Nothing) }

> cldecls :: { ([ClassDecl L],[S]) }
>       : optsemis cldecls1 optsemis    {% checkRevClsDecls (fst $2) >>= \cs -> return (cs, reverse $1 ++ snd $2 ++ reverse $3) }
>       | optsemis                      { ([],reverse $1) }

> cldecls1 :: { ([ClassDecl L],[S]) }
>       : cldecls1 semis cldecl         { ($3 : fst $1, snd $1 ++ reverse $2) }
>       | cldecl                        { ([$1],[]) }

Associated types require the TypeFamilies extension.

> cldecl :: { ClassDecl L }
>       : decl                          { ClsDecl (ann $1) $1 }
>       | atdecl                        {% checkEnabled TypeFamilies >> return $1 }
>       | 'default' signdecl            {% checkEnabled DefaultSignatures >> checkDefSigDef $2 >>= \(n,t,l) -> return (ClsDefSig (nIS $1 <++> ann $2 <** [$1,l]) n t) }

> atdecl :: { ClassDecl L }
>       : 'type' type optkind
>             {% do { dh <- checkSimpleType $2;
>                     return (ClsTyFam  (nIS $1 <++> ann $2 <+?> (fmap ann) (fst $3) <** $1:snd $3) dh (fst $3)) } }
>       | 'type' truedtype '=' truectype
>                     { ClsTyDef (nIS $1 <++> ann $4 <** [$1,$3]) $2 $4 }
>       | 'type' 'instance' truedtype '=' truectype
>                     { ClsTyDef (nIS $1 <++> ann $5 <** [$1,$2,$4]) $3 $5 }
>       | 'data' ctype optkind
>             {% do { (cs,dh) <- checkDataHeader $2;
>                     return (ClsDataFam (nIS $1 <++> ann $2 <+?> (fmap ann) (fst $3) <** $1:snd $3) cs dh (fst $3)) } }

-----------------------------------------------------------------------------
Instance declarations

> optvaldefs :: { (Maybe [InstDecl L],[S],Maybe L) }
>       : 'where' '{'  valdefs '}'      {% checkInstBody (fst $3) >>= \vs -> return (Just vs, $1:$2: snd $3 ++ [$4], Just ($1 <^^> $4))  }
>       | 'where' open valdefs close    {% checkInstBody (fst $3) >>= \vs -> return (Just vs, $1:$2: snd $3 ++ [$4], Just ($1 <^^> $4)) }
>       | {- empty -}                   { (Nothing, [], Nothing) }

> valdefs :: { ([InstDecl L],[S]) }
>       : optsemis valdefs1 optsemis    {% checkRevInstDecls (fst $2) >>= \is -> return (is, reverse $1 ++ snd $2 ++ reverse $3) }
>       | optsemis                      { ([],reverse $1) }

> valdefs1 :: { ([InstDecl L],[S]) }
>       : valdefs1 semis insvaldef      { ($3 : fst $1, snd $1 ++ reverse $2) }
>       | insvaldef                     { ([$1],[]) }

Associated types require the TypeFamilies extension enabled.

> insvaldef :: { InstDecl L }
>       : valdef                        { InsDecl (ann $1) $1 }
>       | atinst                        {% checkEnabled TypeFamilies >> return $1 }
>       | specinldecl                   { InsDecl (ann $1) $1 }
>       | signdecl0                     {% checkEnabled InstanceSigs >> return (InsDecl (ann $1) $1) }

 inlinst :: { InstDecl L }
       : '{-# INLINE' activation qvar '#-}'     { let Loc l (INLINE s) = $1 in InsInline (l <^^> $4 <** [l,$4]) s $2 $3 }

> atinst :: { InstDecl L }
>       : 'type' truedtype '=' truectype
>                {% do { -- no checkSimpleType $4 since dtype may contain type patterns
>                        return (InsType (nIS $1 <++> ann $4 <** [$1,$3]) $2 $4) } }
>       | data_or_newtype truectype constrs0 deriving
>                {% do { -- (cs,c,t) <- checkDataHeader $4;
>                        let {(ds,ss,minf) = $3};
>                        checkDataOrNew $1 ds;
>                        return (InsData ($1 <> $2 <+?> minf <+?> fmap ann $4 <** ss ) $1 $2 (reverse ds) $4) } }
>       | data_or_newtype truectype optkind gadtlist deriving
>                {% do { -- (cs,c,t) <- checkDataHeader $4;
>                        let { (gs,ss,minf) = $4 } ;
>                        checkDataOrNewG $1 gs;
>                        return $ InsGData (ann $1 <+?> minf <+?> fmap ann $5 <** (snd $3 ++ ss)) $1 $2 (fst $3) (reverse gs) $5 } }

-----------------------------------------------------------------------------
Value definitions

> valdef :: { Decl L }
>       : exp0b optsig rhs optwhere     {% checkValDef (($1 <> $3 <+?> (fmap ann) (fst $4)) <** (snd $4)) $1 $2 $3 (fst $4) }
>       | '!' aexp rhs optwhere         {% do { checkEnabled BangPatterns ;
>                                               let { l = nIS $1 <++> ann $2 <** [$1] };
>                                               p <- checkPattern (BangPat l $2);
>                                               return $ PatBind (p <> $3 <+?> (fmap ann) (fst $4) <** snd $4)
>                                                           p $3 (fst $4) } }

May bind implicit parameters
> optwhere :: { (Maybe (Binds L),[S]) }
>       : 'where' binds                 { (Just $2, [$1]) }
>       | {- empty -}                   { (Nothing, []) }

Type signatures on value definitions require ScopedTypeVariables (or PatternSignatures, which is deprecated).

> optsig :: { (Maybe (Type L, S)) }
>       : '::' truectype                {% checkEnabled ScopedTypeVariables >> return (Just ($2, $1)) }
>       | {- empty -}                   { Nothing }

> rhs   :: { Rhs L }
>       : '=' trueexp                   { UnGuardedRhs (nIS $1 <++> ann $2 <** [$1]) $2 }
>       | gdrhs                         { GuardedRhss (snd $1) (reverse $ fst $1) }

> gdrhs :: { ([GuardedRhs L],L) }
>       : gdrhs gdrh                    { ($2 : fst $1, snd $1 <++> ann $2) }
>       | gdrh                          { ([$1],ann $1) }

Guards may contain patterns if PatternGuards is enabled, hence quals instead of exp.
> gdrh :: { GuardedRhs L }
>       : '|' quals '=' trueexp  {% do { checkPatternGuards (fst $2);
>                                        return $ GuardedRhs (nIS $1 <++> ann $4 <** ($1:snd $2 ++ [$3])) (reverse (fst $2)) $4 } }

-----------------------------------------------------------------------------
Expressions

Note: The Report specifies a meta-rule for lambda, let and if expressions
(the exp's that end with a subordinate exp): they extend as far to
the right as possible.  That means they cannot be followed by a type
signature or infix application.  To implement this without shift/reduce
conflicts, we split exp10 into these expressions (exp10a) and the others
(exp10b).  That also means that only an exp0 ending in an exp10b (an exp0b)
can followed by a type signature or infix application.  So we duplicate
the exp0 productions to distinguish these from the others (exp0a).

Ugly: We need non-parenthesized post-operators for HaRP, and to parse both
these and normal left sections, we parse both as PostOp and let the post pass
mangle them into the correct form depending on context.

> trueexp :: { Exp L }
>         : exp                 {% checkExpr $1 }

> exp   :: { PExp L }
>       : exp0b '::' truectype              { ExpTypeSig      ($1 <> $3 <** [$2]) $1 $3 }
>       | exp0                              { $1 }
>       | exp0b qop                         { PostOp          ($1 <> $2)          $1 $2 }
>       | exp0b '-<' exp                    { LeftArrApp      ($1 <> $3 <** [$2]) $1 $3 }
>       | exp0b '>-' exp                    { RightArrApp     ($1 <> $3 <** [$2]) $1 $3 }
>       | exp0b '-<<' exp                   { LeftArrHighApp  ($1 <> $3 <** [$2]) $1 $3 }
>       | exp0b '>>-' exp                   { RightArrHighApp ($1 <> $3 <** [$2]) $1 $3 }

> exp0 :: { PExp L }
>       : exp0a                         { $1 }
>       | exp0b                         { $1 }

> exp0a :: { PExp L }
>       : exp0b qop exp10a              { InfixApp ($1 <> $3) $1 $2 $3 }
>       | exp10a                        { $1 }

> exp0b :: { PExp L }
>       : exp0b qop exp10b              { InfixApp ($1 <> $3) $1 $2 $3 }
>       | exp10b                        { $1 }

> exp10a :: { PExp L }
>       : '\\' apats '->' exp             { Lambda (nIS $1 <++> ann $4 <** [$1,$3]) (reverse $2) $4 }
A let may bind implicit parameters
>       | 'let' binds 'in' exp            { Let    (nIS $1 <++> ann $4 <** [$1,$3])    $2 $4 }
>       | 'if' exp optlayoutsemi 'then' exp optlayoutsemi 'else' exp
>                                        { If     (nIS $1 <++> ann $8 <** ($1:$3 ++ $4:$6 ++ [$7])) $2 $5 $8 }
>       | 'if' ifaltslist                 {% checkEnabled MultiWayIf >>
>                                            let (alts, inf, ss) = $2
>                                            in return (MultiIf (nIS $1 <++> inf <** ($1:ss)) alts) }
>       | 'proc' apat '->' exp            { Proc   (nIS $1 <++> ann $4 <** [$1,$3])    $2 $4 }
>       | exppragma                       { $1 }

> optlayoutsemi :: { [S] }
> 	: ';'				  {% checkEnabled DoAndIfThenElse >> return [$1] }
>	| {- empty -}			  { [] }

We won't come here unless XmlSyntax is already checked.
> opthsxsemi :: { [S] }
> 	: ';'				  { [$1] }
>	| {- empty -}			  { [] }


mdo blocks require the RecursiveDo extension enabled, but the lexer handles that.

> exp10b :: { PExp L }
>       : 'case' exp 'of' altslist      { let (als, inf, ss) = $4 in Case (nIS $1 <++> inf <** ($1:$3:ss)) $2 als }
>       | '\\' 'case' altslist          {% do { checkEnabled LambdaCase ;
>                                               let { (als, inf, ss) = $3 } ;
>                                               return (LCase (nIS $1 <++> inf <** ($1:$2:ss)) als) } }
>       | '-' fexp                      { NegApp (nIS $1 <++> ann $2 <** [$1]) $2 }
>       | 'do'  stmtlist                { let (sts, inf, ss) = $2 in Do   (nIS $1 <++> inf <** $1:ss) sts }
>       | 'mdo' stmtlist                { let (sts, inf, ss) = $2 in MDo  (nIS $1 <++> inf <** $1:ss) sts }
>       | fexp                          { $1 }

> exppragma :: { PExp L }
>       : '{-# CORE' STRING '#-}' exp   { let Loc l (StringTok (s,_)) = $2 in CorePragma (nIS $1 <++> ann $4 <** [l,$3]) s $4 }
>       | '{-# SCC'  STRING '#-}' exp   { let Loc l (StringTok (s,_)) = $2 in SCCPragma  (nIS $1 <++> ann $4 <** [l,$3]) s $4 }
>       | '{-# GENERATED' STRING INT ':' INT '-' INT ':' INT '#-}' exp
>                                           { let { Loc l0 (StringTok (s,_)) = $2;
>                                                   Loc l1 (IntTok (i1,_))   = $3;
>                                                   Loc l2 (IntTok (i2,_))   = $5;
>                                                   Loc l3 (IntTok (i3,_))   = $7;
>                                                   Loc l4 (IntTok (i4,_))   = $9}
>                                              in GenPragma (nIS $1 <++> ann $11 <** [$1,l0,l1,$4,l2,$6,l3,$8,l4,$10])
>                                                       s (fromInteger i1, fromInteger i2)
>                                                         (fromInteger i3, fromInteger i4) $11 }

> fexp :: { PExp L }
>       : fexp aexp                     { App ($1 <> $2) $1 $2 }
>       | aexp                          { $1 }

> apats :: { [Pat L] }
>       : apats apat                    { $2 : $1 }
>       | apat                          { [$1] }

> apat :: { Pat L }
>       : aexp                          {% checkPattern $1 }
>       | '!' aexp                      {% checkPattern (BangPat (nIS $1 <++> ann $2 <** [$1]) $2) }

UGLY: Because patterns and expressions are mixed, aexp has to be split into
two rules: One right-recursive and one left-recursive. Otherwise we get two
reduce/reduce-errors (for as-patterns and irrefutable patters).

Even though the variable in an as-pattern cannot be qualified, we use
qvar here to avoid a shift/reduce conflict, and then check it ourselves
(as for vars above).

Non-linear name binding, @:, requires RegularPatterns, but the lexer handles that.

> aexp  :: { PExp L }
>       : qvar '@' aexp                 {% do { n <- checkUnQual $1;
>                                               return (AsPat ($1 <> $3 <** [$2]) n $3) } }
>       | qvar '@:' aexp                {% do { n <- checkUnQual $1;
>                                               return (CAsRP ($1 <> $3 <** [$2]) n $3) } }
>       | '~' aexp                      { IrrPat (nIS $1 <++> ann $2 <** [$1]) $2 }
>       | aexp1                         { $1 }

Note: The first two alternatives of aexp1 are not necessarily record
updates: they could be labeled constructions.

> aexp1 :: { PExp L }
>       : aexp1 '{' '}'                 {% liftM (amap (const (ann $1 <++> nIS $3 <** [$2,$3]))) $ mkRecConstrOrUpdate $1 [] }
>       | aexp1 '{' fbinds '}'          {% liftM (amap (const (ann $1 <++> nIS $4 <** ($2:snd $3 ++ [$4]))))
>                                               $ mkRecConstrOrUpdate $1 (fst $3) }
>       | aexp2                         { $1 }

According to the Report, the left section (e op) is legal iff (e op x)
parses equivalently to ((e) op x).  Thus e must be an exp0b.
An implicit parameter can be used as an expression, enabled by the lexer.
Extensions using banana brackets are also enabled by the lexer. The only
thing we need to look at here is the erpats that use no non-standard lexemes.

> aexp2 :: { PExp L }
>       : ivar                          { IPVar (ann $1) $1 }
>       | qvar                          { Var (ann $1) $1 }
>       | gcon                          { $1 }
>       | literal                       { Lit (ann $1) $1 }
>       | '(' texp ')'                  { Paren ($1 <^^> $3 <** [$1,$3]) $2 }
>       | '(' texp tsectend             { TupleSection ($1 <^^> head (snd $3) <** $1:reverse (snd $3)) Boxed (Just $2 : fst $3) }
>       | '(' commas texp ')'           { TupleSection ($1 <^^> $4 <** $1:reverse ($4:$2)) Boxed
>                                                       (replicate (length $2) Nothing ++ [Just $3]) }
>       | '(' commas texp tsectend      { TupleSection ($1 <^^> head (snd $4) <** $1:reverse (snd $4 ++ $2)) Boxed
>                                                       (replicate (length $2) Nothing ++ Just $3 : fst $4) }
>       | '(#' texp thashsectend        { TupleSection ($1 <^^> head (snd $3) <** $1:reverse (snd $3)) Unboxed (Just $2 : fst $3) }
>       | '(#' texp '#)'                { TupleSection ($1 <^^> $3 <** [$1,$3]) Unboxed [Just $2] }
>       | '(#' commas texp '#)'         { TupleSection ($1 <^^> $4 <** $1:reverse ($4:$2)) Unboxed
>                                                       (replicate (length $2) Nothing ++ [Just $3]) }
>       | '(#' commas texp thashsectend { TupleSection ($1 <^^> head (snd $4) <** $1:reverse (snd $4 ++ $2)) Unboxed
>                                                       (replicate (length $2) Nothing ++ Just $3 : fst $4) }
>       | '[' list ']'                  { amap (\l -> l <** [$3]) $ $2 ($1 <^^> $3 <** [$1]) }
>       | '[:' parr ':]'                { amap (\l -> l <** [$3]) $ $2 ($1 <^^> $3 <** [$1]) }
>       | '_'                           { WildCard (nIS $1) }
>       | '(' erpats ')'                {% checkEnabled RegularPatterns >> return (Paren ($1 <^^> $3 <** [$1,$3]) $2) }
>       | '(|' sexps '|)'               { SeqRP ($1 <^^> $3 <** ($1:reverse (snd $2) ++ [$3])) $ reverse (fst $2) }
>       | '(|' exp '|' quals '|)'       { GuardRP ($1 <^^> $5 <** ($1:$3 : snd $4 ++ [$5])) $2 $ (reverse $ fst $4) }
>       | xml                           { $1 }

Template Haskell - all this is enabled in the lexer.
>       | IDSPLICE                      { let Loc l (THIdEscape s) = $1 in SpliceExp (nIS l) $ IdSplice (nIS l) s }
>       | '$(' trueexp ')'              { let l = ($1 <^^> $3 <** [$1,$3]) in SpliceExp l $ ParenSplice l $2 }
>       | '[|' trueexp '|]'             { let l = ($1 <^^> $3 <** [$1,$3]) in BracketExp l $ ExpBracket l $2 }
>       | '[p|' exp0 '|]'               {% do { p <- checkPattern $2;
>                                               let {l = ($1 <^^> $3 <** [$1,$3]) };
>                                               return $ BracketExp l $ PatBracket l p } }
>       | '[t|' truectype '|]'              { let l = $1 <^^> $3 <** [$1,$3] in BracketExp l $ TypeBracket l $2 }
>       | '[d|' open topdecls close '|]'    { let l = $1 <^^> $5 <** ($1:snd $3 ++ [$5])
>                                             in BracketExp l $ DeclBracket ($1 <^^> $5 <** ($2:snd $3 ++ [$4,$5])) (fst $3) }
>       | VARQUOTE qvar                 { VarQuote (nIS $1 <++> ann $2 <** [$1]) $2 }
>       | VARQUOTE qcon                 { VarQuote (nIS $1 <++> ann $2 <** [$1]) $2 }
>       | TYPQUOTE tyvar                { TypQuote (nIS $1 <++> ann $2 <** [$1]) (UnQual (ann $2) $2) }
>       | TYPQUOTE gtycon               { TypQuote (nIS $1 <++> ann $2 <** [$1]) $2 }
>       | QUASIQUOTE                    { let Loc l (THQuasiQuote (n,q)) = $1 in QuasiQuote (nIS l) n q }
End Template Haskell

> commas :: { [S] }
>       : commas ','                    { $2 : $1 }
>       | ','                           { [$1] }

> texp :: { PExp L }
>       : exp                           { $1 }
>       | qopm exp0                     { PreOp ($1 <> $2) $1 $2 }
>       | exp '->' exp                  {% do {checkEnabled ViewPatterns;
>                                              return $ ViewPat ($1 <> $3 <** [$2]) $1 $3} }

> tsectend :: { ([Maybe (PExp L)],[S]) }
>       : commas texp tsectend          { let (mes, ss) = $3 in (replicate (length $1 - 1) Nothing ++ Just $2 : mes, ss ++ $1) }
>       | commas texp ')'               { (replicate (length $1 - 1) Nothing ++ [Just $2], $3 : $1) }
>       | commas ')'                    { (replicate (length $1) Nothing, $2 : $1) }

> thashsectend :: { ([Maybe (PExp L)],[S]) }
>       : commas texp thashsectend      { let (mes, ss) = $3 in (replicate (length $1 - 1) Nothing ++ Just $2 : mes, ss ++ $1) }
>       | commas texp '#)'              { (replicate (length $1 - 1) Nothing ++ [Just $2], $3 : $1) }
>       | commas '#)'                   { (replicate (length $1) Nothing, $2 : $1) }

-----------------------------------------------------------------------------
Harp Extensions

> sexps :: { ([PExp L],[S]) }
>       : sexps ',' exp                 { ($3 : fst $1, $2 : snd $1) }
>       | exp                           { ([$1],[]) }

Either patterns are left associative
> erpats :: { PExp L }
>       : exp '|' erpats              { EitherRP ($1 <> $3 <** [$2]) $1 $3 }
>       | exp '|' exp                 { EitherRP ($1 <> $3 <** [$2]) $1 $3 }

-----------------------------------------------------------------------------
Hsx Extensions - requires XmlSyntax, but the lexer handles all that.

> xml :: { PExp L }
>       : '<' name attrs mattr '>' children opthsxsemi '</' name '>'        
>								 {% do { n <- checkEqNames $2 $9;
>                                                                        let { cn = reverse $6;
>                                                                              as = reverse $3;
>                                                                              l  = $1 <^^> $10 <** [$1,$5] ++ $7 ++ [$8,srcInfoSpan (ann $9),$10] };
>                                                                        return $ XTag l n as $4 cn } }
>       | '<' name attrs mattr '/>'                              { XETag   ($1 <^^> $5 <** [$1,$5]) $2 (reverse $3) $4 }
>       | '<%' exp '%>'                                          { XExpTag ($1 <^^> $3 <** [$1,$3]) $2 }
>       | '<%>' children opthsxsemi '</' '%>'                    { XChildTag ($1 <^^> $5 <** ($1:$3++[$4,$5])) (reverse $2) }

> children :: { [PExp L] }
>       : children child                { $2 : $1 }
>       | {- empty -}                   { [] }

> child :: { PExp L }
>       : PCDATA                        { let Loc l (XPCDATA pcd) = $1 in XPcdata (nIS l) pcd }
>       | '<[' sexps ']>'               { XRPats ($1 <^^> $3 <** (snd $2 ++ [$1,$3])) $ reverse (fst $2) }
>       | xml                           { $1 }

> name :: { XName L }
>       : xmlname ':' xmlname           { let {Loc l1 s1 = $1; Loc l2 s2 = $3}
>                                          in XDomName (nIS l1 <++> nIS l2 <** [l1,$2,l2]) s1 s2 }
>       | xmlname                       { let Loc l str = $1 in XName (nIS l) str }

> xmlname :: { Loc String }
>       : VARID                         { let Loc l (VarId  s) = $1 in Loc l s }
>       | CONID                         { let Loc l (ConId  s) = $1 in Loc l s }
>       | DVARID                        { let Loc l (DVarId s) = $1 in Loc l $ mkDVar s }
>       | xmlkeyword                    { $1 }

> xmlkeyword :: { Loc String }
>       : 'type'                        { Loc $1 "type" }
>       | 'class'                       { Loc $1 "class" }
>       | 'data'                        { Loc $1 "data" }
>       | 'foreign'                     { Loc $1 "foreign" }
>       | 'export'                      { Loc $1 "export" }
>       | 'safe'                        { Loc $1 "safe" }
>       | 'unsafe'                      { Loc $1 "unsafe" }
>       | 'interruptible'               { Loc $1 "interruptible" }
>       | 'threadsafe'                  { Loc $1 "threadsafe" }
>       | 'stdcall'                     { Loc $1 "stdcall" }
>       | 'ccall'                       { Loc $1 "ccall" }
>       | 'cplusplus'                   { Loc $1 "cplusplus" }
>       | 'dotnet'                      { Loc $1 "dotnet" }
>       | 'jvm'                         { Loc $1 "jvm" }
>       | 'js'                          { Loc $1 "js" }
>       | 'capi'                        { Loc $1 "capi" }
>       | 'as'                          { Loc $1 "as" }
>       | 'by'                          { Loc $1 "by" }
>       | 'case'                        { Loc $1 "case" }
>       | 'default'                     { Loc $1 "default" }
>       | 'deriving'                    { Loc $1 "deriving" }
>       | 'do'                          { Loc $1 "do" }
>       | 'else'                        { Loc $1 "else" }
>       | 'family'                      { Loc $1 "family" }
>       | 'forall'                      { Loc $1 "forall" }
>       | 'group'                       { Loc $1 "group" }
>       | 'hiding'                      { Loc $1 "hiding" }
>       | 'if'                          { Loc $1 "if" }
>       | 'import'                      { Loc $1 "import" }
>       | 'in'                          { Loc $1 "in" }
>       | 'infix'                       { Loc $1 "infix" }
>       | 'infixl'                      { Loc $1 "infixl" }
>       | 'infixr'                      { Loc $1 "infixr" }
>       | 'instance'                    { Loc $1 "instance" }
>       | 'let'                         { Loc $1 "let" }
>       | 'mdo'                         { Loc $1 "mdo" }
>       | 'module'                      { Loc $1 "module" }
>       | 'newtype'                     { Loc $1 "newtype" }
>       | 'of'                          { Loc $1 "of" }
>       | 'proc'                        { Loc $1 "proc" }
>       | 'rec'                         { Loc $1 "rec" }
>       | 'then'                        { Loc $1 "then" }
>       | 'using'                       { Loc $1 "using" }
>       | 'where'                       { Loc $1 "where" }
>       | 'qualified'                   { Loc $1 "qualified" }


> attrs :: { [ParseXAttr L] }
>       : attrs attr                    { $2 : $1 }
>       | {- empty -}                   { [] }

> attr :: { ParseXAttr L }
>       : name '=' aexp                 { XAttr ($1 <> $3 <** [$2]) $1 $3 }

> mattr :: { Maybe (PExp L) }

>       : aexp                          { Just $1 }
>       | {-empty-}                     { Nothing }

-----------------------------------------------------------------------------
List expressions

The rules below are little bit contorted to keep lexps left-recursive while
avoiding another shift/reduce-conflict.

> list :: { L -> PExp L }
>       : texp                          { \l -> List l [$1] }
>       | lexps                         { \l -> let (ps,ss) = $1 in List (l <** reverse ss) (reverse ps) }
>       | texp '..'                     { \l -> EnumFrom       (l <** [$2]) $1 }
>       | texp ',' exp '..'             { \l -> EnumFromThen   (l <** [$2,$4]) $1 $3 }
>       | texp '..' exp                 { \l -> EnumFromTo     (l <** [$2]) $1 $3 }
>       | texp ',' exp '..' exp         { \l -> EnumFromThenTo (l <** [$2,$4]) $1 $3 $5 }
>       | texp '|' pqualstmts           { \l -> let (stss, ss) = $3 in ParComp (l <** ($2:ss)) $1 (reverse stss) }

> lexps :: { ([PExp L],[S]) }
>       : lexps ',' texp                { let (es, ss) = $1 in ($3 : es, $2 : ss) }
>       | texp ',' texp                 { ([$3,$1], [$2]) }

-----------------------------------------------------------------------------
List comprehensions

> pqualstmts :: { ([[QualStmt L]],[S]) }
>       : pqualstmts '|' qualstmts      { let { (stss, ss1) = $1;
>                                               (sts, ss2) = $3 }
>                                          in (reverse sts : stss, ss1 ++ [$2] ++ reverse ss2)  }
>       | qualstmts                     { let (sts, ss) = $1 in ([reverse sts], reverse ss) }

> qualstmts :: { ([QualStmt L],[S]) }
>       : qualstmts ',' qualstmt        { let (sts, ss) = $1 in ($3 : sts, $2 : ss) }
>       | qualstmt                      { ([$1],[]) }

> qualstmt :: { QualStmt L }
>       : transformqual                 { $1 }
>       | qual                          { QualStmt (ann $1) $1 }

> transformqual :: { QualStmt L }
>       : 'then' trueexp                                { ThenTrans    (nIS $1 <++> ann $2 <** [$1]) $2 }
>       | 'then' trueexp 'by' trueexp                   { ThenBy       (nIS $1 <++> ann $4 <** [$1,$3]) $2 $4 }
>       | 'then' 'group' 'by' trueexp                   { GroupBy      (nIS $1 <++> ann $4 <** [$1,$2,$3]) $4 }
>       | 'then' 'group' 'using' trueexp                { GroupUsing   (nIS $1 <++> ann $4 <** [$1,$2,$3]) $4 }
>       | 'then' 'group' 'by' trueexp 'using' trueexp   { GroupByUsing (nIS $1 <++> ann $6 <** [$1,$2,$3,$5]) $4 $6 }

> quals :: { ([Stmt L],[S]) }
>       : quals ',' qual                { let (sts, ss) = $1 in ($3 : sts, $2 : ss) }
>       | qual                          { ([$1],[]) }

> qual  :: { Stmt L }
>       : pat '<-' trueexp              { Generator ($1 <> $3 <** [$2]) $1 $3 }
>       | trueexp                       { Qualifier (ann $1) $1 }
>       | 'let' binds                   { LetStmt   (nIS $1 <++> ann $2 <** [$1]) $2 }

-----------------------------------------------------------------------------
Parallel array expressions

See comments on list expressions. Parallel arrays are finite sequences.
This definition also allows empty arrays.

> parr :: { L -> PExp L }
>        :                               { \l -> ParArray l [] }
>        | texp                          { \l -> ParArray l [$1] }
>        | lexps                         { \l -> let (ps,ss) = $1 in ParArray (l <** reverse ss) (reverse ps) }
>        | texp '..' exp                 { \l -> ParArrayFromTo     (l <** [$2]) $1 $3  }
>        | texp ',' exp '..' exp         { \l -> ParArrayFromThenTo (l <** [$2,$4]) $1 $3 $5 }
>        | texp '|' pqualstmts           { \l -> let (stss, ss) = $3 in ParArrayComp (l <** ($2:ss)) $1 (reverse stss) }

-----------------------------------------------------------------------------
Case alternatives

> altslist :: { ([Alt L],L,[S]) }
>       : '{'  alts '}'                 { (fst $2, $1 <^^> $3, $1:snd $2 ++ [$3])  }
>       | open alts close               { let l' =  ann . last $ fst $2
>                                          in (fst $2, nIS $1 <++> l', $1:snd $2 ++ [$3]) }

> alts :: { ([Alt L],[S]) }
>       : optsemis alts1 optsemis       { (reverse $ fst $2, $1 ++ snd $2 ++ $3) }

> alts1 :: { ([Alt L],[S]) }
>       : alts1 semis alt               { ($3 : fst $1, snd $1 ++ $2) }
>       | alt                           { ([$1],[]) }

> alt :: { Alt L }
>       : pat ralt optwhere             { Alt ($1 <> $2 <+?> (fmap ann) (fst $3) <** snd $3) $1 $2 (fst $3) }

> ralt :: { Rhs L }
>       : '->' trueexp                  { UnGuardedRhs (nIS $1 <++> ann $2 <** [$1]) $2 }
>       | gdpats                        { GuardedRhss  (snd $1) (reverse $ fst $1) }

> gdpats :: { ([GuardedRhs L],L) }
>       : gdpats gdpat                  { ($2 : fst $1, snd $1 <++> ann $2) }
>       | gdpat                         { ([$1], ann $1) }
 
A guard can be a pattern guard if PatternGuards is enabled, hence quals instead of exp0.
> gdpat :: { GuardedRhs L }
>       : '|' quals '->' trueexp {% do { checkPatternGuards (fst $2);
>                                        let {l = nIS $1 <++> ann $4 <** ($1:snd $2 ++ [$3])};
>                                        return (GuardedRhs l (reverse (fst $2)) $4) } }

> pat :: { Pat L }
>       : exp                           {% checkPattern $1 }
>       | '!' aexp                      {% checkPattern (BangPat (nIS $1 <++> ann $2 <** [$1]) $2) }

> ifaltslist :: { ([GuardedRhs L], L, [S]) }
>       : '{'  ifalts '}'                 { (fst $2, $1 <^^> $3, $1:snd $2 ++ [$3])  }
>       | open ifalts close               { let l' =  ann . last $ fst $2
>                                            in (fst $2, nIS $1 <++> l', $1:snd $2 ++ [$3]) }

> ifalts :: { ([GuardedRhs L], [S]) }
>       : optsemis ifalts1 optsemis       { (reverse $ fst $2, $1 ++ snd $2 ++ $3) }

> ifalts1 :: { ([GuardedRhs L], [S]) }
>       : ifalts1 optsemis gdpat           { ($3 : fst $1, snd $1 ++ $2) }
>       | gdpat                            { ([$1], []) }

-----------------------------------------------------------------------------
Statement sequences

As per the Report, but with stmt expanded to simplify building the list
without introducing conflicts.  This also ensures that the last stmt is
an expression.

TODO: The points can't be added here, must be propagated!

> stmtlist :: { ([Stmt L],L,[S]) }
>       : '{'  stmts '}'                { (fst $2, $1 <^^> $3, $1:snd $2 ++ [$3])  }
>       | open stmts close              { let l' =  ann . last $ fst $2
>                                          in (fst $2, nIS $1 <++> l', $1:snd $2 ++ [$3]) }

> stmts :: { ([Stmt L],[S]) }
>       : stmt stmts1                       { ($1 : fst $2, snd $2) }
>       | ';' stmts                         { (fst $2, $1 : snd $2) }
>       | {- empty -}                       { ([],[]) }

> stmts1 :: { ([Stmt L],[S]) }
>       : ';' stmts                         { (fst $2, $1 : snd $2) }
>       | {- empty -}                       { ([],[]) }

A let statement may bind implicit parameters.
> stmt :: { Stmt L }
>       : 'let' binds                       { LetStmt (nIS $1 <++> ann $2 <** [$1]) $2 }
>       | pat '<-' trueexp                  { Generator ($1 <> $3 <** [$2]) $1 $3 }
>       | trueexp                           { Qualifier (ann $1) $1 }
>       | 'rec' stmtlist                    { let (stms,inf,ss) = $2 in RecStmt (nIS $1 <++> inf <** $1:ss) stms }

-----------------------------------------------------------------------------
Record Field Update/Construction

> fbinds :: { ([PFieldUpdate L],[S]) }
>       : fbind ',' fbinds              { let (fbs, ss) = $3 in ($1 : fbs, $2 : ss) }
>       | fbind                         { ([$1],[]) }
>       | '..'                          {% do { checkEnabled RecordWildCards `atSrcLoc` (getPointLoc $1);
>                                               return ([FieldWildcard (nIS $1)], []) } }

Puns and wild cards need the respective extensions enabled.

> fbind :: { PFieldUpdate L }
>       : qvar '=' exp                  { FieldUpdate ($1 <>$3 <** [$2]) $1 $3 }
>       | qvar                          {% checkEnabled NamedFieldPuns >> checkQualOrUnQual $1 >>= return . FieldPun (ann $1) }

-----------------------------------------------------------------------------
Implicit parameter bindings - need the ImplicitParameter extension enabled, but the lexer handles that.

> ipbinds :: { ([IPBind L],[S]) }
>       : optsemis ipbinds1 optsemis    { (reverse (fst $2), reverse $1 ++ snd $2 ++ reverse $3) }

> ipbinds1 :: { ([IPBind L],[S]) }
>       : ipbinds1 semis ipbind         { ($3 : fst $1, snd $1 ++ reverse $2) }
>       | ipbind                        { ([$1],[]) }

> ipbind :: { IPBind L }
>       : ivar '=' trueexp              { IPBind ($1 <> $3 <** [$2]) $1 $3 }

-----------------------------------------------------------------------------
Variables, Constructors and Operators.

> gcon :: { PExp L }
>       : '(' ')'               { p_unit_con              ($1 <^^> $2 <** [$1,$2]) }
>       | '[' ']'               { List                    ($1 <^^> $2 <** [$1,$2]) [] }
>       | '(' commas ')'        { p_tuple_con             ($1 <^^> $3 <** $1:reverse ($3:$2)) Boxed (length $2) }
>       | '(#' '#)'             { p_unboxed_singleton_con ($1 <^^> $2 <** [$1,$2]) }
>       | '(#' commas '#)'      { p_tuple_con             ($1 <^^> $3 <** $1:reverse ($3:$2)) Unboxed (length $2) }
>       | qcon                  { Con (ann $1) $1 }

> var   :: { Name L }
>       : varid                 { $1 }
>       | '(' varsym ')'        { fmap (const ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3])) $2 }

> var_no_safety :: { Name L }
>       : varid_no_safety       { $1 }
>       | '(' varsym ')'        { fmap (const ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3])) $2 }

> qvar  :: { QName L }
>       : qvarid                { $1 }
>       | '(' qvarsym ')'       { updateQNameLoc ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }

Implicit parameter
> ivar  :: { IPName L }
>       : ivarid                { $1 }

> con   :: { Name L }
>       : conid                 { $1 }
>       | '(' consym ')'        { fmap (const ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3])) $2 }

> qcon  :: { QName L }
>       : qconid                { $1 }
>       | '(' gconsym ')'       { updateQNameLoc ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }

> varop :: { Name L }
>       : varsym                { $1 }
>       | '`' varid '`'         { fmap (const ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3])) $2 }

> qvarop :: { QName L }
>       : qvarsym               { $1 }
>       | '`' qvarid '`'        { updateQNameLoc ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }

> qvaropm :: { QName L }
>       : qvarsymm              { $1 }
>       | '`' qvarid '`'        { updateQNameLoc ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }

> conop :: { Name L }
>       : consym                { $1 }
>       | '`' conid '`'         { fmap (const ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3])) $2 }

> qconop :: { QName L }
>       : gconsym               { $1 }
>       | '`' qconid '`'        { updateQNameLoc ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }

> op    :: { Op L }
>       : varop                 { VarOp (ann $1) $1 }
>       | conop                 { ConOp (ann $1) $1 }

> qop   :: { QOp L }
>       : qvarop                { QVarOp (ann $1) $1 }
>       | qconop                { QConOp (ann $1) $1 }

> qopm  :: { QOp L }
>       : qvaropm               { QVarOp (ann $1) $1 }
>       | qconop                { QConOp (ann $1) $1 }

> gconsym :: { QName L }
>       : ':'                   { list_cons_name (nIS $1) }
>       | qconsym               { $1 }

-----------------------------------------------------------------------------
Identifiers and Symbols

> qvarid :: { QName L }
>       : varid                 { UnQual (ann $1) $1 }
>       | QVARID                { let {Loc l (QVarId q) = $1; nis = nIS l}
>                                  in Qual nis (ModuleName nis (fst q)) (Ident nis (snd q)) }

> varid_no_safety :: { Name L }
>       : VARID                 { let Loc l (VarId v) = $1 in Ident (nIS l) v }
>       | 'as'                  { as_name        (nIS $1) }
>       | 'qualified'           { qualified_name (nIS $1) }
>       | 'hiding'              { hiding_name    (nIS $1) }
>       | 'export'              { export_name    (nIS $1) }
>       | 'stdcall'             { stdcall_name   (nIS $1) }
>       | 'ccall'               { ccall_name     (nIS $1) }
>       | 'cplusplus'           { cplusplus_name (nIS $1) }
>       | 'dotnet'              { dotnet_name    (nIS $1) }
>       | 'jvm'                 { jvm_name       (nIS $1) }
>       | 'js'                  { js_name        (nIS $1) }
>       | 'capi'                { capi_name      (nIS $1) }

> varid :: { Name L }
>       : varid_no_safety       { $1 }
>       | 'safe'                { safe_name       (nIS $1) }
>       | 'unsafe'              { unsafe_name     (nIS $1) }
>       | 'interruptible'       { interruptible_name (nIS $1) }
>       | 'threadsafe'          { threadsafe_name (nIS $1) }
>	| 'forall'		{ forall_name	  (nIS $1) }
>	| 'family'		{ family_name     (nIS $1) }


Implicit parameter
> ivarid :: { IPName L }
>       : IDUPID                { let Loc l (IDupVarId i) = $1 in IPDup (nIS l) i }
>       | ILINID                { let Loc l (ILinVarId i) = $1 in IPLin (nIS l) i }

> qconid :: { QName L }
>       : conid                 { UnQual (ann $1) $1 }
>       | QCONID                { let {Loc l (QConId q) = $1; nis = nIS l} in Qual nis (ModuleName nis (fst q)) (Ident nis (snd q)) }

> conid :: { Name L }
>       : CONID                 { let Loc l (ConId c) = $1 in Ident (nIS l) c }

> qconsym :: { QName L }
>       : consym                { UnQual (ann $1) $1 }
>       | QCONSYM               { let {Loc l (QConSym q) = $1; nis = nIS l} in Qual nis (ModuleName nis (fst q)) (Symbol nis (snd q)) }

> consym :: { Name L }
>       : CONSYM                { let Loc l (ConSym c) = $1 in Symbol (nIS l) c }

> qvarsym :: { QName L }
>       : varsym                { UnQual (ann $1) $1 }
>       | qvarsym1              { $1 }

> qvarsymm :: { QName L }
>       : varsymm               { UnQual (ann $1) $1 }
>       | qvarsym1              { $1 }

> varsym :: { Name L }
>       : VARSYM                { let Loc l (VarSym v) = $1 in Symbol (nIS l) v }
>       | '-'                   { minus_name (nIS $1) }
>       | '!'                   { bang_name  (nIS $1) }
>       | '.'                   { dot_name   (nIS $1) }
>       | '*'                   { star_name  (nIS $1) }

> varsymm :: { Name L } -- varsym not including '-'
>       : VARSYM                { let Loc l (VarSym v) = $1 in Symbol (nIS l) v }
>       | '!'                   { bang_name (nIS $1) }
>       | '.'                   { dot_name  (nIS $1) }
>       | '*'                   { star_name (nIS $1) }

> qvarsym1 :: { QName L }
>       : QVARSYM               { let {Loc l (QVarSym q) = $1; nis = nIS l} in Qual nis (ModuleName nis (fst q)) (Symbol nis (snd q)) }

> literal :: { Literal L }
>       : INT                   { let Loc l (IntTok        (i,raw)) = $1 in Int        (nIS l) i raw }
>       | CHAR                  { let Loc l (Character     (c,raw)) = $1 in Char       (nIS l) c raw }
>       | RATIONAL              { let Loc l (FloatTok      (r,raw)) = $1 in Frac       (nIS l) r raw }
>       | STRING                { let Loc l (StringTok     (s,raw)) = $1 in String     (nIS l) s raw }
>       | PRIMINT               { let Loc l (IntTokHash    (i,raw)) = $1 in PrimInt    (nIS l) i raw }
>       | PRIMWORD              { let Loc l (WordTokHash   (w,raw)) = $1 in PrimWord   (nIS l) w raw }
>       | PRIMFLOAT             { let Loc l (FloatTokHash  (f,raw)) = $1 in PrimFloat  (nIS l) f raw }
>       | PRIMDOUBLE            { let Loc l (DoubleTokHash (d,raw)) = $1 in PrimDouble (nIS l) d raw }
>       | PRIMCHAR              { let Loc l (CharacterHash (c,raw)) = $1 in PrimChar   (nIS l) c raw }
>       | PRIMSTRING            { let Loc l (StringHash    (s,raw)) = $1 in PrimString (nIS l) s raw }

-----------------------------------------------------------------------------
Layout

> open  :: { S }  :       {% pushCurrentContext >> getSrcLoc >>= \s -> return $ mkSrcSpan s s {- >>= \x -> trace (show x) (return x) -} }

> close :: { S }
>       : vccurly               { $1 {- >>= \x -> trace (show x ++ show x ++ show x) (return x) -} } -- context popped in lexer.
>       | error                 {% popContext >> getSrcLoc >>= \s -> return $ mkSrcSpan s s {- >>= \x -> trace (show x ++ show x) (return x) -} }

-----------------------------------------------------------------------------
Miscellaneous (mostly renamings)

> modid :: { ModuleName L }
>       : CONID                 { let Loc l (ConId  n) = $1 in ModuleName (nIS l) n }
>       | QCONID                { let Loc l (QConId n) = $1 in ModuleName (nIS l) (fst n ++ '.':snd n) }

> tyconorcls :: { Name L }
>       : con                   { $1 }

> qtyconorcls :: { QName L }
>       : qcon                  { $1 }

> tyvar :: { Name L }
>       : tyvarid               { $1 }

> tyvarid :: { Name L }
>       : varid_no_safety       { $1 }
>       | 'safe'                { safe_name       (nIS $1) }
>       | 'unsafe'              { unsafe_name     (nIS $1) }
>       | 'threadsafe'          { threadsafe_name (nIS $1) }
	| 'forall'		{ forall_name	  (nIS $1) }
	| 'family'		{ family_name     (nIS $1) }

> qtyvarop :: { QName L }
> qtyvarop : '`' tyvar '`'       { UnQual ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }
>          | tyvarsym            { UnQual (ann $1) $1 }

> tyvarsym :: { Name L }
> tyvarsym : VARSYM              { let Loc l (VarSym x) = $1 in Symbol (nIS l) x }

-----------------------------------------------------------------------------

> {

> type L = SrcSpanInfo -- just for convenience
> type S = SrcSpan

> parseError :: Loc Token -> P a
> parseError t = fail $ "Parse error: " ++ showToken (unLoc t)

> (<>) :: (Annotated a, Annotated b) => a SrcSpanInfo -> b SrcSpanInfo -> SrcSpanInfo
> a <> b = ann a <++> ann b
>
> infixl 6 <>

> nIS = noInfoSpan
> iS = infoSpan

> }

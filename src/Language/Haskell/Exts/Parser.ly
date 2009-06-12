> {
> -----------------------------------------------------------------------------
> -- |
> -- Module      :  Language.Haskell.Exts.Parser
> -- Copyright   :  (c) Niklas Broberg 2004,
> --                Original (c) Simon Marlow, Sven Panne 1997-2000
> -- License     :  BSD-style (see the file LICENSE.txt)
> --
> -- Maintainer  :  Niklas Broberg, d00nibro@dtek.chalmers.se
> -- Stability   :  experimental
> -- Portability :  portable
> --
> --
> -----------------------------------------------------------------------------
>
> module Language.Haskell.Exts.Parser (
>               parseModule, parseModuleWithMode,
>               parseExp, parsePat, parseDecl, parseType,
>               getTopPragmas,
>               ParseMode(..), defaultParseMode, ParseResult(..)) where
>
> import Language.Haskell.Exts.Syntax hiding ( Exp(..), XAttr(..), FieldUpdate(..) )
> import Language.Haskell.Exts.Syntax ( Exp )
> import Language.Haskell.Exts.ParseMonad
> import Language.Haskell.Exts.Lexer
> import Language.Haskell.Exts.ParseUtils
> import Language.Haskell.Exts.Extension
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

2 for ambiguity in 'case x of y | let z = y in z :: Bool -> b'  [State 220, 236]
        (don't know whether to reduce 'Bool' as a btype or shift the '->'.
         Similarly lambda and if. The default resolution in favour of the
         shift means that a guard can never end with a type signature.
         In mitigation: it's a rare case and no Haskell implementation
         allows these, because it would require unbounded lookahead.)
        There are 2 conflicts rather than one because contexts are parsed
        as btypes (cf ctype).

1 for ambiguity in 'let ?x ...'                     [State 707]
        the parser can't tell whether the ?x is the lhs of a normal binding or
        an implicit binding. Fortunately resolving as shift gives it the only
        sensible meaning, namely the lhs of an implicit binding.

1 for ambiguity using hybrid modules                [State 5]
        For HSP pages that start with a <% %> block, the parser cannot tell whether
        to reduce a srcloc or shift the starting <%. Since any other body could not
        start with <%, shifting is the only sensible thing to do.

1 for ambiguity using toplevel xml modules          [State 8]
        For HSP xml pages starting with a <, the parser cannot tell whether to shift
        that < or reduce an implicit 'open'. Since no other body could possibly start
        with <, shifting is the only sensible thing to do.

1 for ambiguity in '{-# RULES "name" [ ... #-}'     [State 212]
    we don't know whether the '[' starts the activation or not: it
    might be the start of the declaration with the activation being
    empty. Resolving with shift means the declaration cannot start with '['.

1 for ambiguity in 'x :: Int = ...'
    we don't know if we should reduce the lefthand side to a type signature
    declaration, or shift the '=' and treat the lefthand side as a pattern with
    a scoped type variable. Since a type signature declaration couldn't be followed
    by a '=', shifting is the only sensible thing to do.
-----------------------------------------------------------------------------

> %token
>       VARID    { VarId $$ }
>       QVARID   { QVarId $$ }
>       IDUPID   { IDupVarId $$ }       -- duplicable implicit parameter ?x
>       ILINID   { ILinVarId $$ }       -- linear implicit parameter %x
>       CONID    { ConId $$ }
>       QCONID   { QConId $$ }
>       DVARID   { DVarId $$ }          -- VARID containing dashes
>       VARSYM   { VarSym $$ }
>       CONSYM   { ConSym $$ }
>       QVARSYM  { QVarSym $$ }
>       QCONSYM  { QConSym $$ }
>       INT      { IntTok $$ }
>       RATIONAL { FloatTok $$ }
>       CHAR     { Character $$ }
>       STRING   { StringTok $$ }

>       PRIMINT     { IntTokHash $$ }
>       PRIMWORD    { WordTokHash $$ }
>       PRIMFLOAT   { FloatTokHash $$ }
>       PRIMDOUBLE  { DoubleTokHash $$ }
>       PRIMCHAR    { CharacterHash $$ }
>       PRIMSTRING  { StringHash $$ }

Symbols

>       '('     { LeftParen }
>       ')'     { RightParen }
>       '(#'    { LeftHashParen }
>       '#)'    { RightHashParen }
>       '{|'    { LeftCurlyBar }
>       '|}'    { RightCurlyBar }
>       ';'     { SemiColon }
>       '{'     { LeftCurly }
>       '}'     { RightCurly }
>       vccurly { VRightCurly }                 -- a virtual close brace
>       '['     { LeftSquare }
>       ']'     { RightSquare }
>       ','     { Comma }
>       '_'     { Underscore }
>       '`'     { BackQuote }

Reserved operators

>       '.'     { Dot }
>       '..'    { DotDot }
>       ':'     { Colon }
>       '::'    { DoubleColon }
>       '='     { Equals }
>       '\\'    { Backslash }
>       '|'     { Bar }
>       '<-'    { LeftArrow }
>       '->'    { RightArrow }
>       '@'     { At }
>       '~'     { Tilde }
>       '=>'    { DoubleArrow }
>       '-'     { Minus }
>       '!'     { Exclamation }
>       '*'     { Star }

Arrows

>       '-<'    { LeftArrowTail }
>       '>-'    { RightArrowTail }
>       '-<<'   { LeftDblArrowTail }
>       '>>-'   { RightDblArrowTail }

Harp

>       '(|'    { RPGuardOpen }
>       '|)'    { RPGuardClose }
>       '@:'    { RPCAt }

Template Haskell

>       IDSPLICE        { THIdEscape $$ }   -- $x
>       '$('            { THParenEscape }
>       '[|'            { THExpQuote }
>       '[p|'           { THPatQuote }
>       '[t|'           { THTypQuote }
>       '[d|'           { THDecQuote }
>       '|]'            { THCloseQuote }
>       VARQUOTE        { THVarQuote }      -- 'x
>       TYPQUOTE        { THTyQuote }       -- ''T
>       QUASIQUOTE      { THQuasiQuote $$ }

Hsx

>       PCDATA          { XPCDATA $$ }
>       '<'             { XStdTagOpen }
>       '</'            { XCloseTagOpen }
>       '<%'            { XCodeTagOpen }
>       '>'             { XStdTagClose }
>       '/>'            { XEmptyTagClose }
>       '%>'            { XCodeTagClose }
>       '<['            { XRPatOpen }
>       ']>'            { XRPatClose }

FFI

>       'foreign'       { KW_Foreign }
>       'export'        { KW_Export }
>       'safe'          { KW_Safe }
>       'unsafe'        { KW_Unsafe }
>       'threadsafe'    { KW_Threadsafe }
>       'stdcall'       { KW_StdCall }
>       'ccall'         { KW_CCall }

Reserved Ids

>       'as'            { KW_As }
>       'case'          { KW_Case }
>       'class'         { KW_Class }
>       'data'          { KW_Data }
>       'default'       { KW_Default }
>       'deriving'      { KW_Deriving }
>       'do'            { KW_Do }
>       'else'          { KW_Else }
>       'family'        { KW_Family }   -- indexed type families
>       'forall'        { KW_Forall }   -- universal/existential qualification
>       'hiding'        { KW_Hiding }
>       'if'            { KW_If }
>       'import'        { KW_Import }
>       'in'            { KW_In }
>       'infix'         { KW_Infix }
>       'infixl'        { KW_InfixL }
>       'infixr'        { KW_InfixR }
>       'instance'      { KW_Instance }
>       'let'           { KW_Let }
>       'mdo'           { KW_MDo }
>       'module'        { KW_Module }
>       'newtype'       { KW_NewType }
>       'of'            { KW_Of }
>       'proc'          { KW_Proc }     -- arrows
>       'rec'           { KW_Rec }      -- arrows
>       'then'          { KW_Then }
>       'type'          { KW_Type }
>       'where'         { KW_Where }
>       'qualified'     { KW_Qualified }

Pragmas

>       '{-# INLINE'            { INLINE $$ }
>       '{-# SPECIALISE'        { SPECIALISE }
>       '{-# SPECIALISE_INLINE' { SPECIALISE_INLINE $$ }
>       '{-# SOURCE'            { SOURCE }
>       '{-# RULES'             { RULES }
>       '{-# CORE'              { CORE }
>       '{-# SCC'               { SCC }
>       '{-# GENERATED'         { GENERATED }
>       '{-# DEPRECATED'        { DEPRECATED }
>       '{-# WARNING'           { WARNING }
>       '{-# UNPACK'            { UNPACK }
>       '{-# OPTIONS'           { OPTIONS $$ }
>       '{-# CFILES'            { CFILES  $$ }
>       '{-# INCLUDE'           { INCLUDE $$ }
>       '{-# LANGUAGE'          { LANGUAGE }
>      '{-# unknown'            { PragmaUnknown $$ }
>       '#-}'                   { PragmaEnd }


> %monad { P }
> %lexer { lexer } { EOF }
> %name parse page
> %name mparseExp trueexp
> %name mparsePat pat
> %name mparseDecl topdecl
> %name mparseType ctype
> %partial mfindOptPragmas toppragmas
> %tokentype { Token }
> %expect 7
> %%

-----------------------------------------------------------------------------
HSP Pages

Any HSP-specific parts requiring the XmlSyntax extension enabled will
be governed by the lexing, since all productions require at least one
special lexeme.

> page :: { Module }
>       : toppragmas topxml                            {% checkExpr $2 >>= mkPageModule $1 }
>       | toppragmas '<%' module '%>' srcloc topxml    {% checkExpr $6 >>= \x -> mkPage ($3 $1) $5 x }
>       | toppragmas module                            { $2 $1 }

> topxml :: { PExp }
>       : srcloc '<' name attrs mattr '>' children '</' name '>'        {% do { n <- checkEqNames $3 $9;
>                                                                               let { cn = reverse $7;
>                                                                                     as = reverse $4; };
>                                                                               return $ XTag $1 n as $5 cn } }
>       | srcloc '<' name attrs mattr '/>'                              { XETag $1 $3 (reverse $4) $5 }


> toppragmas :: { [OptionPragma] }
>           : open toppragmasaux close          { $2 }

> toppragmasaux :: { [OptionPragma] }
>               : toppragma ';' toppragmasaux         { $1 : $3 }
>               | {- nothing -}                         { [] }

> toppragma :: { OptionPragma }
>           : srcloc '{-# LANGUAGE' conids '#-}'       { LanguagePragma $1 $3 }
>           | srcloc '{-# INCLUDE' '#-}'               { IncludePragma  $1 $2 }
>           | srcloc '{-# OPTIONS' '#-}'               { let (mc, s) = $2 in OptionsPragma $1 (readTool mc) s }
>           | srcloc '{-# CFILES'  '#-}'               { CFilesPragma   $1 $2 }
>           | srcloc '{-# unknown' '#-}'               { let (n, s) = $2 in UnknownTopPragma $1 n s }

> conids    :: { [Name] }
>          : conid ',' conids                  { $1 : $3 }
>          | conid                             { [$1] }

-----------------------------------------------------------------------------
Module Header

> module :: { [OptionPragma] -> Module }
>       : srcloc 'module' modid maybemodwarning maybeexports 'where' body
>               { \os -> Module $1 $3 os $4 $5 (fst $7) (snd $7) }
>       | srcloc body
>               { \os -> Module $1 main_mod os Nothing (Just [EVar (UnQual main_name)])
>                                                       (fst $2) (snd $2) }

> maybemodwarning ::  { Maybe WarningText }
>       : '{-# DEPRECATED' STRING '#-}'         { Just $ DeprText $2 }
>       | '{-# WARNING' STRING '#-}'            { Just $ WarnText $2 }
>       | {- empty -}                           { Nothing }

> body :: { ([ImportDecl],[Decl]) }
>       : '{'  bodyaux '}'                      { $2 }
>       | open bodyaux close                    { $2 }

> bodyaux :: { ([ImportDecl],[Decl]) }
>       : optsemis impdecls semis topdecls      { (reverse $2, $4) }
>       | optsemis                topdecls      { ([], $2) }
>       | optsemis impdecls optsemis            { (reverse $2, []) }
>       | optsemis                              { ([], []) }

> semis :: { () }
>       : optsemis ';'                          { () }

> optsemis :: { () }
>       : semis                                 { () }
>       | {- empty -}                           { () }

-----------------------------------------------------------------------------
The Export List

> maybeexports :: { Maybe [ExportSpec] }
>       :  exports                              { Just $1 }
>       |  {- empty -}                          { Nothing }

> exports :: { [ExportSpec] }
>       : '(' exportlist optcomma ')'           { reverse $2 }
>       | '(' optcomma ')'                      { [] }

> optcomma :: { () }
>       : ','                                   { () }
>       | {- empty -}                           { () }

> exportlist :: { [ExportSpec] }
>       :  exportlist ',' export                { $3 : $1 }
>       |  export                               { [$1]  }

> export :: { ExportSpec }
>       :  qvar                                 { EVar $1 }
>       |  qtyconorcls                          { EAbs $1 }
>       |  qtyconorcls '(' '..' ')'             { EThingAll $1 }
>       |  qtyconorcls '(' ')'                  { EThingWith $1 [] }
>       |  qtyconorcls '(' cnames ')'           { EThingWith $1 (reverse $3) }
>       |  'module' modid                       { EModuleContents $2 }

-----------------------------------------------------------------------------
Import Declarations

> impdecls :: { [ImportDecl] }
>       : impdecls semis impdecl                { $3 : $1 }
>       | impdecl                               { [$1] }

> impdecl :: { ImportDecl }
>       : srcloc 'import' optsrc optqualified maybepkg modid maybeas maybeimpspec
>                               { ImportDecl $1 $6 $4 $3 $5 $7 $8 }

> optsrc :: { Bool }
>       : '{-# SOURCE' '#-}'                    { True }
>       | {- empty -}                           { False }

> optqualified :: { Bool }
>       : 'qualified'                           { True  }
>       | {- empty -}                           { False }

Requires the PackageImports extension enabled.
> maybepkg :: { Maybe String }
>       : STRING                                {% do { checkEnabled PackageImports ;
>                                                       return $ Just $1 } }
>       | {- empty -}                           { Nothing }

> maybeas :: { Maybe ModuleName }
>       : 'as' modid                            { Just $2 }
>       | {- empty -}                           { Nothing }


> maybeimpspec :: { Maybe (Bool, [ImportSpec]) }
>       : impspec                               { Just $1 }
>       | {- empty -}                           { Nothing }

> impspec :: { (Bool, [ImportSpec]) }
>       : opthiding '(' importlist optcomma ')' { ($1, reverse $3) }
>       | opthiding '(' optcomma ')'            { ($1, []) }

> opthiding :: { Bool }
>       : 'hiding'                              { True }
>       | {- empty -}                           { False }

> importlist :: { [ImportSpec] }
>       :  importlist ',' importspec            { $3 : $1 }
>       |  importspec                           { [$1]  }

> importspec :: { ImportSpec }
>       :  var                                  { IVar $1 }
>       |  tyconorcls                           { IAbs $1 }
>       |  tyconorcls '(' '..' ')'              { IThingAll $1 }
>       |  tyconorcls '(' ')'                   { IThingWith $1 [] }
>       |  tyconorcls '(' cnames ')'            { IThingWith $1 (reverse $3) }

> cnames :: { [CName] }
>       :  cnames ',' cname                     { $3 : $1 }
>       |  cname                                { [$1]  }

> cname :: { CName }
>       :  var                                  { VarName $1 }
>       |  con                                  { ConName $1 }

-----------------------------------------------------------------------------
Fixity Declarations

> fixdecl :: { Decl }
>       : srcloc infix prec ops                 { InfixDecl $1 $2 $3 (reverse $4) }

> prec :: { Int }
>       : {- empty -}                           { 9 }
>       | INT                                   {% checkPrec $1 }

> infix :: { Assoc }
>       : 'infix'                               { AssocNone  }
>       | 'infixl'                              { AssocLeft  }
>       | 'infixr'                              { AssocRight }

> ops   :: { [Op] }
>       : ops ',' op                            { $3 : $1 }
>       | op                                    { [$1] }

-----------------------------------------------------------------------------
Top-Level Declarations

Note: The report allows topdecls to be empty. This would result in another
shift/reduce-conflict, so we don't handle this case here, but in bodyaux.

> topdecls :: { [Decl] }
>       : topdecls1 optsemis            {% checkRevDecls $1 }

> topdecls1 :: { [Decl] }
>       : topdecls1 semis topdecl       { $3 : $1 }
>       | topdecl                       { [$1] }

> topdecl :: { Decl }
>       : srcloc 'type' dtype '=' ctype
>                       {% do { (c,ts) <- checkSimpleType $3;
>                               return (TypeDecl $1 c ts $5) } }

Requires the TypeFamilies extension enabled, but the lexer will handle
that through the 'family' keyword.
>       | srcloc 'type' 'family' type optkind
>                       {% do { (c,ts) <- checkSimpleType $4;
>                               return (TypeFamDecl $1 c ts $5) } }

Here there is no special keyword so we must do the check.
>       | srcloc 'type' 'instance' dtype '=' ctype
>                       {% do { -- no checkSimpleType $4 since dtype may contain type patterns
>                               checkEnabled TypeFamilies ;
>                               return (TypeInsDecl $1 $4 $6) } }
>       | srcloc data_or_newtype ctype constrs0 deriving
>                       {% do { (cs,c,t) <- checkDataHeader $3;
>                               checkDataOrNew $2 $4;
>                               return (DataDecl $1 $2 cs c t (reverse $4) $5) } }

Requires the GADTs extension enabled, handled in gadtlist.
>       | srcloc data_or_newtype ctype optkind 'where' gadtlist deriving
>                       {% do { (cs,c,t) <- checkDataHeader $3;
>                               checkDataOrNew $2 $6;
>                               return (GDataDecl $1 $2 cs c t $4 (reverse $6) $7) } }

Same as above, lexer will handle through the 'family' keyword.
>       | srcloc 'data' 'family' ctype optkind
>                       {% do { (cs,c,t) <- checkDataHeader $4;
>                               return (DataFamDecl $1 cs c t $5) } }

Here we must check for TypeFamilies.
>       | srcloc data_or_newtype 'instance' ctype constrs0 deriving
>                       {% do { -- (cs,c,t) <- checkDataHeader $4;
>                               checkEnabled TypeFamilies ;
>                               checkDataOrNew $2 $5;
>                               return (DataInsDecl $1 $2 $4 (reverse $5) $6) } }

This style requires both TypeFamilies and GADTs, the latter is handled in gadtlist.
>       | srcloc data_or_newtype 'instance' ctype optkind 'where' gadtlist deriving
>                       {% do { -- (cs,c,t) <- checkDataHeader $4;
>                               checkEnabled TypeFamilies ;
>                               checkDataOrNew $2 $7;
>                               return (GDataInsDecl $1 $2 $4 $5 (reverse $7) $8) } }
>       | srcloc 'class' ctype fds optcbody
>                       {% do { (cs,c,vs) <- checkClassHeader $3;
>                               return (ClassDecl $1 cs c vs $4 $5) } }
>       | srcloc 'instance' ctype optvaldefs
>                       {% do { (cs,c,ts) <- checkInstHeader $3;
>                               return (InstDecl $1 cs c ts $4) } }

Requires the StandaloneDeriving extension enabled.
>       | srcloc 'deriving' 'instance' ctype
>                       {% do { checkEnabled StandaloneDeriving ;
>                               (cs, c, ts) <- checkInstHeader $4;
>                               return (DerivDecl $1 cs c ts) } }
>       | srcloc 'default' '(' typelist ')'
>                       { DefaultDecl $1 $4 }

Requires the TemplateHaskell extension, but the lexer will handle that
throught he '$(' lexeme.
>       | srcloc '$(' trueexp ')'
>                        { SpliceDecl $1 $ ParenSplice $3 }

These require the ForeignFunctionInterface extension, handled by the
lexer through the 'foreign' (and 'export') keyword.
>       | srcloc 'foreign' 'import' callconv safety fspec
>                       { let (s,n,t) = $6 in ForImp $1 $4 $5 s n t }
>       | srcloc 'foreign' 'export' callconv fspec
>                       { let (s,n,t) = $5 in ForExp $1 $4 s n t }

>       | srcloc '{-# RULES' rules '#-}'               { RulePragmaDecl $1 $ reverse $3 }
>       | srcloc '{-# DEPRECATED' warndeprs '#-}'      { DeprPragmaDecl $1 $ reverse $3 }
>       | srcloc '{-# WARNING' warndeprs '#-}'         { WarnPragmaDecl $1 $ reverse $3 }
>       | srcloc '{-# unknown' '#-}'                   { let (n, s) = $2 in UnknownDeclPragma $1 n s }
>       | decl          { $1 }

> data_or_newtype :: { DataOrNew }
>       : 'data'    { DataType }
>       | 'newtype' { NewType  }

> typelist :: { [Type] }
>       : types                         { reverse $1 }
>       | type                          { [$1] }
>       | {- empty -}                   { [] }

> decls :: { [Decl] }
>       : optsemis decls1 optsemis      {% checkRevDecls $2 }
>       | optsemis                      { [] }

> decls1 :: { [Decl] }
>       : decls1 semis decl             { $3 : $1 }
>       | decl                          { [$1] }

> decl :: { Decl }
>       : signdecl                      { $1 }
>       | fixdecl                       { $1 }
>       | valdef                        { $1 }

> decllist :: { [Decl] }
>       : '{'  decls '}'                { $2 }
>       | open decls close              { $2 }

> signdecl :: { Decl }
>       : srcloc exp0b '::' ctype                                {% do { v <- checkSigVar $2;
>                                                                        return $ TypeSig $1 [v] $4 } }
>       | srcloc exp0b ',' vars '::' ctype                      {% do { v <- checkSigVar $2;
>                                                                       return $ TypeSig $1 (v : reverse $4) $6 } }
>       | srcloc '{-# INLINE' activation qvar '#-}'             { InlineSig $1 $2 $3 $4 }
>       | srcloc '{-# SPECIALISE' qvar '::' sigtypes '#-}'      { SpecSig $1 $3 $5 }
>       | srcloc '{-# SPECIALISE_INLINE' activation qvar '::' sigtypes '#-}'
>                                                       { SpecInlineSig $1 $2 $3 $4 $6 }
>       | srcloc '{-# SPECIALISE' 'instance' ctype '#-}'        {% do { (cs,c,ts) <- checkInstHeader $4;
>                                                                       return $ InstSig $1 cs c ts } }

> sigtypes :: { [Type] }
>       : sigtype                           { [ $1 ] }
>       | sigtype ',' sigtypes              { $1 : $3 }

> sigtype :: { Type }
>       : ctype                             { mkTyForall Nothing [] $1 }

Binding can be either of implicit parameters, or it can be a normal sequence
of declarations. The two kinds cannot be mixed within the same block of
binding.

> binds :: { Binds }
>       : decllist                      { BDecls $1 }
>       | '{' ipbinds '}'               { IPBinds $2 }
>       | open ipbinds close            { IPBinds $2 }

ATTENTION: Dirty Hackery Ahead! If the second alternative of vars is var
instead of qvar, we get another shift/reduce-conflict. Consider the
following programs:

   { (+) :: ... }          only var
   { (+) x y  = ... }      could (incorrectly) be qvar

We re-use expressions for patterns, so a qvar would be allowed in patterns
instead of a var only (which would be correct). But deciding what the + is,
would require more lookahead. So let's check for ourselves...

> vars  :: { [Name] }
>       : vars ',' var                  { $3 : $1 }
>       | qvar                          {% do { n <- checkUnQual $1;
>                                               return [n] } }

-----------------------------------------------------------------------------
FFI

These will only be called on in the presence of a 'foreign' keyword,
so no need to check for extensions.

> callconv :: { CallConv }
>          : 'stdcall'                  { StdCall }
>          | 'ccall'                    { CCall }

> safety :: { Safety }
>        : 'safe'                       { PlaySafe False }
>        | 'unsafe'                     { PlayRisky }
>        | 'threadsafe'                 { PlaySafe True }
>        | {- empty -}                  { PlaySafe False }

> fspec :: { (String, Name, Type) }
>       : STRING var_no_safety '::' dtype               { ($1, $2, $4) }
>       |        var_no_safety '::' dtype               { ("", $1, $3) }

-----------------------------------------------------------------------------
Pragmas

> rules :: { [Rule] }
>       : rules ';'rule         { $3 : $1 }
>       | rules ';'             { $1 }
>       | rule                  { [$1] }
>       | {- empty -}           { [] }

> rule :: { Rule }
>      : STRING activation ruleforall exp0 '=' trueexp      {% do { e <- checkRuleExpr $4;
>                                                                   return $ Rule $1 $2 $3 e $6 } }

> activation :: { Activation }
>        : {- empty -}                  { AlwaysActive }
>        | '[' INT ']'                  { ActiveFrom (fromInteger $2) }
>        | '[' '~' INT ']'              { ActiveUntil (fromInteger $3) }

> ruleforall :: { Maybe [RuleVar] }
>       : {- empty -}                           { Nothing }
>       | 'forall' rulevars '.'                 { Just $2 }

> rulevars :: { [RuleVar] }
>       : rulevar                       { [$1] }
>       | rulevar rulevars              { $1 : $2 }

> rulevar :: { RuleVar }
>       : varid                         { RuleVar $1 }
>       | '(' varid '::' ctype ')'      { TypedRuleVar $2 $4 }

> warndeprs :: { [([Name],String)] }
>   : warndeprs ';' warndepr        { $3 : $1 }
>   | warndeprs ';'                 { $1 }
>   | warndepr                      { [$1] }
>   | {- empty -}                   { [] }

> warndepr :: { ([Name], String) }
>       : namevars STRING       { ($1,$2) }

> namevars :: { [Name] }
>           : namevar                   { [$1] }
>           | namevar ',' namevars      { $1 : $3 }

> namevar :: { Name }
>         : con                         { $1 }
>         | var                         { $1 }

-----------------------------------------------------------------------------
Types

Type equality contraints need the TypeFamilies extension.

> dtype :: { Type }
>       : btype                         { $1 }
>       | btype qtyconop dtype          { TyInfix $1 $2 $3 }
>       | btype qtyvarop dtype          { TyInfix $1 $2 $3 } -- FIXME
>       | btype '->' ctype              { TyFun $1 $3 }
>       | btype '~' btype               {% do { checkEnabled TypeFamilies ;
>                                               return $ TyPred $ EqualP $1 $3 } }

Implicit parameters can occur in normal types, as well as in contexts.

> type :: { Type }
>       : ivar '::' dtype               { TyPred $ IParam $1 $3 }
>       | dtype                         { $1 }

> btype :: { Type }
>       : btype atype                   { TyApp $1 $2 }
>       | atype                         { $1 }

UnboxedTuples requires the extension, but that will be handled through
the (# and #) lexemes. Kinds will be handled at the kind rule.

> atype :: { Type }
>       : gtycon                        { TyCon $1 }
>       | tyvar                         { TyVar $1 }
>       | '(' types ')'                 { TyTuple Boxed (reverse $2) }
>       | '(#' types1 '#)'              { TyTuple Unboxed (reverse $2) }
>       | '[' type ']'                  { TyApp list_tycon $2 }
>       | '(' ctype ')'                 { TyParen $2 }
>       | '(' ctype '::' kind ')'       { TyKind $2 $4 }

> gtycon :: { QName }
>       : otycon                        { $1 }
>       | '(' ')'                       { unit_tycon_name }
>       | '(' '->' ')'                  { fun_tycon_name }
>       | '[' ']'                       { list_tycon_name }
>       | '(' commas ')'                { tuple_tycon_name Boxed $2 }
>       | '(#' '#)'                     { unboxed_singleton_tycon_name }
>       | '(#' commas '#)'              { tuple_tycon_name Unboxed $2 }

> otycon :: { QName }
>       : qconid                        { $1 }
>       | '(' gconsym ')'               { $2 }

These are for infix types

> qtyconop :: { QName }
>       : qconop                        { $1 }


(Slightly edited) Comment from GHC's hsparser.y:
"context => type" vs  "type" is a problem, because you can't distinguish between

        foo :: (Baz a, Baz a)
        bar :: (Baz a, Baz a) => [a] -> [a] -> [a]

with one token of lookahead.  The HACK is to parse the context as a btype
(more specifically as a tuple type), then check that it has the right form
C a, or (C1 a, C2 b, ... Cn z) and convert it into a context.  Blaach!

Forall-quantified types require some extension to enable them, but
that requires the 'forall' keyword, so the lexer will handle it.

> ctype :: { Type }
>       : 'forall' ktyvars '.' ctype    { mkTyForall (Just $2) [] $4 }
>       | context '=>' type             { mkTyForall Nothing $1 $3 }
>       | type                          { $1 }

Equality constraints require the TypeFamilies extension.

> context :: { Context }
>       : btype                         {% checkContext $1 }
>       | btype '~' btype               {% checkEnabled TypeFamilies >> checkContext (TyPred $ EqualP $1 $3) }

> types :: { [Type] }
>       : types1 ',' type               { $3 : $1 }

> types1 :: { [Type] }
>       : type                          { [$1] }
>       | types1 ',' type               { $3 : $1 }

> ktyvars :: { [TyVarBind] }
>       : ktyvars ktyvar                { $2 : $1 }
>       | {- empty -}                   { [] }

> ktyvar :: { TyVarBind }
>       : tyvar                         { UnkindedVar $1 }
>       | '(' tyvar '::' kind ')'       { KindedVar $2 $4 }

> tyvars :: { [Name] }
>       : tyvars tyvar                  { $2 : $1 }
>       | {- empty -}                   { [] }


-----------------------------------------------------------------------------
Functional Dependencies

These require the FunctionalDependencies extension to be enabled.

> fds :: { [FunDep] }
>       : {- empty -}                   { [] }
>       | '|' fds1                      {% checkEnabled FunctionalDependencies >> return (reverse $2) }

> fds1 :: { [FunDep] }
>       : fds1 ',' fd                   { $3 : $1 }
>       | fd                            { [$1] }

> fd :: { FunDep }
>       : tyvars '->' tyvars            { FunDep (reverse $1) (reverse $3) }

-----------------------------------------------------------------------------
Datatype declarations

GADTs - require the GADTs extension enabled, but we handle that at the calling site.

> gadtlist :: { [GadtDecl] }
>       : gadtlist1                 {% checkEnabled GADTs >> return $1 }

> gadtlist1 :: { [GadtDecl] }
>       : '{' gadtconstrs1 '}'                  { $2 }
>       | open gadtconstrs1 close               { $2 }

> gadtconstrs1 :: { [GadtDecl] }
>       : optsemis gadtconstrs optsemis         { $2 }

> gadtconstrs :: { [GadtDecl] }
>       : gadtconstrs semis gadtconstr          { $3 : $1 }
>       | gadtconstr                            { [$1] }

> gadtconstr :: { GadtDecl }
>       : srcloc qcon '::' ctype                {% do { c <- checkUnQual $2;
>                                                       return $ GadtDecl $1 c $4 } }

> constrs0 :: { [QualConDecl] }
>       : {- empty -}                   { [] }
>       | '=' constrs                   { $2 }

> constrs :: { [QualConDecl] }
>       : constrs '|' constr            { $3 : $1 }
>       | constr                        { [$1] }

> constr :: { QualConDecl }
>       : srcloc forall context '=>' constr1    { QualConDecl $1 $2 $3 $5 }
>       | srcloc forall constr1                 { QualConDecl $1 $2 [] $3 }

> forall :: { [TyVarBind] }
>       : 'forall' ktyvars '.'          { $2 }
>       | {- empty -}                   { [] }

To avoid conflicts when introducing type operators, we need to parse record constructors
as qcon and then check separately that they are truly unqualified.

> constr1 :: { ConDecl }
>       : scontype                      { ConDecl (fst $1) (snd $1) }
>       | sbtype conop sbtype           { InfixConDecl $1 $2 $3 }
>       | qcon '{' '}'                  {% do { c <- checkUnQual $1; return $ RecDecl c [] } }
>       | qcon '{' fielddecls '}'       {% do { c <- checkUnQual $1; return $ RecDecl c (reverse $3) } }

> scontype :: { (Name, [BangType]) }
>       : btype                         {% do { (c,ts) <- splitTyConApp $1;
>                                               return (c,map UnBangedTy ts) } }
>       | scontype1                     { $1 }

> scontype1 :: { (Name, [BangType]) }
>       : btype '!' atype                       {% do { (c,ts) <- splitTyConApp $1;
>                                                       return (c,map UnBangedTy ts++
>                                                               [BangedTy $3]) } }
>       | btype '{-# UNPACK' '#-}' '!' atype    {% do { (c,ts) <- splitTyConApp $1;
>                                                       return (c,map UnBangedTy ts++
>                                                               [UnpackedTy $5]) } }
>       | scontype1 satype              { (fst $1, snd $1 ++ [$2] ) }

> satype :: { BangType }
>       : atype                         { UnBangedTy $1 }
>       | '!' atype                     { BangedTy   $2 }
>       | '{-# UNPACK' '#-}' '!' atype  { UnpackedTy $4 }

> sbtype :: { BangType }
>       : btype                         { UnBangedTy $1 }
>       | '!' atype                     { BangedTy   $2 }
>       | '{-# UNPACK' '#-}' '!' atype  { UnpackedTy $4 }

> fielddecls :: { [([Name],BangType)] }
>       : fielddecls ',' fielddecl      { $3 : $1 }
>       | fielddecl                     { [$1] }

> fielddecl :: { ([Name],BangType) }
>       : vars '::' stype               { (reverse $1, $3) }

> stype :: { BangType }
>       : ctype                         { UnBangedTy $1 }
>       | '!' atype                     { BangedTy   $2 }
>       | '{-# UNPACK' '#-}' '!' atype  { UnpackedTy $4 }

> deriving :: { [Deriving] }
>       : {- empty -}                   { [] }
>       | 'deriving' qtycls1            { [($2, [])] }
>       | 'deriving' '('          ')'   { [] }
>       | 'deriving' '(' dclasses ')'   { reverse $3 }

> dclasses :: { [Deriving] }
>       : dclasses ',' qtycls           { $3 : $1 }
>       | qtycls                        { [$1] }

> qtycls :: { Deriving }
>       : qtycls1               { ($1, []) }
>       | qconid tyconvars      { ($1, reverse $2) }

> tyconvars :: { [QName] }
>       : tyconvars tyconvar    { $2 : $1 }
>       | tyconvar              { [$1] }

> tyconvar :: { QName }
>       : qconid                { $1 }
>       | qvarid                { $1 }

> qtycls1 :: { QName }
>       : qconid                { $1 }


-----------------------------------------------------------------------------
Kinds

> kind :: { Kind }
>       : kind1             {% checkEnabled KindSignatures >> return $1 }

> kind1 :: { Kind }
>       : akind                 { $1 }
>       | akind '->' kind1      { KindFn $1 $3 }

> akind :: { Kind }
>       : '*'                   { KindStar  }
>       | '!'                   { KindBang  }
>       | '(' kind1 ')'         { $2 }

> optkind :: { Maybe Kind }
>       : {-empty-}             { Nothing }
>       | '::' kind             { Just $2 }
-----------------------------------------------------------------------------
Class declarations

No implicit parameters in the where clause of a class declaration.
> optcbody :: { [ClassDecl] }
>       : 'where' cldecllist            {% checkClassBody $2 }
>       | {- empty -}                   { [] }

> cldecllist :: { [ClassDecl] }
>       : '{'  cldecls '}'              { $2 }
>       | open cldecls close            { $2 }

> cldecls :: { [ClassDecl] }
>       : optsemis cldecls1 optsemis    {% checkRevClsDecls $2 }
>       | optsemis                      { [] }

> cldecls1 :: { [ClassDecl] }
>       : cldecls1 semis cldecl         { $3 : $1 }
>       | cldecl                        { [$1] }

Associated types require the TypeFamilies extension.

> cldecl :: { ClassDecl }
>       : decl                          { ClsDecl $1 }
>       | atdecl                        {% checkEnabled TypeFamilies >> return $1 }

> atdecl :: { ClassDecl }
>       : srcloc 'type' type optkind
>               {% do { (c,ts) <- checkSimpleType $3;
>                       return (ClsTyFam $1 c ts $4) } }
>       | srcloc 'type' dtype '=' ctype
>                       { ClsTyDef $1 $3 $5 }
>       | srcloc 'data' ctype optkind
>                {% do { (cs,c,t) <- checkDataHeader $3;
>                        return (ClsDataFam $1 cs c t $4) } }

-----------------------------------------------------------------------------
Instance declarations

> optvaldefs :: { [InstDecl] }
>       : 'where' '{'  valdefs '}'      {% checkInstBody $3 }
>       | 'where' open valdefs close    {% checkInstBody $3 }
>       | {- empty -}                   { [] }

> valdefs :: { [InstDecl] }
>       : optsemis valdefs1 optsemis    {% checkRevInstDecls $2 }
>       | optsemis                      { [] }

> valdefs1 :: { [InstDecl] }
>       : valdefs1 semis insvaldef      { $3 : $1 }
>       | insvaldef                     { [$1] }

Associated types require the TypeFamilies extension enabled.

> insvaldef :: { InstDecl }
>       : valdef                        { InsDecl $1 }
>       | atinst                        {% checkEnabled TypeFamilies >> return $1 }
>       | inlinst                       { $1 }

> inlinst :: { InstDecl }
>       : srcloc '{-# INLINE' activation qvar '#-}'     { InsInline $1 $2 $3 $4 }

> atinst :: { InstDecl }
>       : srcloc 'type' dtype '=' ctype
>                       {% do { -- no checkSimpleType $4 since dtype may contain type patterns
>                               return (InsType $1 $3 $5) } }
>       | srcloc data_or_newtype ctype constrs0 deriving
>                       {% do { -- (cs,c,t) <- checkDataHeader $4;
>                               checkDataOrNew $2 $4;
>                               return (InsData $1 $2 $3 (reverse $4) $5) } }
>       | srcloc data_or_newtype ctype optkind 'where' gadtlist deriving
>                       {% do { -- (cs,c,t) <- checkDataHeader $4;
>                               checkDataOrNew $2 $6;
>                               return (InsGData $1 $2 $3 $4 (reverse $6) $7) } }

-----------------------------------------------------------------------------
Value definitions

> valdef :: { Decl }
>       : srcloc exp0b optsig rhs optwhere     {% checkValDef $1 $2 $3 $4 $5 }
>       | srcloc '!' aexp rhs optwhere         {% do { checkEnabled BangPatterns ;
>                                                      p <- checkPattern $3;
>                                                      return $ PatBind $1 p Nothing $4 $5 } }

May bind implicit parameters
> optwhere :: { Binds }
>       : 'where' binds                 { $2 }
>       | {- empty -}                   { BDecls [] }

Type signatures on value definitions require ScopedTypeVariables (or PatternSignatures, which is deprecated).

> optsig :: { Maybe Type }
>       : '::' ctype                    {% checkEnabled ScopedTypeVariables >> return (Just $2) }
>       | {- empty -}                   { Nothing }

> rhs   :: { Rhs }
>       : '=' trueexp                   { UnGuardedRhs $2 }
>       | gdrhs                         { GuardedRhss  (reverse $1) }

> gdrhs :: { [GuardedRhs] }
>       : gdrhs gdrh                    { $2 : $1 }
>       | gdrh                          { [$1] }

Guards may contain patterns if PatternGuards is enabled, hence quals instead of exp.
> gdrh :: { GuardedRhs }
>       : srcloc '|' quals '=' trueexp  {% checkPatternGuards $3 >> return (GuardedRhs $1 (reverse $3) $5) }

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

> trueexp :: { Exp }
>         : exp                 {% checkExpr $1 }

> exp   :: { PExp }
>       : exp0b '::' srcloc ctype       { ExpTypeSig $3 $1 $4 }
>       | exp0                          { $1 }
>       | exp0b qop                     { PostOp $1 $2 }
>       | exp0b '-<' exp                { LeftArrApp $1 $3 }
>       | exp0b '>-' exp                { RightArrApp $1 $3 }
>       | exp0b '-<<' exp               { LeftArrHighApp $1 $3 }
>       | exp0b '>>-' exp               { RightArrHighApp $1 $3 }

> exp0 :: { PExp }
>       : exp0a                         { $1 }
>       | exp0b                         { $1 }

> exp0a :: { PExp }
>       : exp0b qop exp10a              { InfixApp $1 $2 $3 }
>       | exp10a                        { $1 }

Hyphenated identifiers require XmlSyntax to be enabled, handled in the lexer.

> exp0b :: { PExp }
>       : exp0b qop exp10b              { InfixApp $1 $2 $3 }
>       | dvarexp                       { $1 }
>       | exp10b                        { $1 }

> exp10a :: { PExp }
>       : '\\' srcloc apats '->' exp    { Lambda $2 (reverse $3) $5 }
A let may bind implicit parameters
>       | 'let' binds 'in' exp          { Let $2 $4 }
>       | 'if' exp 'then' exp 'else' exp { If $2 $4 $6 }
>       | 'proc' apat '->' exp          { Proc $2 $4 }

mdo blocks require the RecursiveDo extension enabled, but the lexer handles that.

> exp10b :: { PExp }
>       : 'case' exp 'of' altslist      { Case $2 $4 }
>       | '-' fexp                      { NegApp $2 }
>       | 'do' stmtlist                 { Do $2 }
>       | 'mdo' stmtlist                { MDo $2 }
>       | exppragma                     { $1 }
>       | fexp                          { $1 }

> exppragma :: { PExp }
>       : '{-# CORE' STRING '#-}'       { CorePragma $2 }
>       | '{-# SCC' STRING '#-}'        { SCCPragma $2 }
>       | '{-# GENERATED' STRING INT ':' INT '-' INT ':' INT '#-}'
>                                       { GenPragma $2 (fromInteger $3, fromInteger $5)
>                                                      (fromInteger $7, fromInteger $9) }
>       | '{-# unknown' '#-}'           { let (n, s) = $1 in UnknownExpPragma n s }

> fexp :: { PExp }
>       : fexp aexp                     { App $1 $2 }
>       | aexp                          { $1 }

> apats :: { [Pat] }
>       : apats apat                    { $2 : $1 }
>       | apat                          { [$1] }

> apat :: { Pat }
>       : aexp                          {% checkPattern $1 }
>       | '!' aexp                      {% checkPattern (BangPat $2) }

UGLY: Because patterns and expressions are mixed, aexp has to be split into
two rules: One right-recursive and one left-recursive. Otherwise we get two
reduce/reduce-errors (for as-patterns and irrefutable patters).

Even though the variable in an as-pattern cannot be qualified, we use
qvar here to avoid a shift/reduce conflict, and then check it ourselves
(as for vars above).

Non-linear name binding, @:, requires RegularPatterns, but the lexer handles that.

> aexp  :: { PExp }
>       : qvar '@' aexp                 {% do { n <- checkUnQual $1;
>                                               return (AsPat n $3) } }
>       | qvar '@:' aexp                {% do { n <- checkUnQual $1;
>                                               return (CAsRP n $3) } }
>       | '~' aexp                      { IrrPat $2 }
>       | aexp1                         { $1 }

Note: The first two alternatives of aexp1 are not necessarily record
updates: they could be labeled constructions.
Generics-style explicit type arguments need the Generics extension, but
we check that in the lexer.

> aexp1 :: { PExp }
>       : aexp1 '{' '}'                 {% mkRecConstrOrUpdate $1 [] }
>       | aexp1 '{' fbinds '}'          {% mkRecConstrOrUpdate $1 (reverse $3) }
>       | qvar '{|' type '|}'           { ExplTypeArg $1 $3 }
>       | aexp2                         { $1 }

According to the Report, the left section (e op) is legal iff (e op x)
parses equivalently to ((e) op x).  Thus e must be an exp0b.
An implicit parameter can be used as an expression, enabled by the lexer.
Extensions using banana brackets are also enabled by the lexer. The only
thing we need to look at here is the erpats that use no non-standard lexemes.

> aexp2 :: { PExp }
>       : ivar                          { IPVar $1 }
>       | qvar                          { Var $1 }
>       | gcon                          { $1 }
>       | literal                       { Lit $1 }
>       | '(' texp ')'                  { Paren $2 }
>       | '(' texp ',' texps ')'        { Tuple ($2 : reverse $4) }
>       | '[' list ']'                  { $2 }
We parse left sections as PostOp instead, and post-mangle them, see above
        | '(' exp0b rqop ')'            { LeftSection $2 $3  } -- this line is commented out
>       | '(' qopm exp0 ')'             { RightSection $2 $3 }
>       | '_'                           { WildCard }
>       | '(' erpats ')'                {% checkEnabled RegularPatterns >> return $2 }
>       | '(|' sexps '|)'               { SeqRP $ reverse $2 }
>       | '(|' exp '|' quals '|)'       { GuardRP $2 $ reverse $4 }
>       | xml                           { $1 }

Template Haskell - all this is enabled in the lexer.
>       | IDSPLICE                      { SpliceExp $ IdSplice $1 }
>       | '$(' trueexp ')'              { SpliceExp $ ParenSplice $2 }
>       | '[|' trueexp '|]'             { BracketExp $ ExpBracket $2 }
>       | '[p|' exp0 '|]'               {% do { p <- checkPattern $2;
>                                               return $ BracketExp $ PatBracket p } }
>       | '[t|' ctype '|]'              { BracketExp $ TypeBracket $2 }
>       | '[d|' open topdecls close '|]'        { BracketExp $ DeclBracket $3 }
>       | VARQUOTE qvar                 { VarQuote $2 }
>       | VARQUOTE qcon                 { VarQuote $2 }
>       | TYPQUOTE tyvar                { TypQuote (UnQual $2) }
>       | TYPQUOTE gtycon               { TypQuote $2 }
>       | QUASIQUOTE                    { let (n,q) = $1 in QuasiQuote n q }
End Template Haskell

> commas :: { Int }
>       : commas ','                    { $1 + 1 }
>       | ','                           { 1 }

> texps :: { [PExp] }
>       : texps ',' texp                { $3 : $1 }
>       | texp                          { [$1] }

> texp :: { PExp }
>       : exp                           { $1 }
>       | exp '->' exp                  { ViewPat $1 $3 }

-----------------------------------------------------------------------------
Harp Extensions

> sexps :: { [PExp] }
>       : sexps ',' exp                 { $3 : $1 }
>       | exp                           { [$1] }

Either patterns are left associative
> erpats :: { PExp }
>       : exp '|' erpats              { EitherRP $1 $3 }
>       | exp '|' exp                 { EitherRP $1 $3 }

-----------------------------------------------------------------------------
Hsx Extensions - requires XmlSyntax, but the lexer handles all that.

> xml :: { PExp }
>       : srcloc '<' name attrs mattr '>' children '</' name '>'        {% do { n <- checkEqNames $3 $9;
>                                                                               let { cn = reverse $7;
>                                                                                     as = reverse $4; };
>                                                                               return $ XTag $1 n as $5 cn } }
>       | srcloc '<' name attrs mattr '/>'                              { XETag $1 $3 (reverse $4) $5 }
>       | '<%' exp '%>'                                                 { XExpTag $2 }

> children :: { [PExp] }
>       : children child                { $2 : $1 }
>       | {- empty -}                   { [] }

> child :: { PExp }
>       : PCDATA                        { XPcdata $1 }
>       | '<[' sexps ']>'               { XRPats $ reverse $2 }
>       | xml                           { $1 }

> name :: { XName }
>       : xmlname ':' xmlname           { XDomName $1 $3 }
>       | xmlname                       { XName $1 }

> xmlname :: { String }
>       : VARID                         { $1 }
>       | CONID                         { $1 }
>       | DVARID                        { mkDVar $1 }
>       | xmlkeyword                    { $1 }

> xmlkeyword :: { String }
>       : 'type'                        { "type" }
>       | 'class'                       { "class" }
>       | 'data'                        { "data" }
>       | 'foreign'                     { "foreign" }
>       | 'export'                      { "export" }
>       | 'safe'                        { "safe" }
>       | 'unsafe'                      { "unsafe" }
>       | 'threadsafe'                  { "threadsafe" }
>       | 'stdcall'                     { "stdcall" }
>       | 'ccall'                       { "ccall" }
>       | 'as'                          { "as" }
>       | 'case'                        { "case" }
>       | 'default'                     { "default" }
>       | 'deriving'                    { "deriving" }
>       | 'do'                          { "do" }
>       | 'else'                        { "else" }
>       | 'family'                      { "family" }
>       | 'forall'                      { "forall" }
>       | 'hiding'                      { "hiding" }
>       | 'if'                          { "if" }
>       | 'import'                      { "import" }
>       | 'in'                          { "in" }
>       | 'infix'                       { "infix" }
>       | 'infixl'                      { "infixl" }
>       | 'infixr'                      { "infixr" }
>       | 'instance'                    { "instance" }
>       | 'let'                         { "let" }
>       | 'mdo'                         { "mdo" }
>       | 'module'                      { "module" }
>       | 'newtype'                     { "newtype" }
>       | 'of'                          { "of" }
>       | 'then'                        { "then" }
>       | 'where'                       { "where" }
>       | 'qualified'                   { "qualified" }


> attrs :: { [ParseXAttr] }
>       : attrs attr                    { $2 : $1 }
>       | {- empty -}                   { [] }

> attr :: { ParseXAttr }
>       : name '=' aexp                 { XAttr $1 $3 }

> mattr :: { Maybe PExp }
>       : aexp                          { Just $1 }
>       | {-empty-}                     { Nothing }

Turning dash variables into infix expressions with '-'
> dvarexp :: { PExp }
>         : DVARID                      { mkDVarExpr $1 }

-----------------------------------------------------------------------------
List expressions

The rules below are little bit contorted to keep lexps left-recursive while
avoiding another shift/reduce-conflict.

> list :: { PExp }
>       : texp                          { List [$1] }
>       | lexps                         { List (reverse $1) }
>       | texp '..'                     { EnumFrom $1 }
>       | texp ',' exp '..'             { EnumFromThen $1 $3 }
>       | texp '..' exp                 { EnumFromTo $1 $3 }
>       | texp ',' exp '..' exp         { EnumFromThenTo $1 $3 $5 }
>       | texp '|' quals                { ListComp $1 (reverse $3) }

> lexps :: { [PExp] }
>       : lexps ',' texp                { $3 : $1 }
>       | texp ',' texp                 { [$3,$1] }

-----------------------------------------------------------------------------
List comprehensions

 quals :: { [Stmt] }
       : quals1                         {% mapM checkStmt $1 }

> quals :: { [Stmt] }
>       : quals ',' qual                { $3 : $1 }
>       | qual                          { [$1] }

> qual  :: { Stmt }
>       : pat srcloc '<-' trueexp       { Generator $2 $1 $4 }
>       | trueexp                       { Qualifier $1 }
>       | 'let' binds                   { LetStmt $2 }

-----------------------------------------------------------------------------
Case alternatives

> altslist :: { [Alt] }
>       : '{'  alts '}'                 { $2 }
>       | open alts close               { $2 }

> alts :: { [Alt] }
>       : optsemis alts1 optsemis       { reverse $2 }

> alts1 :: { [Alt] }
>       : alts1 semis alt               { $3 : $1 }
>       | alt                           { [$1] }

> alt :: { Alt }
>       : srcloc pat ralt optwhere      { Alt $1 $2 $3 $4 }

> ralt :: { GuardedAlts }
>       : '->' trueexp                  { UnGuardedAlt $2 }
>       | gdpats                        { GuardedAlts (reverse $1) }

> gdpats :: { [GuardedAlt] }
>       : gdpats gdpat                  { $2 : $1 }
>       | gdpat                         { [$1] }

A guard can be a pattern guard if PatternGuards is enabled, hence quals instead of exp0.
> gdpat :: { GuardedAlt }
>       : srcloc '|' quals '->' trueexp {% checkPatternGuards $3 >> return (GuardedAlt $1 (reverse $3) $5) }

> pat :: { Pat }
>       : exp                           {% checkPattern $1 }
>       | '!' aexp                      {% checkPattern (BangPat $2) }
-----------------------------------------------------------------------------
Statement sequences

As per the Report, but with stmt expanded to simplify building the list
without introducing conflicts.  This also ensures that the last stmt is
an expression.

> stmtlist :: { [Stmt] }
>       : '{'  stmts '}'                { $2 }
>       | open stmts close              { $2 }

A let statement may bind implicit parameters.
> stmts :: { [Stmt] }
>       : 'let' binds ';' stmts             { LetStmt $2 : $4 }
>       | pat srcloc '<-' trueexp ';' stmts { Generator $2 $1 $4 : $6 }
>       | trueexp ';' stmts                 { Qualifier $1 : $3 }
>       | ';' stmts                         { $2 }
>       | trueexp ';'                       { [Qualifier $1] }
>       | trueexp                           { [Qualifier $1] }
>       | 'rec' stmtlist ';' stmts          { RecStmt $2 : $4 }

-----------------------------------------------------------------------------
Record Field Update/Construction

> fbinds :: { [PFieldUpdate] }
>       : fbinds ',' fbind              { $3 : $1 }
>       | fbind                         { [$1] }

Puns and wild cards need the respective extensions enabled.

> fbind :: { PFieldUpdate }
>       : qvar '=' exp                  { FieldUpdate $1 $3 }
>       | var                           {% checkEnabled NamedFieldPuns >> return (FieldPun $1) }
>       | '..'                          {% checkEnabled RecordWildCards >> return FieldWildcard }

-----------------------------------------------------------------------------
Implicit parameter bindings - need the ImplicitParameter extension enabled, but the lexer handles that.

> ipbinds :: { [IPBind] }
>       : optsemis ipbinds1 optsemis    { reverse $2 }

> ipbinds1 :: { [IPBind] }
>       : ipbinds1 semis ipbind         { $3 : $1 }
>       | ipbind                        { [$1] }

> ipbind :: { IPBind }
>       : srcloc ivar '=' trueexp       { IPBind $1 $2 $4 }

-----------------------------------------------------------------------------
Variables, Constructors and Operators.

> gcon :: { PExp }
>       : '(' ')'               { p_unit_con }
>       | '[' ']'               { List [] }
>       | '(' commas ')'        { p_tuple_con Boxed $2 }
>       | '(#' '#)'             { p_unboxed_singleton_con }
>       | '(#' commas '#)'      { p_tuple_con Unboxed $2 }
>       | qcon                  { Con $1 }

> var   :: { Name }
>       : varid                 { $1 }
>       | '(' varsym ')'        { $2 }

> var_no_safety :: { Name }
>               : varid_no_safety       { $1 }
>               | '(' varsym ')'        { $2 }

> qvar  :: { QName }
>       : qvarid                { $1 }
>       | '(' qvarsym ')'       { $2 }

Implicit parameter
> ivar  :: { IPName }
>       : ivarid                { $1 }

> con   :: { Name }
>       : conid                 { $1 }
>       | '(' consym ')'        { $2 }

> qcon  :: { QName }
>       : qconid                { $1 }
>       | '(' gconsym ')'       { $2 }

> varop :: { Name }
>       : varsym                { $1 }
>       | '`' varid '`'         { $2 }

> qvarop :: { QName }
>       : qvarsym               { $1 }
>       | '`' qvarid '`'        { $2 }

> qvaropm :: { QName }
>       : qvarsymm              { $1 }
>       | '`' qvarid '`'        { $2 }

> conop :: { Name }
>       : consym                { $1 }
>       | '`' conid '`'         { $2 }

> qconop :: { QName }
>       : gconsym               { $1 }
>       | '`' qconid '`'        { $2 }

> op    :: { Op }
>       : varop                 { VarOp $1 }
>       | conop                 { ConOp $1 }

> qop   :: { QOp }
>       : qvarop                { QVarOp $1 }
>       | qconop                { QConOp $1 }

> qopm  :: { QOp }
>       : qvaropm               { QVarOp $1 }
>       | qconop                { QConOp $1 }

> gconsym :: { QName }
>       : ':'                   { list_cons_name }
>       | qconsym               { $1 }

-----------------------------------------------------------------------------
Identifiers and Symbols

> qvarid :: { QName }
>       : varid                 { UnQual $1 }
>       | QVARID                { Qual (ModuleName (fst $1)) (Ident (snd $1)) }

> varid_no_safety :: { Name }
>       : VARID                 { Ident $1 }
>       | 'as'                  { as_name }
>       | 'qualified'           { qualified_name }
>       | 'hiding'              { hiding_name }
>       | 'export'              { export_name }
>       | 'stdcall'             { stdcall_name }
>       | 'ccall'               { ccall_name }

> varid :: { Name }
>       : varid_no_safety       { $1 }
>       | 'safe'                { safe_name }
>       | 'unsafe'              { unsafe_name }
>       | 'threadsafe'          { threadsafe_name }


Implicit parameter
> ivarid :: { IPName }
>       : IDUPID                { IPDup $1 }
>       | ILINID                { IPLin $1 }

> qconid :: { QName }
>       : conid                 { UnQual $1 }
>       | QCONID                { Qual (ModuleName (fst $1)) (Ident (snd $1)) }

> conid :: { Name }
>       : CONID                 { Ident $1 }

> qconsym :: { QName }
>       : consym                { UnQual $1 }
>       | QCONSYM               { Qual (ModuleName (fst $1)) (Symbol (snd $1)) }

> consym :: { Name }
>       : CONSYM                { Symbol $1 }

> qvarsym :: { QName }
>       : varsym                { UnQual $1 }
>       | qvarsym1              { $1 }

> qvarsymm :: { QName }
>       : varsymm               { UnQual $1 }
>       | qvarsym1              { $1 }

> varsym :: { Name }
>       : VARSYM                { Symbol $1 }
>       | '-'                   { minus_name }
>       | '!'                   { bang_name }
>       | '.'                   { dot_name }
>       | '*'                   { star_name }

> varsymm :: { Name } -- varsym not including '-'
>       : VARSYM                { Symbol $1 }
>       | '!'                   { bang_name }
>       | '.'                   { dot_name }
>       | '*'                   { star_name }

> qvarsym1 :: { QName }
>       : QVARSYM               { Qual (ModuleName (fst $1)) (Symbol (snd $1)) }

> literal :: { Literal }
>       : INT                   { Int $1 }
>       | CHAR                  { Char $1 }
>       | RATIONAL              { Frac $1 }
>       | STRING                { String $1 }
>       | PRIMINT               { PrimInt $1 }
>       | PRIMWORD              { PrimWord $1 }
>       | PRIMFLOAT             { PrimFloat $1 }
>       | PRIMDOUBLE            { PrimDouble $1 }
>       | PRIMCHAR              { PrimChar $1 }
>       | PRIMSTRING            { PrimString $1 }


> srcloc :: { SrcLoc }  :       {% getSrcLoc }

-----------------------------------------------------------------------------
Layout

> open  :: { () }       :       {% pushCurrentContext }

> close :: { () }
>       : vccurly               { () } -- context popped in lexer.
>       | error                 {% popContext }

-----------------------------------------------------------------------------
Miscellaneous (mostly renamings)

> modid :: { ModuleName }
>       : CONID                 { ModuleName $1 }
>       | QCONID                { ModuleName (fst $1 ++ '.':snd $1) }

> tyconorcls :: { Name }
>       : con                   { $1 }

 tycon :: { Name }
        : conid                 { $1 }

> qtyconorcls :: { QName }
>       : qcon                  { $1 }

> tyvar :: { Name }
>       : varid                 { $1 }

> qtyvarop :: { QName }
> qtyvarop : '`' tyvar '`'       { UnQual $2 }
>          | tyvarsym            { UnQual $1 }

> tyvarsym :: { Name }
> tyvarsym : VARSYM              { Symbol $1 }

-----------------------------------------------------------------------------

> {
> happyError :: P a
> happyError = fail "Parse error"

> -- | Parse of a string, which should contain a complete Haskell module.
> parseModule :: String -> ParseResult Module
> parseModule = runParser parse

> -- | Parse of a string containing a Haskell expression.
> parseExp :: String -> ParseResult Exp
> parseExp = runParser mparseExp

> -- | Parse of a string containing a Haskell pattern.
> parsePat :: String -> ParseResult Pat
> parsePat = runParser mparsePat

> -- | Parse of a string containing a Haskell top-level declaration.
> parseDecl :: String -> ParseResult Decl
> parseDecl = runParser mparseDecl

> -- | Parse of a string containing a Haskell type.
> parseType :: String -> ParseResult Type
> parseType = runParser mparseType

> -- | Parse of a string starting with a series of top-level option pragmas.
> getTopPragmas :: String -> ParseResult [OptionPragma]
> getTopPragmas = runParser mfindOptPragmas

> -- | Parse of a string, which should contain a complete Haskell 98 module.
> parseModuleWithMode :: ParseMode -> String -> ParseResult Module
> parseModuleWithMode mode = runParserWithMode mode parse
> }

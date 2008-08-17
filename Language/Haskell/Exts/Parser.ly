> {
> -----------------------------------------------------------------------------
> -- |
> -- Module      :  Language.Haskell.Exts.Parser
> -- Original    :  Language.Haskell.Parser
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
>               ParseMode(..), defaultParseMode, ParseResult(..)) where
> 
> import Language.Haskell.Exts.Syntax
> import Language.Haskell.Exts.ParseMonad
> import Language.Haskell.Exts.Lexer
> import Language.Haskell.Exts.ParseUtils
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
Conflicts: 5 shift/reduce

2 for ambiguity in 'case x of y | let z = y in z :: Bool -> b'
        (don't know whether to reduce 'Bool' as a btype or shift the '->'.
         Similarly lambda and if. The default resolution in favour of the
         shift means that a guard can never end with a type signature.
         In mitigation: it's a rare case and no Haskell implementation
         allows these, because it would require unbounded lookahead.)
        There are 2 conflicts rather than one because contexts are parsed
        as btypes (cf ctype).
        
1 for ambiguity in 'let ?x ...'
        the parser can't tell whether the ?x is the lhs of a normal binding or
        an implicit binding. Fortunately resolving as shift gives it the only
        sensible meaning, namely the lhs of an implicit binding.

1 for ambiguity using hybrid modules
        For HSP pages that start with a <% %> block, the parser cannot tell whether
        to reduce a srcloc or shift the starting <%. Since any other body could not
        start with <%, shifting is the only sensible thing to do.

1 for ambiguity using toplevel xml modules
        For HSP xml pages starting with a <, the parser cannot tell whether to shift
        that < or reduce an implicit 'open'. Since no other body could possibly start
        with <, shifting is the only sensible thing to do.

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

Symbols

>       '('     { LeftParen }
>       ')'     { RightParen }
>       '(#'    { LeftHashParen }
>       '#)'    { RightHashParen }
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

Harp

       '(/'    { RPSeqOpen }
       '/)'    { RPSeqClose }
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

       'reifyDecl'     { THReifyDecl }
       'reifyType'     { THReifyType }
       'reifyFixity'   { THReifyFixity }

Hsx

>       PCDATA          { XPcdata $$ }
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
>       'dlet'          { KW_DLet }     -- implicit parameter binding clause
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
>       'then'          { KW_Then }
>       'type'          { KW_Type }
>       'where'         { KW_Where }
>       'with'          { KW_With }     -- implicit parameter binding clause
>       'qualified'     { KW_Qualified }

> %monad { P }
> %lexer { lexer } { EOF }
> %name parse
> %tokentype { Token }
> %expect 5
> %%

-----------------------------------------------------------------------------
HSP Pages

> page :: { HsModule }
>       : topxml                                {% mkPageModule $1 }
>       | '<%' module '%>' srcloc topxml        {% mkPage $2 $4 $5 }
>       | module                                { $1 }

> topxml :: { HsExp }
>       : srcloc '<' name attrs mattr '>' children '</' name '>'        {% do { n <- checkEqNames $3 $9;
>                                                                               let { cn = reverse $7;
>                                                                                     as = reverse $4; };
>                                                                               return $ HsXTag $1 n as $5 cn } }
>       | srcloc '<' name attrs mattr '/>'                              { HsXETag $1 $3 (reverse $4) $5 }


-----------------------------------------------------------------------------
Module Header

> module :: { HsModule }
>       : srcloc 'module' modid maybeexports 'where' body
>               { HsModule $1 $3 $4 (fst $6) (snd $6) }
>       | srcloc body
>               { HsModule $1 main_mod (Just [HsEVar (UnQual main_name)])
>                                                       (fst $2) (snd $2) }

> body :: { ([HsImportDecl],[HsDecl]) }
>       : '{'  bodyaux '}'                      { $2 }
>       | open bodyaux close                    { $2 }

> bodyaux :: { ([HsImportDecl],[HsDecl]) }
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

> maybeexports :: { Maybe [HsExportSpec] }
>       :  exports                              { Just $1 }
>       |  {- empty -}                          { Nothing }

> exports :: { [HsExportSpec] }
>       : '(' exportlist optcomma ')'           { reverse $2 }
>       | '(' optcomma ')'                      { [] }

> optcomma :: { () }
>       : ','                                   { () }
>       | {- empty -}                           { () }

> exportlist :: { [HsExportSpec] }
>       :  exportlist ',' export                { $3 : $1 }
>       |  export                               { [$1]  }

> export :: { HsExportSpec }
>       :  qvar                                 { HsEVar $1 }
>       |  qtyconorcls                          { HsEAbs $1 }
>       |  qtyconorcls '(' '..' ')'             { HsEThingAll $1 }
>       |  qtyconorcls '(' ')'                  { HsEThingWith $1 [] }
>       |  qtyconorcls '(' cnames ')'           { HsEThingWith $1 (reverse $3) }
>       |  'module' modid                       { HsEModuleContents $2 }

-----------------------------------------------------------------------------
Import Declarations

> impdecls :: { [HsImportDecl] }
>       : impdecls semis impdecl                { $3 : $1 }
>       | impdecl                               { [$1] }

> impdecl :: { HsImportDecl }
>       : srcloc 'import' optqualified modid maybeas maybeimpspec
>                               { HsImportDecl $1 $4 $3 $5 $6 }

> optqualified :: { Bool }
>       : 'qualified'                           { True  }
>       | {- empty -}                           { False }

> maybeas :: { Maybe Module }
>       : 'as' modid                            { Just $2 }
>       | {- empty -}                           { Nothing }


> maybeimpspec :: { Maybe (Bool, [HsImportSpec]) }
>       : impspec                               { Just $1 }
>       | {- empty -}                           { Nothing }

> impspec :: { (Bool, [HsImportSpec]) }
>       : opthiding '(' importlist optcomma ')' { ($1, reverse $3) }
>       | opthiding '(' optcomma ')'            { ($1, []) }

> opthiding :: { Bool }
>       : 'hiding'                              { True }
>       | {- empty -}                           { False }

> importlist :: { [HsImportSpec] }
>       :  importlist ',' importspec            { $3 : $1 }
>       |  importspec                           { [$1]  }

> importspec :: { HsImportSpec }
>       :  var                                  { HsIVar $1 }
>       |  tyconorcls                           { HsIAbs $1 }
>       |  tyconorcls '(' '..' ')'              { HsIThingAll $1 }
>       |  tyconorcls '(' ')'                   { HsIThingWith $1 [] }
>       |  tyconorcls '(' cnames ')'            { HsIThingWith $1 (reverse $3) }

> cnames :: { [HsCName] }
>       :  cnames ',' cname                     { $3 : $1 }
>       |  cname                                { [$1]  }

> cname :: { HsCName }
>       :  var                                  { HsVarName $1 }
>       |  con                                  { HsConName $1 }

-----------------------------------------------------------------------------
Fixity Declarations

> fixdecl :: { HsDecl }
>       : srcloc infix prec ops                 { HsInfixDecl $1 $2 $3 (reverse $4) }

> prec :: { Int }
>       : {- empty -}                           { 9 }
>       | INT                                   {% checkPrec $1 }

> infix :: { HsAssoc }
>       : 'infix'                               { HsAssocNone  }
>       | 'infixl'                              { HsAssocLeft  }
>       | 'infixr'                              { HsAssocRight }

> ops   :: { [HsOp] }
>       : ops ',' op                            { $3 : $1 }
>       | op                                    { [$1] }

-----------------------------------------------------------------------------
Top-Level Declarations

Note: The report allows topdecls to be empty. This would result in another
shift/reduce-conflict, so we don't handle this case here, but in bodyaux.

> topdecls :: { [HsDecl] }
>       : topdecls1 optsemis            {% checkRevDecls $1 }

> topdecls1 :: { [HsDecl] }
>       : topdecls1 semis topdecl       { $3 : $1 }
>       | topdecl                       { [$1] }

> topdecl :: { HsDecl }
>       : srcloc 'type' dtype '=' ctype
>                       {% do { (c,ts) <- checkSimpleType $3;
>                               return (HsTypeDecl $1 c ts $5) } }
>       | srcloc 'type' 'family' type optkind
>                       {% do { (c,ts) <- checkSimpleType $4;
>                               return (HsTypeFamDecl $1 c ts $5) } }
>       | srcloc 'type' 'instance' dtype '=' ctype
>                       {% do { -- no checkSimpleType $4 since dtype may contain type patterns
>                               return (HsTypeInsDecl $1 $4 $6) } }
>       | srcloc data_or_newtype ctype constrs0 deriving
>                       {% do { (cs,c,t) <- checkDataHeader $3;
>                               checkDataOrNew $2 $4;
>                               return (HsDataDecl $1 $2 cs c t (reverse $4) $5) } }
>       | srcloc data_or_newtype ctype optkind 'where' gadtlist
>                       {% do { (cs,c,t) <- checkDataHeader $3;
>                               checkDataOrNew $2 $6;
>                               return (HsGDataDecl $1 $2 cs c t $4 (reverse $6)) } }
>       | srcloc 'data' 'family' ctype optkind
>                       {% do { (cs,c,t) <- checkDataHeader $4;
>                               return (HsDataFamDecl $1 cs c t $5) } }
>       | srcloc data_or_newtype 'instance' ctype constrs0 deriving
>                       {% do { -- (cs,c,t) <- checkDataHeader $4;
>                               checkDataOrNew $2 $5;
>                               return (HsDataInsDecl $1 $2 $4 (reverse $5) $6) } }
>       | srcloc data_or_newtype 'instance' ctype optkind 'where' gadtlist
>                       {% do { -- (cs,c,t) <- checkDataHeader $4;
>                               checkDataOrNew $2 $7;
>                               return (HsGDataInsDecl $1 $2 $4 $5 (reverse $7)) } }
>       | srcloc 'class' ctype fds optcbody
>                       {% do { (cs,c,vs) <- checkClassHeader $3;
>                               return (HsClassDecl $1 cs c vs $4 $5) } }
>       | srcloc 'instance' ctype optvaldefs
>                       {% do { (cs,c,ts) <- checkInstHeader $3;
>                               return (HsInstDecl $1 cs c ts $4) } }
>       | srcloc 'deriving' 'instance' ctype
>                       {% do { (cs, c, ts) <- checkInstHeader $4;
>                               return (HsDerivDecl $1 cs c ts) } }
>       | srcloc 'default' '(' typelist ')'
>                       { HsDefaultDecl $1 $4 }
>       | srcloc '$(' exp ')'
>                        {% do { e <- checkExpr $3;
>                                return $ HsSpliceDecl $1 $ HsParenSplice e } }
>
>       | srcloc 'foreign' 'import' callconv safety fspec
>                       { let (s,n,t) = $6 in HsForImp $1 $4 $5 s n t }
>       | srcloc 'foreign' 'export' callconv fspec
>                       { let (s,n,t) = $5 in HsForExp $1 $4 s n t }
>       | decl          { $1 }

> data_or_newtype :: { DataOrNew }
>       : 'data'    { DataType }
>       | 'newtype' { NewType  }

> typelist :: { [HsType] }
>       : types                         { reverse $1 }
>       | type                          { [$1] }
>       | {- empty -}                   { [] }

> decls :: { [HsDecl] }
>       : optsemis decls1 optsemis      {% checkRevDecls $2 }
>       | optsemis                      { [] }

> decls1 :: { [HsDecl] }
>       : decls1 semis decl             { $3 : $1 }
>       | decl                          { [$1] }

> decl :: { HsDecl }
>       : signdecl                      { $1 }
>       | fixdecl                       { $1 }
>       | valdef                        { $1 }

> decllist :: { [HsDecl] }
>       : '{'  decls '}'                { $2 }
>       | open decls close              { $2 }

> signdecl :: { HsDecl }
>       : srcloc vars '::' ctype        { HsTypeSig $1 (reverse $2) $4 }

Binding can be either of implicit parameters, or it can be a normal sequence
of declarations. The two kinds cannot be mixed within the same block of
binding.

> binds :: { HsBinds }
>       : decllist                      { HsBDecls $1 }
>       | '{' ipbinds '}'               { HsIPBinds $2 }
>       | open ipbinds close            { HsIPBinds $2 }

ATTENTION: Dirty Hackery Ahead! If the second alternative of vars is var
instead of qvar, we get another shift/reduce-conflict. Consider the
following programs:

   { (+) :: ... }          only var
   { (+) x y  = ... }      could (incorrectly) be qvar

We re-use expressions for patterns, so a qvar would be allowed in patterns
instead of a var only (which would be correct). But deciding what the + is,
would require more lookahead. So let's check for ourselves...

> vars  :: { [HsName] }
>       : vars ',' var                  { $3 : $1 }
>       | qvar                          {% do { n <- checkUnQual $1;
>                                               return [n] } }

-----------------------------------------------------------------------------
FFI

> callconv :: { HsCallConv }
>          : 'stdcall'                  { StdCall }
>          | 'ccall'                    { CCall }

> safety :: { HsSafety }
>        : 'safe'                       { PlaySafe False }
>        | 'unsafe'                     { PlayRisky }
>        | 'threadsafe'                 { PlaySafe True }
>        | {- empty -}                  { PlaySafe False }

> fspec :: { (String, HsName, HsType) }
>       : STRING var_no_safety '::' dtype               { ($1, $2, $4) }
>       |        var_no_safety '::' dtype               { ("", $1, $3) }

-----------------------------------------------------------------------------
Types

> dtype :: { HsType }
>       : btype                         { $1 }
>       | btype qtyconop dtype          { HsTyInfix $1 $2 $3 }
>       | btype qtyvarop dtype          { HsTyInfix $1 $2 $3 }
>       | btype '->' dtype              { HsTyFun $1 $3 }
>   | btype '~' btype       { HsTyPred $ HsEqualP $1 $3 }

Implicit parameters can occur in normal types, as well as in contexts.

> type :: { HsType }
>       : ivar '::' dtype               { HsTyPred $ HsIParam $1 $3 }
>       | dtype                         { $1 }

> btype :: { HsType }
>       : btype atype                   { HsTyApp $1 $2 }
>       | atype                         { $1 }

> atype :: { HsType }
>       : gtycon                        { HsTyCon $1 }
>       | tyvar                         { HsTyVar $1 }
>       | '(' types ')'                 { HsTyTuple Boxed (reverse $2) }
>       | '(#' types1 '#)'              { HsTyTuple Unboxed (reverse $2) }
>       | '[' type ']'                  { HsTyApp list_tycon $2 }
>       | '(' ctype ')'                 { $2 }
>       | '(' ctype '::' kind ')'       { HsTyKind $2 $4 }

> gtycon :: { HsQName }
>       : qconid                        { $1 }
>       | '(' ')'                       { unit_tycon_name }
>       | '(' '->' ')'                  { fun_tycon_name }
>       | '[' ']'                       { list_tycon_name }
>       | '(' commas ')'                { tuple_tycon_name $2 }

These are for infix types

> qtyconop :: { HsQName }
>       : qconop                        { $1 }





(Slightly edited) Comment from GHC's hsparser.y:
"context => type" vs  "type" is a problem, because you can't distinguish between

        foo :: (Baz a, Baz a)
        bar :: (Baz a, Baz a) => [a] -> [a] -> [a]

with one token of lookahead.  The HACK is to parse the context as a btype
(more specifically as a tuple type), then check that it has the right form
C a, or (C1 a, C2 b, ... Cn z) and convert it into a context.  Blaach!

> ctype :: { HsType }
>       : 'forall' ktyvars '.' ctype    { mkHsTyForall (Just $2) [] $4 }
>       | context '=>' type             { mkHsTyForall Nothing $1 $3 }
>       | type                          { $1 }

> context :: { HsContext }
>       : btype                         {% checkContext $1 }
>   | btype '~' btype       {% checkContext (HsTyPred $ HsEqualP $1 $3) }

> types :: { [HsType] }
>       : types1 ',' type               { $3 : $1 }

> types1 :: { [HsType] }
>       : type                          { [$1] }
>       | types1 ',' type               { $3 : $1 }

 simpletype :: { (HsName, [HsName]) }
        : tycon tyvars                  { ($1,reverse $2) }

> ktyvars :: { [HsTyVarBind] }
>       : ktyvars ktyvar                { $2 : $1 }
>       | {- empty -}                   { [] }

> ktyvar :: { HsTyVarBind }
>       : tyvar                         { HsUnkindedVar $1 }
>       | '(' tyvar '::' kind ')'       { HsKindedVar $2 $4 }

> tyvars :: { [HsName] }
>       : tyvars tyvar                  { $2 : $1 }
>       | {- empty -}                   { [] }


-----------------------------------------------------------------------------
Functional Dependencies

> fds :: { [HsFunDep] }
>       : {- empty -}                   { [] }
>       | '|' fds1                      { reverse $2 }

> fds1 :: { [HsFunDep] }
>       : fds1 ',' fd                   { $3 : $1 }
>       | fd                            { [$1] }

> fd :: { HsFunDep }
>       : tyvars '->' tyvars            { HsFunDep (reverse $1) (reverse $3) }

-----------------------------------------------------------------------------
Datatype declarations

GADTs

> gadtlist :: { [HsGadtDecl] }
>       : '{' gadtconstrs1 '}'                  { $2 }
>       | open gadtconstrs1 close               { $2 }

> gadtconstrs1 :: { [HsGadtDecl] }
>       : optsemis gadtconstrs optsemis         { $2 }

> gadtconstrs :: { [HsGadtDecl] }
>       : gadtconstrs semis gadtconstr          { $3 : $1 }
>       | gadtconstr                            { [$1] }

> gadtconstr :: { HsGadtDecl }
>       : srcloc qcon '::' ctype                {% do { c <- checkUnQual $2;
>                                                       return $ HsGadtDecl $1 c $4 } }

> constrs0 :: { [HsQualConDecl] }
>       : {- empty -}                   { [] }
>       | '=' constrs                   { $2 }

> constrs :: { [HsQualConDecl] }
>       : constrs '|' constr            { $3 : $1 }
>       | constr                        { [$1] }

> constr :: { HsQualConDecl }
>       : srcloc forall context '=>' constr1    { HsQualConDecl $1 $2 $3 $5 }
>       | srcloc forall constr1                 { HsQualConDecl $1 $2 [] $3 }

> forall :: { [HsTyVarBind] }
>       : 'forall' ktyvars '.'          { $2 }
>       | {- empty -}                   { [] }

> constr1 :: { HsConDecl }
>       : scontype                      { HsConDecl (fst $1) (snd $1) }
>       | sbtype conop sbtype           { HsConDecl $2 [$1,$3] }
>       | con '{' '}'                   { HsRecDecl $1 [] }
>       | con '{' fielddecls '}'        { HsRecDecl $1 (reverse $3) }

> scontype :: { (HsName, [HsBangType]) }
>       : btype                         {% do { (c,ts) <- splitTyConApp $1;
>                                               return (c,map HsUnBangedTy ts) } }
>       | scontype1                     { $1 }

> scontype1 :: { (HsName, [HsBangType]) }
>       : btype '!' atype               {% do { (c,ts) <- splitTyConApp $1;
>                                               return (c,map HsUnBangedTy ts++
>                                                       [HsBangedTy $3]) } }
>       | scontype1 satype              { (fst $1, snd $1 ++ [$2] ) }

> satype :: { HsBangType }
>       : atype                         { HsUnBangedTy $1 }
>       | '!' atype                     { HsBangedTy   $2 }

> sbtype :: { HsBangType }
>       : btype                         { HsUnBangedTy $1 }
>       | '!' atype                     { HsBangedTy   $2 }

> fielddecls :: { [([HsName],HsBangType)] }
>       : fielddecls ',' fielddecl      { $3 : $1 }
>       | fielddecl                     { [$1] }

> fielddecl :: { ([HsName],HsBangType) }
>       : vars '::' stype               { (reverse $1, $3) }

> stype :: { HsBangType }
>       : ctype                         { HsUnBangedTy $1 }     
>       | '!' atype                     { HsBangedTy   $2 }

> deriving :: { [HsQName] }
>       : {- empty -}                   { [] }
>       | 'deriving' qtycls             { [$2] }
>       | 'deriving' '('          ')'   { [] }
>       | 'deriving' '(' dclasses ')'   { reverse $3 }

> dclasses :: { [HsQName] }
>       : dclasses ',' qtycls           { $3 : $1 }
>       | qtycls                        { [$1] }

-----------------------------------------------------------------------------
Kinds

> kind :: { HsKind }
>       : akind                 { $1 }
>       | akind '->' kind       { HsKindFn $1 $3 }

> akind :: { HsKind }
>       : '*'                   { HsKindStar  }
>       | '!'                   { HsKindBang  }
>       | '(' kind ')'          { $2 }

> optkind :: { Maybe HsKind }
>       : {-empty-}             { Nothing }
>       | '::' kind             { Just $2 }
-----------------------------------------------------------------------------
Class declarations

No implicit parameters in the where clause of a class declaration.
> optcbody :: { [HsClassDecl] }
>       : 'where' cldecllist            {% checkClassBody $2 }
>       | {- empty -}                   { [] }

> cldecllist :: { [HsClassDecl] }
>       : '{'  cldecls '}'              { $2 }
>       | open cldecls close            { $2 }

> cldecls :: { [HsClassDecl] }
>       : optsemis cldecls1 optsemis    {% checkRevClsDecls $2 }
>       | optsemis                      { [] }

> cldecls1 :: { [HsClassDecl] }
>       : cldecls1 semis cldecl         { $3 : $1 }
>       | cldecl                        { [$1] }

> cldecl :: { HsClassDecl }
>       : decl                          { HsClsDecl $1 }
>       | atdecl                        { $1 }

> atdecl :: { HsClassDecl }
>       : srcloc 'type' type optkind           
>               {% do { (c,ts) <- checkSimpleType $3;
>                       return (HsClsTyFam $1 c ts $4) } }
>       | srcloc 'type' dtype '=' ctype
>                       { HsClsTyDef $1 $3 $5 }
>       | srcloc 'data' ctype optkind
>                {% do { (cs,c,t) <- checkDataHeader $3;
>                        return (HsClsDataFam $1 cs c t $4) } }

-----------------------------------------------------------------------------
Instance declarations

> optvaldefs :: { [HsInstDecl] }
>       : 'where' '{'  valdefs '}'      {% checkInstBody $3 }
>       | 'where' open valdefs close    {% checkInstBody $3 }
>       | {- empty -}                   { [] }

> valdefs :: { [HsInstDecl] }
>       : optsemis valdefs1 optsemis    {% checkRevInstDecls $2 }
>       | optsemis                      { [] }

> valdefs1 :: { [HsInstDecl] }
>       : valdefs1 semis insvaldef      { $3 : $1 }
>       | insvaldef                     { [$1] }

> insvaldef :: { HsInstDecl }
>       : valdef                        { HsInsDecl $1 }
>       | atinst                        { $1 }

> atinst :: { HsInstDecl }
>       : srcloc 'type' dtype '=' ctype
>                       {% do { -- no checkSimpleType $4 since dtype may contain type patterns
>                               return (HsInsType $1 $3 $5) } }
>       | srcloc data_or_newtype ctype constrs0 deriving
>                       {% do { -- (cs,c,t) <- checkDataHeader $4;
>                               checkDataOrNew $2 $4;
>                               return (HsInsData $1 $2 $3 (reverse $4) $5) } }
>       | srcloc data_or_newtype ctype optkind 'where' gadtlist
>                       {% do { -- (cs,c,t) <- checkDataHeader $4;
>                               checkDataOrNew $2 $6;
>                               return (HsInsGData $1 $2 $3 $4 (reverse $6)) } }

-----------------------------------------------------------------------------
Value definitions

> valdef :: { HsDecl }
>       : srcloc exp0b rhs optwhere     {% checkValDef $1 $2 $3 $4 }

May bind implicit parameters
> optwhere :: { HsBinds }
>       : 'where' binds                 { $2 }
>       | {- empty -}                   { HsBDecls [] }

> rhs   :: { HsRhs }
>       : '=' exp                       {% do { e <- checkExpr $2;
>                                               return (HsUnGuardedRhs e) } }
>       | gdrhs                         { HsGuardedRhss  (reverse $1) }

> gdrhs :: { [HsGuardedRhs] }
>       : gdrhs gdrh                    { $2 : $1 }
>       | gdrh                          { [$1] }

Guards may contain patterns, hence quals instead of exp.
> gdrh :: { HsGuardedRhs }
>       : srcloc '|' quals '=' exp      {% do { e <- checkExpr $5;
>                                               return (HsGuardedRhs $1 (reverse $3) e) } }

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

> exp   :: { HsExp }
>       : exp0b '::' srcloc ctype       { HsExpTypeSig $3 $1 $4 }
>       | exp0b 'with' ipbinding        { HsWith $1 $3 }  -- implicit parameters
>       | exp0                          { $1 }
>       | exp0b qop                     { HsPostOp $1 $2 } -- for HaRP's sake

> exp0 :: { HsExp }
>       : exp0a                         { $1 }
>       | exp0b                         { $1 }

> exp0a :: { HsExp }
>       : exp0b qop exp10a              { HsInfixApp $1 $2 $3 }
>       | exp10a                        { $1 }

> exp0b :: { HsExp }
>       : exp0b qop exp10b              { HsInfixApp $1 $2 $3 }
>       | dvarexp                       { $1 }
>       | exp10b                        { $1 }

> exp10a :: { HsExp }
>       : '\\' srcloc apats '->' exp    { HsLambda $2 (reverse $3) $5 }
A let may bind implicit parameters
>       | 'let' binds 'in' exp          { HsLet $2 $4 }
>       | 'dlet' ipbinding 'in' exp     { HsDLet $2 $4 }
>       | 'if' exp 'then' exp 'else' exp { HsIf $2 $4 $6 }

> exp10b :: { HsExp }
>       : 'case' exp 'of' altslist      { HsCase $2 $4 }
>       | '-' fexp                      { HsNegApp $2 }
>       | 'do' stmtlist                 { HsDo $2 }
>       | 'mdo' stmtlist                { HsMDo $2 }
       | reifyexp                      { HsReifyExp $1 }
>       | fexp                          { $1 }

> fexp :: { HsExp }
>       : fexp aexp                     { HsApp $1 $2 }
>       | aexp                          { $1 }

> apats :: { [HsPat] }
>       : apats apat                    { $2 : $1 }
>       | apat                          { [$1] }

> apat :: { HsPat }
>       : aexp                          {% checkPattern $1 }

UGLY: Because patterns and expressions are mixed, aexp has to be split into
two rules: One right-recursive and one left-recursive. Otherwise we get two
reduce/reduce-errors (for as-patterns and irrefutable patters).

Even though the variable in an as-pattern cannot be qualified, we use
qvar here to avoid a shift/reduce conflict, and then check it ourselves
(as for vars above).

> aexp  :: { HsExp }
>       : qvar '@' aexp                 {% do { n <- checkUnQual $1;
>                                               return (HsAsPat n $3) } }
>       | qvar '@:' aexp                {% do { n <- checkUnQual $1;
>                                               return (HsCAsRP n $3) } }
>       | '~' aexp                      { HsIrrPat $2 }
>       | aexp1                         { $1 }

Note: The first two alternatives of aexp1 are not necessarily record
updates: they could be labeled constructions.

> aexp1 :: { HsExp }
>       : aexp1 '{' '}'                 {% mkRecConstrOrUpdate $1 [] }
>       | aexp1 '{' fbinds '}'          {% mkRecConstrOrUpdate $1 (reverse $3) }
>       | aexp2                         { $1 }

According to the Report, the left section (e op) is legal iff (e op x)
parses equivalently to ((e) op x).  Thus e must be an exp0b.
An implicit parameter can be used as an expression.

> aexp2 :: { HsExp }
>       : ivar                          { HsIPVar $1 }
>       | qvar                          { HsVar $1 }
>       | gcon                          { $1 }
>       | literal                       { HsLit $1 }
>       | '(' exp ')'                   { HsParen $2 }
>       | '(' texps ')'                 { HsTuple (reverse $2) }
>       | '[' list ']'                  { $2 }
        | '(' exp0b rqop ')'            { HsLeftSection $2 $3  }
>       | '(' qopm exp0 ')'             { HsRightSection $2 $3 }
>       | '_'                           { HsWildCard }
>       | '(' erpats ')'                { $2 }
>       | '(|' sexps '|)'               { HsSeqRP $ reverse $2 }
>       | '(|' exp '|' quals '|)'       { HsGuardRP $2 $ reverse $4 }
>       | xml                           { $1 }

Template Haskell
>       | IDSPLICE                      { HsSpliceExp $ HsIdSplice $1 }
>       | '$(' exp ')'                  {% do { e <- checkExpr $2;
>                                               return $ HsSpliceExp $ HsParenSplice e } }
>       | '[|' exp '|]'                 {% do { e <- checkExpr $2;
>                                               return $ HsBracketExp $ HsExpBracket e } }
>       | '[p|' exp0 '|]'               {% do { p <- checkPattern $2;
>                                               return $ HsBracketExp $ HsPatBracket p } }
>       | '[t|' ctype '|]'              { HsBracketExp $ HsTypeBracket $2 }
>       | '[d|' open topdecls close '|]'        { HsBracketExp $ HsDeclBracket $3 }
>       | VARQUOTE qvar                 { HsVarQuote $2 }
>       | VARQUOTE qcon                 { HsVarQuote $2 }
>       | TYPQUOTE tyvar                { HsTypQuote (UnQual $2) }
>       | TYPQUOTE gtycon               { HsTypQuote $2 }


 reifyexp :: { HsReify }
       : 'reifyDecl' gtycon            { HsReifyDecl $2 }
       | 'reifyDecl' qvar              { HsReifyDecl $2 }
       | 'reifyType' qcname            { HsReifyType $2 }
       | 'reifyFixity' qcname          { HsReifyFixity $2 }


 qcname :: { HsQName }
       : qvar                          { $1 }
       | gcon                          {% getGConName $1 }
End Template Haskell

> commas :: { Int }
>       : commas ','                    { $1 + 1 }
>       | ','                           { 1 }

> texps :: { [HsExp] }
>       : texps ',' exp                 { $3 : $1 }
>       | exp ',' exp                   { [$3,$1] }

-----------------------------------------------------------------------------
Harp Extensions

> sexps :: { [HsExp] }
>       : sexps ',' exp                 { $3 : $1 }
>       | exp                           { [$1] }

Either patterns are left associative
> erpats :: { HsExp }
>       : exp '|' erpats              { HsEitherRP $1 $3 }
>       | exp '|' exp                 { HsEitherRP $1 $3 }

-----------------------------------------------------------------------------
Hsx Extensions

> xml :: { HsExp }
>       : srcloc '<' name attrs mattr '>' children '</' name '>'        {% do { n <- checkEqNames $3 $9;
>                                                                               let { cn = reverse $7;
>                                                                                     as = reverse $4; };
>                                                                               return $ HsXTag $1 n as $5 cn } }
>       | srcloc '<' name attrs mattr '/>'                              { HsXETag $1 $3 (reverse $4) $5 }
>       | '<%' exp '%>'                                                 { HsXExpTag $2 }

> children :: { [HsExp] }
>       : children child                { $2 : $1 }
>       | {- empty -}                   { [] }

> child :: { HsExp }
>       : PCDATA                        { HsXPcdata $1 }
>       | '<[' sexps ']>'               { HsXRPats $ reverse $2 }
>       | xml                           { $1 }

> name :: { HsXName }
>       : xmlname ':' xmlname           { HsXDomName $1 $3 }
>       | xmlname                       { HsXName $1 }

> xmlname :: { String }
>       : VARID                         { $1 }
>       | CONID                         { $1 }
>       | DVARID                        { mkDVar $1 }
>       | 'type'                        { "type" }
>       | 'class'                       { "class" }

> attrs :: { [HsXAttr] }
>       : attrs attr                    { $2 : $1 }
>       | {- empty -}                   { [] }

> attr :: { HsXAttr }
>       : name '=' aexp                 { HsXAttr $1 $3 }

> mattr :: { Maybe HsExp }
>       : aexp                          { Just $1 }
>       | {-empty-}                     { Nothing }

Turning dash variables into infix expressions with '-'
> dvarexp :: { HsExp }
>         : DVARID                      { mkDVarExpr $1 }

-----------------------------------------------------------------------------
List expressions

The rules below are little bit contorted to keep lexps left-recursive while
avoiding another shift/reduce-conflict.

> list :: { HsExp }
>       : exp                           { HsList [$1] }
>       | lexps                         { HsList (reverse $1) }
>       | exp '..'                      { HsEnumFrom $1 }
>       | exp ',' exp '..'              { HsEnumFromThen $1 $3 }
>       | exp '..' exp                  { HsEnumFromTo $1 $3 }
>       | exp ',' exp '..' exp          { HsEnumFromThenTo $1 $3 $5 }
>       | exp '|' quals                 { HsListComp $1 (reverse $3) }

> lexps :: { [HsExp] }
>       : lexps ',' exp                 { $3 : $1 }
>       | exp ',' exp                   { [$3,$1] }

-----------------------------------------------------------------------------
List comprehensions

> quals :: { [HsStmt] }
>       : quals1                        {% mapM checkStmt $1 }

> quals1 :: { [HsStmt] }
>       : quals1 ',' qual               { $3 : $1 }
>       | qual                          { [$1] }

> qual  :: { HsStmt }
>       : pat srcloc '<-' exp           { HsGenerator $2 $1 $4 }
>       | exp                           { HsQualifier $1 }
>       | 'let' binds                   { HsLetStmt $2 }

-----------------------------------------------------------------------------
Case alternatives

> altslist :: { [HsAlt] }
>       : '{'  alts '}'                 { $2 }
>       | open alts close               { $2 }

> alts :: { [HsAlt] }
>       : optsemis alts1 optsemis       { reverse $2 }

> alts1 :: { [HsAlt] }
>       : alts1 semis alt               { $3 : $1 }
>       | alt                           { [$1] }

> alt :: { HsAlt }
>       : srcloc pat ralt optwhere      { HsAlt $1 $2 $3 $4 }

> ralt :: { HsGuardedAlts }
>       : '->' exp                      { HsUnGuardedAlt $2 }
>       | gdpats                        { HsGuardedAlts (reverse $1) }

> gdpats :: { [HsGuardedAlt] }
>       : gdpats gdpat                  { $2 : $1 }
>       | gdpat                         { [$1] }

A guard can be a pattern guard, hence quals instead of exp0.
> gdpat :: { HsGuardedAlt }
>       : srcloc '|' quals '->' exp     { HsGuardedAlt $1 (reverse $3) $5 }

> pat :: { HsPat }
>       : exp0b                         {% checkPattern $1 }

-----------------------------------------------------------------------------
Statement sequences

As per the Report, but with stmt expanded to simplify building the list
without introducing conflicts.  This also ensures that the last stmt is
an expression.

> stmtlist :: { [HsStmt] }
>       : '{'  stmts '}'                { $2 }
>       | open stmts close              { $2 }

A let statement may bind implicit parameters.
> stmts :: { [HsStmt] }
>       : 'let' binds ';' stmts         { HsLetStmt $2 : $4 }
>       | pat srcloc '<-' exp ';' stmts { HsGenerator $2 $1 $4 : $6 }
>       | exp ';' stmts                 { HsQualifier $1 : $3 }
>       | ';' stmts                     { $2 }
>       | exp ';'                       { [HsQualifier $1] }
>       | exp                           { [HsQualifier $1] }

-----------------------------------------------------------------------------
Record Field Update/Construction

> fbinds :: { [HsFieldUpdate] }
>       : fbinds ',' fbind              { $3 : $1 }
>       | fbind                         { [$1] }

> fbind :: { HsFieldUpdate }
>       : qvar '=' exp                  { HsFieldUpdate $1 $3 }

-----------------------------------------------------------------------------
Implicit parameter bindings

> ipbinding :: { [HsIPBind] }
>       : '{' ipbinds '}'               { $2 }
>       | open ipbinds close            { $2 }

> ipbinds :: { [HsIPBind] }
>       : optsemis ipbinds1 optsemis    { reverse $2 }

> ipbinds1 :: { [HsIPBind] }
>       : ipbinds1 semis ipbind         { $3 : $1 }
>       | ipbind                        { [$1] }

> ipbind :: { HsIPBind }
>       : srcloc ivar '=' exp           { HsIPBind $1 $2 $4 }

-----------------------------------------------------------------------------
Variables, Constructors and Operators.

> gcon :: { HsExp }
>       : '(' ')'               { unit_con }
>       | '[' ']'               { HsList [] }
>       | '(' commas ')'        { tuple_con $2 }
>       | qcon                  { HsCon $1 }

> var   :: { HsName }
>       : varid                 { $1 }
>       | '(' varsym ')'        { $2 }

> var_no_safety :: { HsName }
>               : varid_no_safety       { $1 }
>               | '(' varsym ')'        { $2 }

> qvar  :: { HsQName }
>       : qvarid                { $1 }
>       | '(' qvarsym ')'       { $2 }

Implicit parameter
> ivar  :: { HsIPName }
>       : ivarid                { $1 }

> con   :: { HsName }
>       : conid                 { $1 }
>       | '(' consym ')'        { $2 }

> qcon  :: { HsQName }
>       : qconid                { $1 }
>       | '(' gconsym ')'       { $2 }

> varop :: { HsName }
>       : varsym                { $1 }
>       | '`' varid '`'         { $2 }

> qvarop :: { HsQName }
>       : qvarsym               { $1 }
>       | '`' qvarid '`'        { $2 }

> qvaropm :: { HsQName }
>       : qvarsymm              { $1 }
>       | '`' qvarid '`'        { $2 }

> conop :: { HsName }
>       : consym                { $1 }  
>       | '`' conid '`'         { $2 }

> qconop :: { HsQName }
>       : gconsym               { $1 }
>       | '`' qconid '`'        { $2 }

> op    :: { HsOp }
>       : varop                 { HsVarOp $1 }
>       | conop                 { HsConOp $1 }

> qop   :: { HsQOp }
>       : qvarop                { HsQVarOp $1 }
>       | qconop                { HsQConOp $1 }

> qopm  :: { HsQOp }
>       : qvaropm               { HsQVarOp $1 }
>       | qconop                { HsQConOp $1 }

> gconsym :: { HsQName }
>       : ':'                   { list_cons_name }
>       | qconsym               { $1 }

-----------------------------------------------------------------------------
Identifiers and Symbols

> qvarid :: { HsQName }
>       : varid                 { UnQual $1 }
>       | QVARID                { Qual (Module (fst $1)) (HsIdent (snd $1)) }

> varid_no_safety :: { HsName }
>       : VARID                 { HsIdent $1 }
>       | 'as'                  { as_name }
>       | 'qualified'           { qualified_name }
>       | 'hiding'              { hiding_name }
>       | 'export'              { export_name }
>       | 'stdcall'             { stdcall_name }
>       | 'ccall'               { ccall_name }

> varid :: { HsName }
>       : varid_no_safety       { $1 }
>       | 'safe'                { safe_name }
>       | 'unsafe'              { unsafe_name }
>       | 'threadsafe'          { threadsafe_name }


Implicit parameter
> ivarid :: { HsIPName }
>       : IDUPID                { HsIPDup $1 }
>       | ILINID                { HsIPLin $1 }

> qconid :: { HsQName }
>       : conid                 { UnQual $1 }
>       | QCONID                { Qual (Module (fst $1)) (HsIdent (snd $1)) }

> conid :: { HsName }
>       : CONID                 { HsIdent $1 }

> qconsym :: { HsQName }
>       : consym                { UnQual $1 }
>       | QCONSYM               { Qual (Module (fst $1)) (HsSymbol (snd $1)) }

> consym :: { HsName }
>       : CONSYM                { HsSymbol $1 }

> qvarsym :: { HsQName }
>       : varsym                { UnQual $1 }
>       | qvarsym1              { $1 }

> qvarsymm :: { HsQName }
>       : varsymm               { UnQual $1 }
>       | qvarsym1              { $1 }

> varsym :: { HsName }
>       : VARSYM                { HsSymbol $1 }
>       | '-'                   { minus_name }
>       | '!'                   { pling_name }
>       | '.'                   { dot_name }
>       | '*'                   { star_name }

> varsymm :: { HsName } -- varsym not including '-'
>       : VARSYM                { HsSymbol $1 }
>       | '!'                   { pling_name }
>       | '.'                   { dot_name }
>       | '*'                   { star_name }

> qvarsym1 :: { HsQName }
>       : QVARSYM               { Qual (Module (fst $1)) (HsSymbol (snd $1)) }

> literal :: { HsLiteral }
>       : INT                   { HsInt $1 }
>       | CHAR                  { HsChar $1 }
>       | RATIONAL              { HsFrac $1 }
>       | STRING                { HsString $1 }

> srcloc :: { SrcLoc }  :       {% getSrcLoc }
 
-----------------------------------------------------------------------------
Layout

> open  :: { () }       :       {% pushCurrentContext }

> close :: { () }
>       : vccurly               { () } -- context popped in lexer.
>       | error                 {% popContext }

-----------------------------------------------------------------------------
Miscellaneous (mostly renamings)

> modid :: { Module }
>       : CONID                 { Module $1 }
>       | QCONID                { Module (fst $1 ++ '.':snd $1) }

> tyconorcls :: { HsName }
>       : conid                 { $1 }

 tycon :: { HsName }
        : conid                 { $1 }

> qtyconorcls :: { HsQName }
>       : qconid                { $1 }

> qtycls :: { HsQName }
>       : qconid                { $1 }

> tyvar :: { HsName }
>       : varid                 { $1 }

> qtyvarop :: { HsQName }
> qtyvarop : '`' tyvar '`'       { UnQual $2 }
>          | tyvarsym            { UnQual $1 }

> tyvarsym :: { HsName }
> tyvarsym : VARSYM              { HsSymbol $1 }

-----------------------------------------------------------------------------

> {
> happyError :: P a
> happyError = fail "Parse error"

> -- | Parse of a string, which should contain a complete Haskell 98 module.
> parseModule :: String -> ParseResult HsModule
> parseModule = runParser parse

> -- | Parse of a string, which should contain a complete Haskell 98 module.
> parseModuleWithMode :: ParseMode -> String -> ParseResult HsModule
> parseModuleWithMode mode = runParserWithMode mode parse
> }

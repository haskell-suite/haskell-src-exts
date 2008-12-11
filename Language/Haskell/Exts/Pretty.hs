{-# OPTIONS_GHC -w #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Pretty
-- Copyright   :  (c) Niklas Broberg 2004,
--                (c) The GHC Team, Noel Winstanley 1997-2000
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Pretty printer for Haskell with extensions.
--
-----------------------------------------------------------------------------

module Language.Haskell.Exts.Pretty (
                -- * Pretty printing
                Pretty,
                prettyPrintStyleMode, prettyPrintWithMode, prettyPrint,
                -- * Pretty-printing styles (from "Text.PrettyPrint.HughesPJ")
                P.Style(..), P.style, P.Mode(..),
                -- * Haskell formatting modes
                PPHsMode(..), Indent, PPLayout(..), defaultMode) where

import Language.Haskell.Exts.Syntax

import qualified Text.PrettyPrint as P

infixl 5 $$$

-----------------------------------------------------------------------------

-- | Varieties of layout we can use.
data PPLayout = PPOffsideRule   -- ^ classical layout
              | PPSemiColon     -- ^ classical layout made explicit
              | PPInLine        -- ^ inline decls, with newlines between them
              | PPNoLayout      -- ^ everything on a single line
              deriving Eq

type Indent = Int

-- | Pretty-printing parameters.
--
-- /Note:/ the 'onsideIndent' must be positive and less than all other indents.
data PPHsMode = PPHsMode {
                                -- | indentation of a class or instance
                classIndent :: Indent,
                                -- | indentation of a @do@-expression
                doIndent :: Indent,
                                -- | indentation of the body of a
                                -- @case@ expression
                caseIndent :: Indent,
                                -- | indentation of the declarations in a
                                -- @let@ expression
                letIndent :: Indent,
                                -- | indentation of the declarations in a
                                -- @where@ clause
                whereIndent :: Indent,
                                -- | indentation added for continuation
                                -- lines that would otherwise be offside
                onsideIndent :: Indent,
                                -- | blank lines between statements?
                spacing :: Bool,
                                -- | Pretty-printing style to use
                layout :: PPLayout,
                                -- | add GHC-style @LINE@ pragmas to output?
                linePragmas :: Bool,
                                -- | not implemented yet
                comments :: Bool
                }

-- | The default mode: pretty-print using the offside rule and sensible
-- defaults.
defaultMode :: PPHsMode
defaultMode = PPHsMode{
                      classIndent = 8,
                      doIndent = 3,
                      caseIndent = 4,
                      letIndent = 4,
                      whereIndent = 6,
                      onsideIndent = 2,
                      spacing = True,
                      layout = PPOffsideRule,
                      linePragmas = False,
                      comments = True
                      }

-- | Pretty printing monad
newtype DocM s a = DocM (s -> a)

instance Functor (DocM s) where
         fmap f xs = do x <- xs; return (f x)

instance Monad (DocM s) where
        (>>=) = thenDocM
        (>>) = then_DocM
        return = retDocM

{-# INLINE thenDocM #-}
{-# INLINE then_DocM #-}
{-# INLINE retDocM #-}
{-# INLINE unDocM #-}
{-# INLINE getPPEnv #-}

thenDocM :: DocM s a -> (a -> DocM s b) -> DocM s b
thenDocM m k = DocM $ (\s -> case unDocM m $ s of a -> unDocM (k a) $ s)

then_DocM :: DocM s a -> DocM s b -> DocM s b
then_DocM m k = DocM $ (\s -> case unDocM m $ s of _ -> unDocM k $ s)

retDocM :: a -> DocM s a
retDocM a = DocM (\_s -> a)

unDocM :: DocM s a -> (s -> a)
unDocM (DocM f) = f

-- all this extra stuff, just for this one function.
getPPEnv :: DocM s s
getPPEnv = DocM id

-- So that pp code still looks the same
-- this means we lose some generality though

-- | The document type produced by these pretty printers uses a 'PPHsMode'
-- environment.
type Doc = DocM PPHsMode P.Doc

-- | Things that can be pretty-printed, including all the syntactic objects
-- in "Language.Haskell.Syntax".
class Pretty a where
        -- | Pretty-print something in isolation.
        pretty :: a -> Doc
        -- | Pretty-print something in a precedence context.
        prettyPrec :: Int -> a -> Doc
        pretty = prettyPrec 0
        prettyPrec _ = pretty

-- The pretty printing combinators

empty :: Doc
empty = return P.empty

nest :: Int -> Doc -> Doc
nest i m = m >>= return . P.nest i


-- Literals

text, ptext :: String -> Doc
text = return . P.text
ptext = return . P.text

char :: Char -> Doc
char = return . P.char

int :: Int -> Doc
int = return . P.int

integer :: Integer -> Doc
integer = return . P.integer

float :: Float -> Doc
float = return . P.float

double :: Double -> Doc
double = return . P.double

rational :: Rational -> Doc
rational = return . P.rational

-- Simple Combining Forms

parens, brackets, braces,quotes,doubleQuotes :: Doc -> Doc
parens d = d >>= return . P.parens
brackets d = d >>= return . P.brackets
braces d = d >>= return . P.braces
quotes d = d >>= return . P.quotes
doubleQuotes d = d >>= return . P.doubleQuotes

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

-- Constants

semi,comma,colon,space,equals :: Doc
semi = return P.semi
comma = return P.comma
colon = return P.colon
space = return P.space
equals = return P.equals

lparen,rparen,lbrack,rbrack,lbrace,rbrace :: Doc
lparen = return  P.lparen
rparen = return  P.rparen
lbrack = return  P.lbrack
rbrack = return  P.rbrack
lbrace = return  P.lbrace
rbrace = return  P.rbrace

-- Combinators

(<>),(<+>),($$),($+$) :: Doc -> Doc -> Doc
aM <> bM = do{a<-aM;b<-bM;return (a P.<> b)}
aM <+> bM = do{a<-aM;b<-bM;return (a P.<+> b)}
aM $$ bM = do{a<-aM;b<-bM;return (a P.$$ b)}
aM $+$ bM = do{a<-aM;b<-bM;return (a P.$+$ b)}

hcat,hsep,vcat,sep,cat,fsep,fcat :: [Doc] -> Doc
hcat dl = sequence dl >>= return . P.hcat
hsep dl = sequence dl >>= return . P.hsep
vcat dl = sequence dl >>= return . P.vcat
sep dl = sequence dl >>= return . P.sep
cat dl = sequence dl >>= return . P.cat
fsep dl = sequence dl >>= return . P.fsep
fcat dl = sequence dl >>= return . P.fcat

-- Some More

hang :: Doc -> Int -> Doc -> Doc
hang dM i rM = do{d<-dM;r<-rM;return $ P.hang d i r}

-- Yuk, had to cut-n-paste this one from Pretty.hs
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate p (d1:ds) = go d1 ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d <> p) : go e es

-- | render the document with a given style and mode.
renderStyleMode :: P.Style -> PPHsMode -> Doc -> String
renderStyleMode ppStyle ppMode d = P.renderStyle ppStyle . unDocM d $ ppMode

-- | render the document with a given mode.
renderWithMode :: PPHsMode -> Doc -> String
renderWithMode = renderStyleMode P.style

-- | render the document with 'defaultMode'.
render :: Doc -> String
render = renderWithMode defaultMode

-- | pretty-print with a given style and mode.
prettyPrintStyleMode :: Pretty a => P.Style -> PPHsMode -> a -> String
prettyPrintStyleMode ppStyle ppMode = renderStyleMode ppStyle ppMode . pretty

-- | pretty-print with the default style and a given mode.
prettyPrintWithMode :: Pretty a => PPHsMode -> a -> String
prettyPrintWithMode = prettyPrintStyleMode P.style

-- | pretty-print with the default style and 'defaultMode'.
prettyPrint :: Pretty a => a -> String
prettyPrint = prettyPrintWithMode defaultMode

fullRenderWithMode :: PPHsMode -> P.Mode -> Int -> Float ->
                      (P.TextDetails -> a -> a) -> a -> Doc -> a
fullRenderWithMode ppMode m i f fn e mD =
                   P.fullRender m i f fn e $ (unDocM mD) ppMode


fullRender :: P.Mode -> Int -> Float -> (P.TextDetails -> a -> a)
              -> a -> Doc -> a
fullRender = fullRenderWithMode defaultMode

-------------------------  Pretty-Print a Module --------------------
instance Pretty Module where
        pretty (Module pos m os mbWarn mbExports imp decls) =
                markLine pos $
                myVcat $ map pretty os ++
                            [topLevel (ppModuleHeader m mbWarn mbExports)
                                     (map pretty imp ++ map pretty decls)]

--------------------------  Module Header ------------------------------
ppModuleHeader :: ModuleName -> Maybe WarningText -> Maybe [ExportSpec] -> Doc
ppModuleHeader m mbWarn mbExportList = mySep [
        text "module",
        pretty m,
        maybePP ppWarnTxt mbWarn,
        maybePP (parenList . map pretty) mbExportList,
        text "where"]

ppWarnTxt :: WarningText -> Doc
ppWarnTxt (DeprText s) = mySep [text "{-# DEPRECATED", text s, text "#-}"]
ppWarnTxt (WarnText s) = mySep [text "{-# WARNING",    text s, text "#-}"]

instance Pretty ModuleName where
        pretty (ModuleName modName) = text modName

instance Pretty ExportSpec where
        pretty (EVar name)                = pretty name
        pretty (EAbs name)                = pretty name
        pretty (EThingAll name)           = pretty name <> text "(..)"
        pretty (EThingWith name nameList) =
                pretty name <> (parenList . map pretty $ nameList)
        pretty (EModuleContents m)        = text "module" <+> pretty m

instance Pretty ImportDecl where
        pretty (ImportDecl pos m qual src mbName mbSpecs) =
                markLine pos $
                mySep [text "import",
                       if qual then text "qualified" else empty,
                       if src  then text "{-# SOURCE #-}" else empty,
                       pretty m,
                       maybePP (\m' -> text "as" <+> pretty m') mbName,
                       maybePP exports mbSpecs]
            where
                exports (b,specList) =
                        if b then text "hiding" <+> specs else specs
                    where specs = parenList . map pretty $ specList

instance Pretty ImportSpec where
        pretty (IVar name)                = pretty name
        pretty (IAbs name)                = pretty name
        pretty (IThingAll name)           = pretty name <> text "(..)"
        pretty (IThingWith name nameList) =
                pretty name <> (parenList . map pretty $ nameList)

-------------------------  Declarations ------------------------------
instance Pretty Decl where
        pretty (TypeDecl loc name nameList htype) =
                blankline $
                markLine loc $
                mySep ( [text "type", pretty name]
                        ++ map pretty nameList
                        ++ [equals, pretty htype])

        pretty (DataDecl loc don context name nameList constrList derives) =
                blankline $
                markLine loc $
                mySep ( [pretty don, ppContext context, pretty name]
                        ++ map pretty nameList)
                        <+> (myVcat (zipWith (<+>) (equals : repeat (char '|'))
                                                   (map pretty constrList))
                        $$$ ppDeriving derives)

        pretty (GDataDecl loc don context name nameList optkind gadtList derives) =
                blankline $
                markLine loc $
                mySep ( [pretty don, ppContext context, pretty name]
                        ++ map pretty nameList ++ ppOptKind optkind ++ [text "where"])
                        $$$ ppBody classIndent (map pretty gadtList)
                        $$$ ppDeriving derives

        pretty (TypeFamDecl loc name nameList optkind) =
                blankline $
                markLine loc $
                mySep ([text "type", text "family", pretty name]
                        ++ map pretty nameList
                        ++ ppOptKind optkind)
        
        pretty (DataFamDecl loc context name nameList optkind) =
                blankline $
                markLine loc $
                mySep ( [text "data", text "family", ppContext context, pretty name]
                        ++ map pretty nameList ++ ppOptKind optkind)

        pretty (TypeInsDecl loc ntype htype) =
                blankline $
                markLine loc $
                mySep [text "type", text "instance", pretty ntype, equals, pretty htype]
        
        pretty (DataInsDecl loc don ntype constrList derives) =
                blankline $
                markLine loc $
                mySep [pretty don, text "instance", pretty ntype]
                        <+> (myVcat (zipWith (<+>) (equals : repeat (char '|'))
                                                   (map pretty constrList))
                              $$$ ppDeriving derives)

        pretty (GDataInsDecl loc don ntype optkind gadtList derives) =
                blankline $
                markLine loc $
                mySep ( [pretty don, text "instance", pretty ntype]
                        ++ ppOptKind optkind ++ [text "where"])
                        $$$ ppBody classIndent (map pretty gadtList)
                        $$$ ppDeriving derives

        --m{spacing=False}
        -- special case for empty class declaration
        pretty (ClassDecl pos context name nameList fundeps []) =
                blankline $
                markLine pos $
                mySep ( [text "class", ppContext context, pretty name]
                        ++ map pretty nameList ++ [ppFunDeps fundeps])
        pretty (ClassDecl pos context name nameList fundeps declList) =
                blankline $
                markLine pos $
                mySep ( [text "class", ppContext context, pretty name]
                        ++ map pretty nameList ++ [ppFunDeps fundeps, text "where"])
                $$$ ppBody classIndent (map pretty declList)

        -- m{spacing=False}
        -- special case for empty instance declaration
        pretty (InstDecl pos context name args []) =
                blankline $
                markLine pos $
                mySep ( [text "instance", ppContext context, pretty name]
                        ++ map ppAType args)
        pretty (InstDecl pos context name args declList) =
                blankline $
                markLine pos $
                mySep ( [text "instance", ppContext context, pretty name]
                        ++ map ppAType args ++ [text "where"])
                $$$ ppBody classIndent (map pretty declList)

        pretty (DerivDecl pos context name args) =
                blankline $
                markLine pos $
                mySep ( [text "deriving", text "instance", ppContext context, pretty name]
                        ++ map ppAType args)
        pretty (DefaultDecl pos htypes) =
                blankline $
                markLine pos $
                text "default" <+> parenList (map pretty htypes)

        pretty (SpliceDecl pos splice) =
                blankline $
                markLine pos $
                pretty splice

        pretty (TypeSig pos nameList qualType) =
                blankline $
                markLine pos $
                mySep ((punctuate comma . map pretty $ nameList)
                      ++ [text "::", pretty qualType])

        pretty (FunBind matches) =
                foldr ($$$) empty (map pretty matches)

        pretty (PatBind pos pat rhs whereBinds) =
                markLine pos $
                myFsep [pretty pat, pretty rhs] $$$ ppWhere whereBinds

        pretty (InfixDecl pos assoc prec opList) =
                blankline $
                markLine pos $
                mySep ([pretty assoc, int prec]
                       ++ (punctuate comma . map pretty $ opList))

        pretty (ForImp pos cconv saf str name typ) =
                blankline $
                markLine pos $
                mySep [text "foreign import", pretty cconv, pretty saf,
                       text (show str), pretty name, text "::", pretty typ]

        pretty (ForExp pos cconv str name typ) =
                blankline $
                markLine pos $
                mySep [text "foreign export", pretty cconv,
                       text (show str), pretty name, text "::", pretty typ]

        pretty (RulePragmaDecl pos rules) = 
                blankline $
                markLine pos $
                myVcat $ text "{-# RULES" : map pretty rules ++ [text " #-}"]
                
        pretty (DeprPragmaDecl pos deprs) =
                blankline $
                markLine pos $
                myVcat $ text "{-# DEPRECATED" : map ppWarnDepr deprs ++ [text " #-}"]
                
        pretty (WarnPragmaDecl pos deprs) =
                blankline $
                markLine pos $
                myVcat $ text "{-# WARNING" : map ppWarnDepr deprs ++ [text " #-}"]

        pretty (InlineSig pos inl activ name) =
                blankline $
                markLine pos $
                mySep [text (if inl then "{-# INLINE" else "{-# NOINLINE"), pretty activ, pretty name, text "#-}"]
                
        pretty (SpecSig pos name types) =
                blankline $
                markLine pos $
                mySep $ [text "{-# SPECIALISE", pretty name, text "::"]
                    ++ punctuate comma (map pretty types) ++ [text "#-}"]

        pretty (SpecInlineSig pos inl activ name types) = 
                blankline $
                markLine pos $
                mySep $ [text "{-# SPECIALISE", text (if inl then "INLINE" else "NOINLINE"), 
                        pretty activ, pretty name, text "::"]
                        ++ (punctuate comma $ map pretty types) ++ [text "#-}"]

        pretty (InstSig pos context name args) =
                blankline $
                markLine pos $
                mySep $ [text "{-# SPECIALISE", text "instance", ppContext context, pretty name]
                            ++ map ppAType args ++ [text "#-}"]

        pretty (UnknownDeclPragma pos n s) =
                blankline $
                markLine pos $
                mySep $ [text "{-#", text n, text s, text "#-}"]


instance Pretty DataOrNew where
        pretty DataType = text "data"
        pretty NewType  = text "newtype"

instance Pretty Assoc where
        pretty AssocNone  = text "infix"
        pretty AssocLeft  = text "infixl"
        pretty AssocRight = text "infixr"

instance Pretty Match where
        pretty (Match pos f ps rhs whereBinds) =
                markLine pos $
                myFsep (lhs ++ [pretty rhs])
                $$$ ppWhere whereBinds
            where
                lhs = case ps of
                        l:r:ps' | isSymbolName f ->
                                let hd = [pretty l, ppName f, pretty r] in
                                if null ps' then hd
                                else parens (myFsep hd) : map (prettyPrec 2) ps'
                        _ -> pretty f : map (prettyPrec 2) ps

ppWhere :: Binds -> Doc
ppWhere (BDecls []) = empty
ppWhere (BDecls l)  = nest 2 (text "where" $$$ ppBody whereIndent (map pretty l))
ppWhere (IPBinds b) = nest 2 (text "where" $$$ ppBody whereIndent (map pretty b))


instance Pretty ClassDecl where
    pretty (ClsDecl decl) = pretty decl

    pretty (ClsDataFam loc context name nameList optkind) =
                markLine loc $
                mySep ( [text "data", ppContext context, pretty name]
                        ++ map pretty nameList ++ ppOptKind optkind)

    pretty (ClsTyFam loc name nameList optkind) =
                markLine loc $
                mySep ( [text "type", pretty name]
                        ++ map pretty nameList ++ ppOptKind optkind)
    
    pretty (ClsTyDef loc ntype htype) =
                markLine loc $
                mySep [text "type", pretty ntype, equals, pretty htype] 

instance Pretty InstDecl where
        pretty (InsDecl decl) = pretty decl

        pretty (InsType loc ntype htype) =
                markLine loc $
                mySep [text "type", pretty ntype, equals, pretty htype]

        pretty (InsData loc don ntype constrList derives) =
                markLine loc $
                mySep [pretty don, pretty ntype]
                        <+> (myVcat (zipWith (<+>) (equals : repeat (char '|'))
                                                   (map pretty constrList))
                              $$$ ppDeriving derives)

        pretty (InsGData loc don ntype optkind gadtList) =
                markLine loc $
                mySep ( [pretty don, pretty ntype]
                        ++ ppOptKind optkind ++ [text "where"])
                        $$$ ppBody classIndent (map pretty gadtList)


------------------------- FFI stuff -------------------------------------
instance Pretty Safety where
        pretty PlayRisky        = text "unsafe"
        pretty (PlaySafe b)     = text $ if b then "threadsafe" else "safe"

instance Pretty CallConv where
        pretty StdCall  = text "stdcall"
        pretty CCall    = text "ccall"

------------------------- Pragmas ---------------------------------------
ppWarnDepr :: ([Name], String) -> Doc
ppWarnDepr (names, txt) = mySep $ (punctuate comma $ map pretty names) ++ [text $ show txt]

instance Pretty Rule where
        pretty (Rule tag activ rvs rhs lhs) =
            mySep $ [text $ show tag, pretty activ, 
                        maybePP ppRuleVars rvs, 
                        pretty rhs, char '=', pretty lhs]

ppRuleVars :: [RuleVar] -> Doc
ppRuleVars []  = empty
ppRuleVars rvs = mySep $ text "forall" : map pretty rvs ++ [char '.']

instance Pretty Activation where
    pretty AlwaysActive    = empty
    pretty (ActiveFrom i)  = char '['  <> int i <> char ']'
    pretty (ActiveUntil i) = text "[~" <> int i <> char ']'

instance Pretty RuleVar where
    pretty (RuleVar n) = pretty n
    pretty (TypedRuleVar n t) = mySep [pretty n, text "::", pretty t]

instance Pretty OptionPragma where
    pretty (LanguagePragma _ ns) =
        myFsep $ text "{-# LANGUAGE" : map pretty ns ++ [text "#-}"]
    pretty (IncludePragma _ s) =
        myFsep $ [text "{-# INCLUDE", text s, text "#-}"]
    pretty (CFilesPragma _ s) =
        myFsep $ [text "{-# CFILES", text s, text "#-}"]
    pretty (OptionsPragma _ (Just tool) s) =
        myFsep $ [text "{-# OPTIONS_" <> pretty tool, text s, text "#-}"]
    pretty (OptionsPragma _ _ s) =
        myFsep $ [text "{-# OPTIONS", text s, text "#-}"]
    pretty (UnknownTopPragma _ n s) =
        myFsep $ map text ["{-#", n, s, "#-}"]

instance Pretty Tool where
    pretty (UnknownTool s) = text s
    pretty t               = text $ show t

------------------------- Data & Newtype Bodies -------------------------
instance Pretty QualConDecl where
        pretty (QualConDecl _pos tvs ctxt con) =
                myFsep [ppForall (Just tvs), ppContext ctxt, pretty con]

instance Pretty GadtDecl where
        pretty (GadtDecl _pos name ty) =
                myFsep [pretty name, text "::", pretty ty]

instance Pretty ConDecl where
        pretty (RecDecl name fieldList) =
                pretty name <> (braceList . map ppField $ fieldList)

        pretty (ConDecl name@(Symbol _) [l, r]) =
                myFsep [prettyPrec prec_btype l, ppName name,
                        prettyPrec prec_btype r]
        pretty (ConDecl name typeList) =
                mySep $ ppName name : map (prettyPrec prec_atype) typeList

ppField :: ([Name],BangType) -> Doc
ppField (names, ty) =
        myFsepSimple $ (punctuate comma . map pretty $ names) ++
                       [text "::", pretty ty]

instance Pretty BangType where
        prettyPrec _ (BangedTy ty) = char '!' <> ppAType ty
        prettyPrec p (UnBangedTy ty) = prettyPrec p ty
        prettyPrec p (UnpackedTy ty) = text "{-# UNPACK #-}" <+> char '!' <> prettyPrec p ty

ppDeriving :: [QName] -> Doc
ppDeriving []  = empty
ppDeriving [d] = text "deriving" <+> ppQName d
ppDeriving ds  = text "deriving" <+> parenList (map ppQName ds)

------------------------- Types -------------------------
ppBType :: Type -> Doc
ppBType = prettyPrec prec_btype

ppAType :: Type -> Doc
ppAType = prettyPrec prec_atype

-- precedences for types
prec_btype, prec_atype :: Int
prec_btype = 1  -- left argument of ->,
                -- or either argument of an infix data constructor
prec_atype = 2  -- argument of type or data constructor, or of a class

instance Pretty Type where
        prettyPrec p (TyForall mtvs ctxt htype) = parensIf (p > 0) $
                myFsep [ppForall mtvs, ppContext ctxt, pretty htype]
        prettyPrec p (TyFun a b) = parensIf (p > 0) $
                myFsep [ppBType a, text "->", pretty b]
        prettyPrec _ (TyTuple bxd l) =
                let ds = map pretty l
                 in case bxd of
                        Boxed   -> parenList ds
                        Unboxed -> hashParenList ds
        prettyPrec p (TyApp a b)
                | a == list_tycon = brackets $ pretty b         -- special case
                | otherwise = parensIf (p > prec_btype) $
                        myFsep [pretty a, ppAType b]
        prettyPrec _ (TyVar name) = pretty name
        prettyPrec _ (TyCon name) = pretty name
        prettyPrec _ (TyPred asst) = pretty asst
        prettyPrec _ (TyInfix a op b) = parens (myFsep [pretty op, pretty a, pretty b])
        prettyPrec _ (TyKind t k) = parens (myFsep [pretty t, text "::", pretty k])


instance Pretty TyVarBind where
        pretty (KindedVar var kind) = myFsep [pretty var, text "::", pretty kind]
        pretty (UnkindedVar var)    = pretty var

ppForall :: Maybe [TyVarBind] -> Doc
ppForall Nothing   = empty
ppForall (Just []) = empty
ppForall (Just vs) =    myFsep (text "forall" : map pretty vs ++ [char '.'])

---------------------------- Kinds ----------------------------

instance Pretty Kind where
        pretty KindStar     = text "*"
        pretty KindBang     = text "!"
        pretty (KindFn a b) = myFsep [pretty a, text "->", pretty b]

ppOptKind :: Maybe Kind -> [Doc]
ppOptKind Nothing  = []
ppOptKind (Just k) = [text "::", pretty k]

------------------- Functional Dependencies -------------------
instance Pretty FunDep where
        pretty (FunDep from to) =
                myFsep $ map pretty from ++ [text "->"] ++ map pretty to


ppFunDeps :: [FunDep] -> Doc
ppFunDeps []  = empty
ppFunDeps fds = myFsep $ (char '|':) . punctuate comma . map pretty $ fds

------------------------- Expressions -------------------------
instance Pretty Rhs where
        pretty (UnGuardedRhs e) = equals <+> pretty e
        pretty (GuardedRhss guardList) = myVcat . map pretty $ guardList

instance Pretty GuardedRhs where
        pretty (GuardedRhs _pos guards ppBody) =
                myFsep $ [char '|'] ++ (punctuate comma . map pretty $ guards) ++ [equals, pretty ppBody]

instance Pretty Literal where
        pretty (Int i)        = integer i
        pretty (Char c)       = text (show c)
        pretty (String s)     = text (show s)
        pretty (Frac r)       = double (fromRational r)
        -- GHC unboxed literals:
        pretty (CharPrim c)   = text (show c)           <> char '#'
        pretty (StringPrim s) = text (show s)           <> char '#'
        pretty (IntPrim i)    = integer i               <> char '#'
        pretty (FloatPrim r)  = float  (fromRational r) <> char '#'
        pretty (DoublePrim r) = double (fromRational r) <> text "##"

instance Pretty Exp where
        pretty (Lit l) = pretty l
        -- lambda stuff
        pretty (InfixApp a op b) = myFsep [pretty a, pretty op, pretty b]
        pretty (NegApp e) = myFsep [char '-', pretty e]
        pretty (App a b) = myFsep [pretty a, pretty b]
        pretty (Lambda _loc expList ppBody) = myFsep $
                char '\\' : map pretty expList ++ [text "->", pretty ppBody]
        -- keywords
        -- two cases for lets
        pretty (Let (BDecls declList) letBody) =
                ppLetExp declList letBody
        pretty (Let (IPBinds bindList) letBody) =
                ppLetExp bindList letBody

        pretty (If cond thenexp elsexp) =
                myFsep [text "if", pretty cond,
                        text "then", pretty thenexp,
                        text "else", pretty elsexp]
        pretty (Case cond altList) =
                myFsep [text "case", pretty cond, text "of"]
                $$$ ppBody caseIndent (map pretty altList)
        pretty (Do stmtList) =
                text "do" $$$ ppBody doIndent (map pretty stmtList)
        pretty (MDo stmtList) =
                text "mdo" $$$ ppBody doIndent (map pretty stmtList)
        -- Constructors & Vars
        pretty (Var name) = pretty name
        pretty (IPVar ipname) = pretty ipname
        pretty (Con name) = pretty name
        pretty (Tuple expList) = parenList . map pretty $ expList
        -- weird stuff
        pretty (Paren e) = parens . pretty $ e
        pretty (LeftSection e op) = parens (pretty e <+> pretty op)
        pretty (RightSection op e) = parens (pretty op <+> pretty e)
        pretty (RecConstr c fieldList) =
                pretty c <> (braceList . map pretty $ fieldList)
        pretty (RecUpdate e fieldList) =
                pretty e <> (braceList . map pretty $ fieldList)
        -- Lists
        pretty (List list) =
                bracketList . punctuate comma . map pretty $ list
        pretty (EnumFrom e) =
                bracketList [pretty e, text ".."]
        pretty (EnumFromTo from to) =
                bracketList [pretty from, text "..", pretty to]
        pretty (EnumFromThen from thenE) =
                bracketList [pretty from <> comma, pretty thenE, text ".."]
        pretty (EnumFromThenTo from thenE to) =
                bracketList [pretty from <> comma, pretty thenE,
                             text "..", pretty to]
        pretty (ListComp e stmtList) =
                bracketList ([pretty e, char '|']
                             ++ (punctuate comma . map pretty $ stmtList))
        pretty (ExpTypeSig _pos e ty) =
                myFsep [pretty e, text "::", pretty ty]
        -- Template Haskell
        pretty (BracketExp b) = pretty b
        pretty (SpliceExp s) = pretty s
        pretty (TypQuote t)  = text "\'\'" <> pretty t
        pretty (VarQuote x)  = text "\'" <> pretty x
        -- Hsx
        pretty (XTag _ n attrs mattr cs) =
                let ax = maybe [] (return . pretty) mattr
                 in hcat $
                     (myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [char '>']):
                        map pretty cs ++ [myFsep $ [text "</" <> pretty n, char '>']]
        pretty (XETag _ n attrs mattr) =
                let ax = maybe [] (return . pretty) mattr
                 in myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [text "/>"]
        pretty (XPcdata s) = text s
        pretty (XExpTag e) =
                myFsep $ [text "<%", pretty e, text "%>"]
        -- Pragmas
        pretty (CorePragma s) = myFsep $ map text ["{-# CORE", show s, "#-}"]
        pretty (SCCPragma  s) = myFsep $ map text ["{-# SCC",  show s, "#-}"]
        pretty (GenPragma  s (a,b) (c,d)) =
                myFsep $ [text "{-# GENERATED", text $ show s, 
                            int a, char ':', int b, char '-', 
                            int c, char ':', int d, text "#-}"]
        pretty (UnknownExpPragma n s) = 
                myFsep $ [text "{-#", text n, text s, text "#-}"]

instance Pretty XAttr where
        pretty (XAttr n v) =
                myFsep [pretty n, char '=', pretty v]

instance Pretty XName where
        pretty (XName n) = text n
        pretty (XDomName d n) = text d <> char ':' <> text n

--ppLetExp :: [Decl] -> Exp -> Doc
ppLetExp l b = myFsep [text "let" <+> ppBody letIndent (map pretty l),
                        text "in", pretty b]

ppWith binds = nest 2 (text "with" $$$ ppBody withIndent (map pretty binds))
withIndent = whereIndent

--------------------- Template Haskell -------------------------

instance Pretty Bracket where
        pretty (ExpBracket e) = ppBracket "[|" e
        pretty (PatBracket p) = ppBracket "[p|" p
        pretty (TypeBracket t) = ppBracket "[t|" t
        pretty (DeclBracket d) =
                myFsep $ text "[d|" : map pretty d ++ [text "|]"]

ppBracket o x = myFsep [text o, pretty x, text "|]"]

instance Pretty Splice where
        pretty (IdSplice s) = char '$' <> text s
        pretty (ParenSplice e) =
                myFsep [text "$(", pretty e, char ')']

------------------------- Patterns -----------------------------

instance Pretty Pat where
        prettyPrec _ (PVar name) = pretty name
        prettyPrec _ (PLit lit) = pretty lit
        prettyPrec _ (PNeg p) = myFsep [char '-', pretty p]
        prettyPrec p (PInfixApp a op b) = parensIf (p > 0) $
                myFsep [pretty a, pretty (QConOp op), pretty b]
        prettyPrec p (PApp n ps) = parensIf (p > 1) $
                myFsep (pretty n : map pretty ps)
        prettyPrec _ (PTuple ps) = parenList . map pretty $ ps
        prettyPrec _ (PList ps) =
                bracketList . punctuate comma . map pretty $ ps
        prettyPrec _ (PParen p) = parens . pretty $ p
        prettyPrec _ (PRec c fields) =
                pretty c <> (braceList . map pretty $ fields)
        -- special case that would otherwise be buggy
        prettyPrec _ (PAsPat name (PIrrPat pat)) =
                myFsep [pretty name <> char '@', char '~' <> pretty pat]
        prettyPrec _ (PAsPat name pat) =
                hcat [pretty name, char '@', pretty pat]
        prettyPrec _ PWildCard = char '_'
        prettyPrec _ (PIrrPat pat) = char '~' <> pretty pat
        prettyPrec _ (PatTypeSig _pos pat ty) =
                myFsep [pretty pat, text "::", pretty ty]
        prettyPrec _ (PViewPat e p) =
                myFsep [pretty e, text "->", pretty p]
        -- HaRP
        prettyPrec _ (PRPat rs) = 
                bracketList . punctuate comma . map pretty $ rs
        -- Hsx
        prettyPrec _ (PXTag _ n attrs mattr cp) =
            let ap = maybe [] (return . pretty) mattr
             in hcat $ -- TODO: should not introduce blanks
                  (myFsep $ (char '<' <> pretty n): map pretty attrs ++ ap ++ [char '>']):
                    map pretty cp ++ [myFsep $ [text "</" <> pretty n, char '>']]
        prettyPrec _ (PXETag _ n attrs mattr) =
                let ap = maybe [] (return . pretty) mattr
                 in myFsep $ (char '<' <> pretty n): map pretty attrs ++ ap ++ [text "/>"]
        prettyPrec _ (PXPcdata s) = text s
        prettyPrec _ (PXPatTag p) =
                myFsep $ [text "<%", pretty p, text "%>"]
        prettyPrec _ (PXRPats ps) =
                myFsep $ text "<[" : map pretty ps ++ [text "%>"]

instance Pretty PXAttr where
        pretty (PXAttr n p) =
                myFsep [pretty n, char '=', pretty p]

instance Pretty PatField where
        pretty (PFieldPat name pat) =
                myFsep [pretty name, equals, pretty pat]

--------------------- Regular Patterns -------------------------

instance Pretty RPat where
        pretty (RPOp r op) = pretty r <> pretty op
        pretty (RPEither r1 r2) = parens . myFsep $
                [pretty r1, char '|', pretty r2]
        pretty (RPSeq rs) =
                myFsep $ text "(/" : map pretty rs ++ [text "/)"]
        pretty (RPGuard r gs) =
                myFsep $ text "(|" : pretty r : char '|' : map pretty gs ++ [text "|)"]
        -- special case that would otherwise be buggy
        pretty (RPCAs n (RPPat (PIrrPat p))) =
                myFsep [pretty n <> text "@:", char '~' <> pretty p]
        pretty (RPCAs n r) = hcat [pretty n, text "@:", pretty r]
        -- special case that would otherwise be buggy
        pretty (RPAs n (RPPat (PIrrPat p))) =
                myFsep [pretty n <> text "@:", char '~' <> pretty p]
        pretty (RPAs n r) = hcat [pretty n, char '@', pretty r]
        pretty (RPPat p) = pretty p
        pretty (RPParen rp) = parens . pretty $ rp

instance Pretty RPatOp where
        pretty RPStar  = char '*'
        pretty RPStarG = text "*!"
        pretty RPPlus  = char '+'
        pretty RPPlusG = text "+!"
        pretty RPOpt   = char '?'
        pretty RPOptG  = text "?!"

------------------------- Case bodies  -------------------------
instance Pretty Alt where
        pretty (Alt _pos e gAlts binds) =
                pretty e <+> pretty gAlts $$$ ppWhere binds

instance Pretty GuardedAlts where
        pretty (UnGuardedAlt e) = text "->" <+> pretty e
        pretty (GuardedAlts altList) = myVcat . map pretty $ altList

instance Pretty GuardedAlt where
        pretty (GuardedAlt _pos guards body) =
                myFsep $ char '|': (punctuate comma . map pretty $ guards) ++ [text "->", pretty body]

------------------------- Statements in monads, guards & list comprehensions -----
instance Pretty Stmt where
        pretty (Generator _loc e from) =
                pretty e <+> text "<-" <+> pretty from
        pretty (Qualifier e) = pretty e
        -- two cases for lets
        pretty (LetStmt (BDecls declList)) =
                ppLetStmt declList
        pretty (LetStmt (IPBinds bindList)) =
                ppLetStmt bindList

ppLetStmt l = text "let" $$$ ppBody letIndent (map pretty l)

------------------------- Record updates
instance Pretty FieldUpdate where
        pretty (FieldUpdate name e) =
                myFsep [pretty name, equals, pretty e]

------------------------- Names -------------------------
instance Pretty QOp where
        pretty (QVarOp n) = ppQNameInfix n
        pretty (QConOp n) = ppQNameInfix n

ppQNameInfix :: QName -> Doc
ppQNameInfix name
        | isSymbolName (getName name) = ppQName name
        | otherwise = char '`' <> ppQName name <> char '`'

instance Pretty QName where
        pretty name = case name of
                UnQual (Symbol ('#':_)) -> char '(' <+> ppQName name <+> char ')'
                _ -> parensIf (isSymbolName (getName name)) (ppQName name)

ppQName :: QName -> Doc
ppQName (UnQual name) = ppName name
ppQName (Qual m name) = pretty m <> char '.' <> ppName name
ppQName (Special sym) = text (specialName sym)

instance Pretty Op where
        pretty (VarOp n) = ppNameInfix n
        pretty (ConOp n) = ppNameInfix n

ppNameInfix :: Name -> Doc
ppNameInfix name
        | isSymbolName name = ppName name
        | otherwise = char '`' <> ppName name <> char '`'

instance Pretty Name where
        pretty name = case name of
                Symbol ('#':_) -> char '(' <+> ppName name <+> char ')'
                _ -> parensIf (isSymbolName name) (ppName name)

ppName :: Name -> Doc
ppName (Ident s) = text s
ppName (Symbol s) = text s

instance Pretty IPName where
        pretty (IPDup s) = char '?' <> text s
        pretty (IPLin s) = char '%' <> text s

instance Pretty IPBind where
        pretty (IPBind _loc ipname exp) =
                myFsep [pretty ipname, equals, pretty exp]

instance Pretty CName where
        pretty (VarName n) = pretty n
        pretty (ConName n) = pretty n

isSymbolName :: Name -> Bool
isSymbolName (Symbol _) = True
isSymbolName _ = False

getName :: QName -> Name
getName (UnQual s) = s
getName (Qual _ s) = s
getName (Special Cons) = Symbol ":"
getName (Special FunCon) = Symbol "->"
getName (Special s) = Ident (specialName s)

specialName :: SpecialCon -> String
specialName UnitCon = "()"
specialName ListCon = "[]"
specialName FunCon = "->"
specialName (TupleCon n) = "(" ++ replicate (n-1) ',' ++ ")"
specialName Cons = ":"

ppContext :: Context -> Doc
ppContext []      = empty
ppContext context = mySep [parenList (map pretty context), text "=>"]

-- hacked for multi-parameter type classes
instance Pretty Asst where
        pretty (ClassA a ts)  = myFsep $ ppQName a : map ppAType ts
        pretty (IParam i t)   = myFsep $ [pretty i, text "::", pretty t]
        pretty (EqualP t1 t2) = myFsep $ [pretty t1, text "~", pretty t2]

------------------------- pp utils -------------------------
maybePP :: (a -> Doc) -> Maybe a -> Doc
maybePP pp Nothing = empty
maybePP pp (Just a) = pp a

parenList :: [Doc] -> Doc
parenList = parens . myFsepSimple . punctuate comma

hashParenList :: [Doc] -> Doc
hashParenList = hashParens . myFsepSimple . punctuate comma
  where hashParens = parens . hashes
        hashes = \doc -> char '#' <> doc <> char '#'

braceList :: [Doc] -> Doc
braceList = braces . myFsepSimple . punctuate comma

bracketList :: [Doc] -> Doc
bracketList = brackets . myFsepSimple

-- Wrap in braces and semicolons, with an extra space at the start in
-- case the first doc begins with "-", which would be scanned as {-
flatBlock :: [Doc] -> Doc
flatBlock = braces . (space <>) . hsep . punctuate semi

-- Same, but put each thing on a separate line
prettyBlock :: [Doc] -> Doc
prettyBlock = braces . (space <>) . vcat . punctuate semi

-- Monadic PP Combinators -- these examine the env

blankline :: Doc -> Doc
blankline dl = do{e<-getPPEnv;if spacing e && layout e /= PPNoLayout
                              then space $$ dl else dl}
topLevel :: Doc -> [Doc] -> Doc
topLevel header dl = do
         e <- fmap layout getPPEnv
         case e of
             PPOffsideRule -> header $$ vcat dl
             PPSemiColon -> header $$ prettyBlock dl
             PPInLine -> header $$ prettyBlock dl
             PPNoLayout -> header <+> flatBlock dl

ppBody :: (PPHsMode -> Int) -> [Doc] -> Doc
ppBody f dl = do
         e <- fmap layout getPPEnv
         case e of PPOffsideRule -> indent
                   PPSemiColon   -> indentExplicit
                   _ -> flatBlock dl
                   where
                   indent  = do{i <-fmap f getPPEnv;nest i . vcat $ dl}
                   indentExplicit = do {i <- fmap f getPPEnv;
                           nest i . prettyBlock $ dl}

($$$) :: Doc -> Doc -> Doc
a $$$ b = layoutChoice (a $$) (a <+>) b

mySep :: [Doc] -> Doc
mySep = layoutChoice mySep' hsep
        where
        -- ensure paragraph fills with indentation.
        mySep' [x]    = x
        mySep' (x:xs) = x <+> fsep xs
        mySep' []     = error "Internal error: mySep"

myVcat :: [Doc] -> Doc
myVcat = layoutChoice vcat hsep

myFsepSimple :: [Doc] -> Doc
myFsepSimple = layoutChoice fsep hsep

-- same, except that continuation lines are indented,
-- which is necessary to avoid triggering the offside rule.
myFsep :: [Doc] -> Doc
myFsep = layoutChoice fsep' hsep
        where   fsep' [] = empty
                fsep' (d:ds) = do
                        e <- getPPEnv
                        let n = onsideIndent e
                        nest n (fsep (nest (-n) d:ds))

layoutChoice :: (a -> Doc) -> (a -> Doc) -> a -> Doc
layoutChoice a b dl = do e <- getPPEnv
                         if layout e == PPOffsideRule ||
                            layout e == PPSemiColon
                          then a dl else b dl

-- Prefix something with a LINE pragma, if requested.
-- GHC's LINE pragma actually sets the current line number to n-1, so
-- that the following line is line n.  But if there's no newline before
-- the line we're talking about, we need to compensate by adding 1.

markLine :: SrcLoc -> Doc -> Doc
markLine loc doc = do
        e <- getPPEnv
        let y = srcLine loc
        let line l =
              text ("{-# LINE " ++ show l ++ " \"" ++ srcFilename loc ++ "\" #-}")
        if linePragmas e then layoutChoice (line y $$) (line (y+1) <+>) doc
              else doc

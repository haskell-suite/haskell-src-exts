-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Pretty
-- Copyright   :  (c) Niklas Broberg 2004-2009,
--                (c) The GHC Team, Noel Winstanley 1997-2000
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
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
import qualified Language.Haskell.Exts.Annotated.Syntax as A
import Language.Haskell.Exts.Annotated.Simplify
import qualified Language.Haskell.Exts.ParseSyntax as P

import Language.Haskell.Exts.SrcLoc

import qualified Text.PrettyPrint as P
import Data.List (intersperse)

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
                linePragmas :: Bool
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
                      linePragmas = False
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
-- in "Language.Haskell.Exts.Syntax" and "Language.Haskell.Exts.Annotated.Syntax".
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
                    (if m == ModuleName "" then id
                     else \x -> [topLevel (ppModuleHeader m mbWarn mbExports) x])
                    (map pretty imp ++ map pretty decls)

--------------------------  Module Header ------------------------------
ppModuleHeader :: ModuleName -> Maybe WarningText -> Maybe [ExportSpec] -> Doc
ppModuleHeader m mbWarn mbExportList = mySep [
        text "module",
        pretty m,
        maybePP ppWarnTxt mbWarn,
        maybePP (parenList . map pretty) mbExportList,
        text "where"]

ppWarnTxt :: WarningText -> Doc
ppWarnTxt (DeprText s) = mySep [text "{-# DEPRECATED", text (show s), text "#-}"]
ppWarnTxt (WarnText s) = mySep [text "{-# WARNING",    text (show s), text "#-}"]

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
        pretty (ImportDecl pos m qual src mbPkg mbName mbSpecs) =
                markLine pos $
                mySep [text "import",
                       if src  then text "{-# SOURCE #-}" else empty,
                       if qual then text "qualified" else empty,
                       maybePP (\s -> text (show s)) mbPkg,
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
                        $$$ ppBody letIndent [ppDeriving derives]

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

        pretty (FunBind matches) = do
                e <- fmap layout getPPEnv
                case e of PPOffsideRule -> foldr ($$$) empty (map pretty matches)
                          _ -> foldr (\x y -> x <> semi <> y) empty (map pretty matches)

        pretty (PatBind pos pat optsig rhs whereBinds) =
                markLine pos $
                myFsep [pretty pat, maybePP ppSig optsig, pretty rhs] $$$ ppWhere whereBinds

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

        pretty (InlineConlikeSig pos activ name) =
                blankline $
                markLine pos $
                mySep [text "{-# INLINE_CONLIKE", pretty activ, pretty name, text "#-}"]

        pretty (SpecSig pos activ name types) =
                blankline $
                markLine pos $
                mySep $ [text "{-# SPECIALISE", pretty activ, pretty name, text "::"]
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

        pretty (AnnPragma pos ann) =
                blankline $
                markLine pos $
                mySep $ [text "{-# ANN", pretty ann, text "#-}"]

instance Pretty Annotation where
        pretty (Ann n e) = myFsep [pretty n, pretty e]
        pretty (TypeAnn n e) = myFsep [text "type", pretty n, pretty e]
        pretty (ModuleAnn e) = myFsep [text "module", pretty e]

instance Pretty DataOrNew where
        pretty DataType = text "data"
        pretty NewType  = text "newtype"

instance Pretty Assoc where
        pretty AssocNone  = text "infix"
        pretty AssocLeft  = text "infixl"
        pretty AssocRight = text "infixr"

instance Pretty Match where
        pretty (Match pos f ps optsig rhs whereBinds) =
                markLine pos $
                myFsep (lhs ++ [maybePP ppSig optsig, pretty rhs])
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

ppSig :: Type -> Doc
ppSig t = text "::" <+> pretty t

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

        pretty (InsGData loc don ntype optkind gadtList derives) =
                markLine loc $
                mySep ( [pretty don, pretty ntype]
                        ++ ppOptKind optkind ++ [text "where"])
                        $$$ ppBody classIndent (map pretty gadtList)
                        $$$ ppDeriving derives

--        pretty (InsInline loc inl activ name) =
--                markLine loc $
--                mySep [text (if inl then "{-# INLINE" else "{-# NOINLINE"), pretty activ, pretty name, text "#-}"]


------------------------- FFI stuff -------------------------------------
instance Pretty Safety where
        pretty PlayRisky        = text "unsafe"
        pretty (PlaySafe b)     = text $ if b then "threadsafe" else "safe"
        pretty PlayInterruptible = text "interruptible"

instance Pretty CallConv where
        pretty StdCall   = text "stdcall"
        pretty CCall     = text "ccall"
        pretty CPlusPlus = text "cplusplus"
        pretty DotNet    = text "dotnet"
        pretty Jvm       = text "jvm"
        pretty Js        = text "js"

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
    pretty (TypedRuleVar n t) = parens $ mySep [pretty n, text "::", pretty t]

instance Pretty ModulePragma where
    pretty (LanguagePragma _ ns) =
        myFsep $ text "{-# LANGUAGE" : punctuate (char ',') (map pretty ns) ++ [text "#-}"]
    pretty (OptionsPragma _ (Just tool) s) =
        myFsep $ [text "{-# OPTIONS_" <> pretty tool, text s, text "#-}"]
    pretty (OptionsPragma _ _ s) =
        myFsep $ [text "{-# OPTIONS", text s, text "#-}"]
    pretty (AnnModulePragma _ ann) =
        myFsep $ [text "{-# ANN", pretty ann, text "#-}"]


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

{-        pretty (ConDecl name@(Symbol _) [l, r]) =
                myFsep [prettyPrec prec_btype l, ppName name,
                        prettyPrec prec_btype r] -}
        pretty (ConDecl name typeList) =
                mySep $ ppName name : map (prettyPrec prec_atype) typeList
        pretty (InfixConDecl l name r) =
                myFsep [prettyPrec prec_btype l, ppNameInfix name,
                         prettyPrec prec_btype r]

ppField :: ([Name],BangType) -> Doc
ppField (names, ty) =
        myFsepSimple $ (punctuate comma . map pretty $ names) ++
                       [text "::", pretty ty]

instance Pretty BangType where
        prettyPrec _ (BangedTy ty) = char '!' <> ppAType ty
        prettyPrec p (UnBangedTy ty) = prettyPrec p ty
        prettyPrec p (UnpackedTy ty) = text "{-# UNPACK #-}" <+> char '!' <> prettyPrec p ty

ppDeriving :: [Deriving] -> Doc
ppDeriving []  = empty
ppDeriving [(d, [])] = text "deriving" <+> ppQName d
ppDeriving ds  = text "deriving" <+> parenList (map ppDer ds)
    where ppDer :: (QName, [Type]) -> Doc
          ppDer (n, ts) = mySep (pretty n : map pretty ts)

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
        prettyPrec _ (TyList t)  = brackets $ pretty t
        prettyPrec p (TyApp a b) =
                {-
                | a == list_tycon = brackets $ pretty b         -- special case
                | otherwise = -} parensIf (p > prec_btype) $
                                    myFsep [pretty a, ppAType b]
        prettyPrec _ (TyVar name) = pretty name
        prettyPrec _ (TyCon name) = pretty name
        prettyPrec _ (TyParen t) = parens (pretty t)
--        prettyPrec _ (TyPred asst) = pretty asst
        prettyPrec _ (TyInfix a op b) = myFsep [pretty a, ppQNameInfix op, pretty b]
        prettyPrec _ (TyKind t k) = parens (myFsep [pretty t, text "::", pretty k])


instance Pretty TyVarBind where
        pretty (KindedVar var kind) = parens $ myFsep [pretty var, text "::", pretty kind]
        pretty (UnkindedVar var)    = pretty var

ppForall :: Maybe [TyVarBind] -> Doc
ppForall Nothing   = empty
ppForall (Just []) = empty
ppForall (Just vs) =    myFsep (text "forall" : map pretty vs ++ [char '.'])

---------------------------- Kinds ----------------------------

instance Pretty Kind where
        prettyPrec _ KindStar      = text "*"
        prettyPrec _ KindBang      = text "!"
        prettyPrec n (KindFn a b)  = parensIf (n > 0) $ myFsep [prettyPrec 1 a, text "->", pretty b]
        prettyPrec _ (KindParen k) = parens $ pretty k
        prettyPrec _ (KindVar n)   = pretty n

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
        pretty (PrimChar c)   = text (show c)           <> char '#'
        pretty (PrimString s) = text (show s)           <> char '#'
        pretty (PrimInt i)    = integer i               <> char '#'
        pretty (PrimWord w)   = integer w               <> text "##"
        pretty (PrimFloat r)  = float  (fromRational r) <> char '#'
        pretty (PrimDouble r) = double (fromRational r) <> text "##"

instance Pretty Exp where
        prettyPrec _ (Lit l) = pretty l
        -- lambda stuff
        prettyPrec p (InfixApp a op b) = parensIf (p > 2) $ myFsep [prettyPrec 2 a, pretty op, prettyPrec 1 b]
        prettyPrec p (NegApp e) = parensIf (p > 0) $ char '-' <> prettyPrec 4 e
        prettyPrec p (App a b) = parensIf (p > 3) $ myFsep [prettyPrec 3 a, prettyPrec 4 b]
        prettyPrec p (Lambda _loc patList ppBody) = parensIf (p > 1) $ myFsep $
                char '\\' : map (prettyPrec 2) patList ++ [text "->", pretty ppBody]
        -- keywords
        -- two cases for lets
        prettyPrec p (Let (BDecls declList) letBody) =
                parensIf (p > 1) $ ppLetExp declList letBody
        prettyPrec p (Let (IPBinds bindList) letBody) =
                parensIf (p > 1) $ ppLetExp bindList letBody

        prettyPrec p (If cond thenexp elsexp) = parensIf (p > 1) $
                myFsep [text "if", pretty cond,
                        text "then", pretty thenexp,
                        text "else", pretty elsexp]
        prettyPrec p (Case cond altList) = parensIf (p > 1) $
                myFsep [text "case", pretty cond, text "of"]
                $$$ ppBody caseIndent (map pretty altList)
        prettyPrec p (Do stmtList) = parensIf (p > 1) $
                text "do" $$$ ppBody doIndent (map pretty stmtList)
        prettyPrec p (MDo stmtList) = parensIf (p > 1) $
                text "mdo" $$$ ppBody doIndent (map pretty stmtList)
        -- Constructors & Vars
        prettyPrec _ (Var name) = pretty name
        prettyPrec _ (IPVar ipname) = pretty ipname
        prettyPrec _ (Con name) = pretty name
        prettyPrec _ (Tuple bxd expList) =
                let ds = map pretty expList
                in case bxd of
                       Boxed   -> parenList ds
                       Unboxed -> hashParenList ds
        prettyPrec _ (TupleSection bxd mExpList) =
                let ds = map (maybePP pretty) mExpList
                in case bxd of
                       Boxed   -> parenList ds
                       Unboxed -> hashParenList ds
        -- weird stuff
        prettyPrec _ (Paren e) = parens . pretty $ e
        prettyPrec _ (LeftSection e op) = parens (pretty e <+> pretty op)
        prettyPrec _ (RightSection op e) = parens (pretty op <+> pretty e)
        prettyPrec _ (RecConstr c fieldList) =
                pretty c <> (braceList . map pretty $ fieldList)
        prettyPrec _ (RecUpdate e fieldList) =
                pretty e <> (braceList . map pretty $ fieldList)
        -- Lists
        prettyPrec _ (List list) =
                bracketList . punctuate comma . map pretty $ list
        prettyPrec _ (EnumFrom e) =
                bracketList [pretty e, text ".."]
        prettyPrec _ (EnumFromTo from to) =
                bracketList [pretty from, text "..", pretty to]
        prettyPrec _ (EnumFromThen from thenE) =
                bracketList [pretty from <> comma, pretty thenE, text ".."]
        prettyPrec _ (EnumFromThenTo from thenE to) =
                bracketList [pretty from <> comma, pretty thenE,
                             text "..", pretty to]
        prettyPrec _ (ListComp e qualList) =
                bracketList ([pretty e, char '|']
                             ++ (punctuate comma . map pretty $ qualList))
        prettyPrec _ (ParComp e qualLists) =
                bracketList (punctuate (char '|') $
                                pretty e : (map (hsep . punctuate comma . map pretty) $ qualLists))
        prettyPrec p (ExpTypeSig _pos e ty) = parensIf (p > 0) $
                myFsep [pretty e, text "::", pretty ty]
        -- Template Haskell
        prettyPrec _ (BracketExp b) = pretty b
        prettyPrec _ (SpliceExp s) = pretty s
        prettyPrec _ (TypQuote t)  = text "\'\'" <> pretty t
        prettyPrec _ (VarQuote x)  = text "\'" <> pretty x
        prettyPrec _ (QuasiQuote n qt) = text ("[" ++ n ++ "|" ++ qt ++ "|]")
        -- Hsx
        prettyPrec _ (XTag _ n attrs mattr cs) =
                let ax = maybe [] (return . pretty) mattr
                 in hcat $
                     (myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [char '>']):
                        map pretty cs ++ [myFsep $ [text "</" <> pretty n, char '>']]
        prettyPrec _ (XETag _ n attrs mattr) =
                let ax = maybe [] (return . pretty) mattr
                 in myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [text "/>"]
        prettyPrec _ (XPcdata s) = text s
        prettyPrec _ (XExpTag e) =
                myFsep $ [text "<%", pretty e, text "%>"]
        prettyPrec _ (XChildTag _ cs) =
                myFsep $ text "<%>" : map pretty cs ++ [text "</%>"]

        -- Pragmas
        prettyPrec p (CorePragma s e) = myFsep $ map text ["{-# CORE", show s, "#-}"] ++ [pretty e]
        prettyPrec _ (SCCPragma  s e) = myFsep $ map text ["{-# SCC",  show s, "#-}"] ++ [pretty e]
        prettyPrec _ (GenPragma  s (a,b) (c,d) e) =
                myFsep $ [text "{-# GENERATED", text $ show s,
                            int a, char ':', int b, char '-',
                            int c, char ':', int d, text "#-}", pretty e]
        -- Arrows
        prettyPrec p (Proc _ pat e) = parensIf (p > 1) $ myFsep $ [text "proc", pretty pat, text "->", pretty e]
        prettyPrec p (LeftArrApp l r)      = parensIf (p > 0) $ myFsep $ [pretty l, text "-<",  pretty r]
        prettyPrec p (RightArrApp l r)     = parensIf (p > 0) $ myFsep $ [pretty l, text ">-",  pretty r]
        prettyPrec p (LeftArrHighApp l r)  = parensIf (p > 0) $ myFsep $ [pretty l, text "-<<", pretty r]
        prettyPrec p (RightArrHighApp l r) = parensIf (p > 0) $ myFsep $ [pretty l, text ">>-", pretty r]


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
        prettyPrec p (PNeg pat) = parensIf (p > 0) $ myFsep [char '-', pretty pat]
        prettyPrec p (PInfixApp a op b) = parensIf (p > 0) $
                myFsep [prettyPrec 1 a, pretty (QConOp op), prettyPrec 1 b]
        prettyPrec p (PApp n ps) = parensIf (p > 1 && not (null ps)) $
                myFsep (pretty n : map (prettyPrec 2) ps)
        prettyPrec _ (PTuple bxd ps) =
                let ds = map pretty ps
                in case bxd of
                       Boxed   -> parenList ds
                       Unboxed -> hashParenList ds
        prettyPrec _ (PList ps) =
                bracketList . punctuate comma . map pretty $ ps
        prettyPrec _ (PParen pat) = parens . pretty $ pat
        prettyPrec _ (PRec c fields) =
                pretty c <> (braceList . map pretty $ fields)
        -- special case that would otherwise be buggy
        prettyPrec _ (PAsPat name (PIrrPat pat)) =
                myFsep [pretty name <> char '@', char '~' <> prettyPrec 2 pat]
        prettyPrec _ (PAsPat name pat) =
                hcat [pretty name, char '@', prettyPrec 2 pat]
        prettyPrec _ PWildCard = char '_'
        prettyPrec _ (PIrrPat pat) = char '~' <> prettyPrec 2 pat
        prettyPrec p (PatTypeSig _pos pat ty) = parensIf (p > 0) $
                myFsep [pretty pat, text "::", pretty ty]
        prettyPrec p (PViewPat e pat) = parensIf (p > 0) $
                myFsep [pretty e, text "->", pretty pat]
        prettyPrec p (PNPlusK n k) = parensIf (p > 0) $
                myFsep [pretty n, text "+", text $ show k]
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
        -- Generics
        prettyPrec _ (PExplTypeArg qn t) =
                myFsep [pretty qn, text "{|", pretty t, text "|}"]
        -- BangPatterns
        prettyPrec _ (PBangPat pat) = text "!" <> prettyPrec 2 pat

instance Pretty PXAttr where
        pretty (PXAttr n p) =
                myFsep [pretty n, char '=', pretty p]

instance Pretty PatField where
        pretty (PFieldPat name pat) =
                myFsep [pretty name, equals, pretty pat]
        pretty (PFieldPun name) = pretty name
        pretty (PFieldWildcard) = text ".."

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
        pretty (RecStmt stmtList) =
                text "rec" $$$ ppBody letIndent (map pretty stmtList)

ppLetStmt l = text "let" $$$ ppBody letIndent (map pretty l)

instance Pretty QualStmt where
        pretty (QualStmt s) = pretty s
        pretty (ThenTrans    f)    = myFsep $ [text "then", pretty f]
        pretty (ThenBy       f e)  = myFsep $ [text "then", pretty f, text "by", pretty e]
        pretty (GroupBy      e)    = myFsep $ [text "then", text "group", text "by", pretty e]
        pretty (GroupUsing   f)    = myFsep $ [text "then", text "group", text "using", pretty f]
        pretty (GroupByUsing e f)  = myFsep $ [text "then", text "group", text "by",
                                                pretty e, text "using", pretty f]



------------------------- Record updates
instance Pretty FieldUpdate where
        pretty (FieldUpdate name e) =
                myFsep [pretty name, equals, pretty e]
        pretty (FieldPun name) = pretty name
        pretty (FieldWildcard) = text ".."

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

instance Pretty SpecialCon where
        pretty sc = text $ specialName sc

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
specialName (TupleCon b n) = "(" ++ hash ++ replicate (n-1) ',' ++ hash ++ ")"
    where hash = if b == Unboxed then "#" else ""
specialName Cons = ":"
specialName UnboxedSingleCon = "(# #)"

ppContext :: Context -> Doc
ppContext []      = empty
ppContext context = mySep [parenList (map pretty context), text "=>"]

-- hacked for multi-parameter type classes
instance Pretty Asst where
        pretty (ClassA a ts)   = myFsep $ ppQName a : map ppAType ts
        pretty (InfixA a op b) = myFsep $ [pretty a, ppQNameInfix op, pretty b]
        pretty (IParam i t)    = myFsep $ [pretty i, text "::", pretty t]
        pretty (EqualP t1 t2)  = myFsep $ [pretty t1, text "~", pretty t2]

-- Pretty print a source location, useful for printing out error messages
instance Pretty SrcLoc where
  pretty srcLoc =
    return $ P.hsep [ colonFollow (P.text $ srcFilename srcLoc)
                    , colonFollow (P.int  $ srcLine     srcLoc)
                    , P.int $ srcColumn srcLoc
                    ]

colonFollow p = P.hcat [ p, P.colon ]


instance Pretty SrcSpan where
    pretty srcSpan =
        return $ P.hsep [ colonFollow (P.text $ srcSpanFilename srcSpan)
                        , P.hcat [ P.text "("
                                 , P.int $ srcSpanStartLine srcSpan
                                 , P.colon
                                 , P.int $ srcSpanStartColumn srcSpan
                                 , P.text ")"
                                 ]
                        , P.text "-"
                        , P.hcat [ P.text "("
                                 , P.int $ srcSpanEndLine srcSpan
                                 , P.colon
                                 , P.int $ srcSpanEndColumn srcSpan
                                 , P.text ")"
                                 ]
                        ]

---------------------------------------------------------------------
-- Annotated version

-------------------------  Pretty-Print a Module --------------------
instance SrcInfo pos => Pretty (A.Module pos) where
        pretty (A.Module pos mbHead os imp decls) =
                markLine pos $
                myVcat $ map pretty os ++
                    (case mbHead of
                        Nothing -> id
                        Just h  -> \x -> [topLevel (pretty h) x])
                    (map pretty imp ++ map pretty decls)
        pretty (A.XmlPage pos _mn os n attrs mattr cs) =
                markLine pos $
                myVcat $ map pretty os ++
                    [let ax = maybe [] (return . pretty) mattr
                      in hcat $
                         (myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [char '>']):
                            map pretty cs ++ [myFsep $ [text "</" <> pretty n, char '>']]]
        pretty (A.XmlHybrid pos mbHead os imp decls n attrs mattr cs) =
                markLine pos $
                myVcat $ map pretty os ++ [text "<%"] ++
                    (case mbHead of
                        Nothing -> id
                        Just h  -> \x -> [topLevel (pretty h) x])
                    (map pretty imp ++ map pretty decls ++
                        [let ax = maybe [] (return . pretty) mattr
                          in hcat $
                             (myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [char '>']):
                                map pretty cs ++ [myFsep $ [text "</" <> pretty n, char '>']]])



--------------------------  Module Header ------------------------------
instance Pretty (A.ModuleHead l) where
    pretty (A.ModuleHead _ m mbWarn mbExportList) = mySep [
        text "module",
        pretty m,
        maybePP pretty mbWarn,
        maybePP pretty mbExportList,
        text "where"]

instance Pretty (A.WarningText l) where
    pretty = ppWarnTxt. sWarningText

instance Pretty (A.ModuleName l) where
        pretty = pretty . sModuleName

instance Pretty (A.ExportSpecList l) where
        pretty (A.ExportSpecList _ especs)  = parenList $ map pretty especs

instance Pretty (A.ExportSpec l) where
        pretty = pretty . sExportSpec

instance SrcInfo pos => Pretty (A.ImportDecl pos) where
        pretty = pretty . sImportDecl

instance Pretty (A.ImportSpecList l) where
        pretty (A.ImportSpecList _ b ispecs)  =
            (if b then text "hiding" else empty)
                <+> parenList (map pretty ispecs)

instance Pretty (A.ImportSpec l) where
        pretty = pretty . sImportSpec

-------------------------  Declarations ------------------------------
instance SrcInfo pos => Pretty (A.Decl pos) where
        pretty = pretty . sDecl

instance Pretty (A.DeclHead l) where
    pretty (A.DHead l n tvs)       = mySep (pretty n : map pretty tvs)
    pretty (A.DHInfix l tva n tvb) = mySep [pretty tva, pretty n, pretty tvb]
    pretty (A.DHParen l dh)        = parens (pretty dh)

instance Pretty (A.InstHead l) where
    pretty (A.IHead l qn ts)       = mySep (pretty qn : map pretty ts)
    pretty (A.IHInfix l ta qn tb)  = mySep [pretty ta, pretty qn, pretty tb]
    pretty (A.IHParen l ih)        = parens (pretty ih)

instance Pretty (A.DataOrNew l) where
        pretty = pretty . sDataOrNew

instance Pretty (A.Assoc l) where
        pretty = pretty . sAssoc

instance SrcInfo pos => Pretty (A.Match pos) where
        pretty = pretty . sMatch

instance SrcInfo loc => Pretty (A.ClassDecl loc) where
        pretty = pretty . sClassDecl

instance SrcInfo loc => Pretty (A.InstDecl loc) where
        pretty = pretty . sInstDecl

------------------------- FFI stuff -------------------------------------
instance Pretty (A.Safety l) where
        pretty = pretty . sSafety

instance Pretty (A.CallConv l) where
        pretty = pretty . sCallConv

------------------------- Pragmas ---------------------------------------
instance SrcInfo loc => Pretty (A.Rule loc) where
        pretty = pretty . sRule

instance Pretty (A.Activation l) where
    pretty = pretty . sActivation

instance Pretty (A.RuleVar l) where
    pretty = pretty . sRuleVar

instance SrcInfo loc => Pretty (A.ModulePragma loc) where
    pretty (A.LanguagePragma _ ns) =
        myFsep $ text "{-# LANGUAGE" : punctuate (char ',') (map pretty ns) ++ [text "#-}"]
    pretty (A.OptionsPragma _ (Just tool) s) =
        myFsep $ [text "{-# OPTIONS_" <> pretty tool, text s, text "#-}"]
    pretty (A.OptionsPragma _ _ s) =
        myFsep $ [text "{-# OPTIONS", text s, text "#-}"]
    pretty (A.AnnModulePragma _ ann) =
        myFsep $ [text "{-# ANN", pretty ann, text "#-}"]

instance SrcInfo loc => Pretty (A.Annotation loc) where
    pretty = pretty . sAnnotation

------------------------- Data & Newtype Bodies -------------------------
instance Pretty (A.QualConDecl l) where
        pretty (A.QualConDecl _pos mtvs ctxt con) =
                myFsep [ppForall (fmap (map sTyVarBind) mtvs), ppContext $ maybe [] sContext ctxt, pretty con]

instance Pretty (A.GadtDecl l) where
        pretty (A.GadtDecl _pos name ty) =
                myFsep [pretty name, text "::", pretty ty]

instance Pretty (A.ConDecl l) where
        pretty = pretty . sConDecl

instance Pretty (A.FieldDecl l) where
        pretty (A.FieldDecl _ names ty) =
                myFsepSimple $ (punctuate comma . map pretty $ names) ++
                       [text "::", pretty ty]


instance Pretty (A.BangType l) where
        pretty = pretty . sBangType

instance Pretty (A.Deriving l) where
        pretty (A.Deriving _ []) = text "deriving" <+> parenList []
        pretty (A.Deriving _ [A.IHead _ d []]) = text "deriving" <+> pretty d
        pretty (A.Deriving _ ihs) = text "deriving" <+> parenList (map pretty ihs)

------------------------- Types -------------------------
instance Pretty (A.Type l) where
        pretty = pretty . sType

instance Pretty (A.TyVarBind l) where
        pretty = pretty . sTyVarBind

---------------------------- Kinds ----------------------------

instance Pretty (A.Kind l) where
        pretty = pretty . sKind

------------------- Functional Dependencies -------------------
instance Pretty (A.FunDep l) where
        pretty = pretty . sFunDep

------------------------- Expressions -------------------------
instance SrcInfo loc => Pretty (A.Rhs loc) where
        pretty = pretty . sRhs

instance SrcInfo loc => Pretty (A.GuardedRhs loc) where
        pretty = pretty . sGuardedRhs

instance Pretty (A.Literal l) where
        pretty = pretty . sLiteral

instance SrcInfo loc => Pretty (A.Exp loc) where
        pretty = pretty . sExp

instance SrcInfo loc => Pretty (A.XAttr loc) where
        pretty = pretty . sXAttr

instance Pretty (A.XName l) where
        pretty = pretty . sXName

--------------------- Template Haskell -------------------------

instance SrcInfo loc => Pretty (A.Bracket loc) where
        pretty = pretty . sBracket

instance SrcInfo loc => Pretty (A.Splice loc) where
        pretty = pretty . sSplice

------------------------- Patterns -----------------------------

instance SrcInfo loc => Pretty (A.Pat loc) where
        pretty = pretty . sPat

instance SrcInfo loc => Pretty (A.PXAttr loc) where
        pretty = pretty . sPXAttr

instance SrcInfo loc => Pretty (A.PatField loc) where
        pretty = pretty . sPatField

--------------------- Regular Patterns -------------------------

instance SrcInfo loc => Pretty (A.RPat loc) where
        pretty = pretty . sRPat

instance Pretty (A.RPatOp l) where
        pretty = pretty . sRPatOp

------------------------- Case bodies  -------------------------
instance SrcInfo loc => Pretty (A.Alt loc) where
        pretty = pretty . sAlt

instance SrcInfo loc => Pretty (A.GuardedAlts loc) where
        pretty = pretty . sGuardedAlts

instance SrcInfo loc => Pretty (A.GuardedAlt loc) where
        pretty = pretty . sGuardedAlt

------------------------- Statements in monads, guards & list comprehensions -----
instance SrcInfo loc => Pretty (A.Stmt loc) where
        pretty = pretty . sStmt

instance SrcInfo loc => Pretty (A.QualStmt loc) where
        pretty = pretty . sQualStmt

------------------------- Record updates
instance SrcInfo loc => Pretty (A.FieldUpdate loc) where
        pretty = pretty . sFieldUpdate

------------------------- Names -------------------------
instance Pretty (A.QOp l) where
        pretty = pretty . sQOp

instance Pretty (A.QName l) where
        pretty = pretty . sQName

instance Pretty (A.Op l) where
        pretty = pretty . sOp

instance Pretty (A.Name l) where
        pretty = pretty . sName

instance Pretty (A.IPName l) where
        pretty = pretty . sIPName

instance SrcInfo loc => Pretty (A.IPBind loc) where
        pretty = pretty . sIPBind

instance Pretty (A.CName l) where
        pretty = pretty . sCName

instance Pretty (A.Context l) where
        pretty (A.CxEmpty _) = mySep [text "()", text "=>"]
        pretty (A.CxSingle _ asst) = mySep [pretty asst, text "=>"]
        pretty (A.CxTuple _ assts) = myFsep $ [parenList (map pretty assts), text "=>"]
        pretty (A.CxParen _ asst)  = parens (pretty asst)

-- hacked for multi-parameter type classes
instance Pretty (A.Asst l) where
        pretty = pretty . sAsst

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

markLine :: SrcInfo s => s -> Doc -> Doc
markLine loc doc = do
        e <- getPPEnv
        let y = startLine loc
        let line l =
              text ("{-# LINE " ++ show l ++ " \"" ++ fileName loc ++ "\" #-}")
        if linePragmas e then layoutChoice (line y $$) (line (y+1) <+>) doc
              else doc

--------------------------------------------------------------------------------
-- Pretty-printing of internal constructs, for error messages while parsing

instance SrcInfo loc => Pretty (P.PExp loc) where
        pretty (P.Lit _ l) = pretty l
        pretty (P.InfixApp _ a op b) = myFsep [pretty a, pretty op, pretty b]
        pretty (P.NegApp _ e) = myFsep [char '-', pretty e]
        pretty (P.App _ a b) = myFsep [pretty a, pretty b]
        pretty (P.Lambda _loc expList ppBody) = myFsep $
                char '\\' : map pretty expList ++ [text "->", pretty ppBody]
        pretty (P.Let _ (A.BDecls _ declList) letBody) =
                ppLetExp declList letBody
        pretty (P.Let _ (A.IPBinds _ bindList) letBody) =
                ppLetExp bindList letBody
        pretty (P.If _ cond thenexp elsexp) =
                myFsep [text "if", pretty cond,
                        text "then", pretty thenexp,
                        text "else", pretty elsexp]
        pretty (P.Case _ cond altList) =
                myFsep [text "case", pretty cond, text "of"]
                $$$ ppBody caseIndent (map pretty altList)
        pretty (P.Do _ stmtList) =
                text "do" $$$ ppBody doIndent (map pretty stmtList)
        pretty (P.MDo _ stmtList) =
                text "mdo" $$$ ppBody doIndent (map pretty stmtList)
        pretty (P.Var _ name) = pretty name
        pretty (P.IPVar _ ipname) = pretty ipname
        pretty (P.Con _ name) = pretty name
        pretty (P.TupleSection _ bxd mExpList) =
                let ds = map (maybePP pretty) mExpList
                in case bxd of
                       Boxed   -> parenList ds
                       Unboxed -> hashParenList ds
        pretty (P.Paren _ e) = parens . pretty $ e
        pretty (P.RecConstr _ c fieldList) =
                pretty c <> (braceList . map pretty $ fieldList)
        pretty (P.RecUpdate _ e fieldList) =
                pretty e <> (braceList . map pretty $ fieldList)
        pretty (P.List _ list) =
                bracketList . punctuate comma . map pretty $ list
        pretty (P.EnumFrom _ e) =
                bracketList [pretty e, text ".."]
        pretty (P.EnumFromTo _ from to) =
                bracketList [pretty from, text "..", pretty to]
        pretty (P.EnumFromThen _ from thenE) =
                bracketList [pretty from <> comma, pretty thenE, text ".."]
        pretty (P.EnumFromThenTo _ from thenE to) =
                bracketList [pretty from <> comma, pretty thenE,
                             text "..", pretty to]
        pretty (P.ParComp _ e qualLists) =
                bracketList (intersperse (char '|') $
                                pretty e : (punctuate comma . concatMap (map pretty) $ qualLists))
        pretty (P.ExpTypeSig _pos e ty) =
                myFsep [pretty e, text "::", pretty ty]
        pretty (P.BracketExp _ b) = pretty b
        pretty (P.SpliceExp _ s) = pretty s
        pretty (P.TypQuote _ t)  = text "\'\'" <> pretty t
        pretty (P.VarQuote _ x)  = text "\'" <> pretty x
        pretty (P.QuasiQuote _ n qt) = text ("[$" ++ n ++ "|" ++ qt ++ "|]")
        pretty (P.XTag _ n attrs mattr cs) =
                let ax = maybe [] (return . pretty) mattr
                 in hcat $
                     (myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [char '>']):
                        map pretty cs ++ [myFsep $ [text "</" <> pretty n, char '>']]
        pretty (P.XETag _ n attrs mattr) =
                let ax = maybe [] (return . pretty) mattr
                 in myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [text "/>"]
        pretty (P.XPcdata _ s) = text s
        pretty (P.XExpTag _ e) =
                myFsep $ [text "<%", pretty e, text "%>"]
        pretty (P.XChildTag _ es) =
                myFsep $ text "<%>" : map pretty es ++ [text "</%>"]
        pretty (P.CorePragma _ s e) = myFsep $ map text ["{-# CORE", show s, "#-}"] ++ [pretty e]
        pretty (P.SCCPragma  _ s e) = myFsep $ map text ["{-# SCC",  show s, "#-}"] ++ [pretty e]
        pretty (P.GenPragma  _ s (a,b) (c,d) e) =
                myFsep $ [text "{-# GENERATED", text $ show s,
                            int a, char ':', int b, char '-',
                            int c, char ':', int d, text "#-}", pretty e]
        pretty (P.Proc _ p e) = myFsep $ [text "proc", pretty p, text "->", pretty e]
        pretty (P.LeftArrApp _ l r)      = myFsep $ [pretty l, text "-<",  pretty r]
        pretty (P.RightArrApp _ l r)     = myFsep $ [pretty l, text ">-",  pretty r]
        pretty (P.LeftArrHighApp _ l r)  = myFsep $ [pretty l, text "-<<", pretty r]
        pretty (P.RightArrHighApp _ l r) = myFsep $ [pretty l, text ">>-", pretty r]
        pretty (P.AsPat _ name (P.IrrPat _ pat)) =
                myFsep [pretty name <> char '@', char '~' <> pretty pat]
        pretty (P.AsPat _ name pat) =
                hcat [pretty name, char '@', pretty pat]
        pretty (P.WildCard _) = char '_'
        pretty (P.IrrPat _ pat) = char '~' <> pretty pat
        pretty (P.PostOp _ e op) = pretty e <+> pretty op
        pretty (P.PreOp _ op e)  = pretty op <+> pretty e
        pretty (P.ViewPat _ e p) =
                myFsep [pretty e, text "->", pretty p]
        pretty (P.SeqRP _ rs) = myFsep $ text "(/" : map pretty rs ++ [text "/)"]
        pretty (P.GuardRP _ r gs) =
                myFsep $ text "(|" : pretty r : char '|' : map pretty gs ++ [text "|)"]
        pretty (P.EitherRP _ r1 r2) = parens . myFsep $ [pretty r1, char '|', pretty r2]
        pretty (P.CAsRP _ n (P.IrrPat _ e)) =
                myFsep [pretty n <> text "@:", char '~' <> pretty e]
        pretty (P.CAsRP _ n r) = hcat [pretty n, text "@:", pretty r]
        pretty (P.XRPats _ ps) =
                myFsep $ text "<[" : map pretty ps ++ [text "%>"]
        pretty (P.ExplTypeArg _ qn t) =
                myFsep [pretty qn, text "{|", pretty t, text "|}"]
        pretty (P.BangPat _ e) = text "!" <> pretty e

instance SrcInfo loc => Pretty (P.PFieldUpdate loc) where
        pretty (P.FieldUpdate _ name e) =
                myFsep [pretty name, equals, pretty e]
        pretty (P.FieldPun _ name) = pretty name
        pretty (P.FieldWildcard _) = text ".."

instance SrcInfo loc => Pretty (P.ParseXAttr loc) where
        pretty (P.XAttr _ n v) =
                myFsep [pretty n, char '=', pretty v]

instance SrcInfo loc => Pretty (P.PContext loc) where
        pretty (P.CxEmpty _) = mySep [text "()", text "=>"]
        pretty (P.CxSingle _ asst) = mySep [pretty asst, text "=>"]
        pretty (P.CxTuple _ assts) = myFsep $ [parenList (map pretty assts), text "=>"]
        pretty (P.CxParen _ asst)  = parens (pretty asst)

instance SrcInfo loc => Pretty (P.PAsst loc) where
        pretty (P.ClassA _ a ts)   = myFsep $ ppQName (sQName a) : map (prettyPrec prec_atype) ts
        pretty (P.InfixA _ a op b) = myFsep $ [pretty a, ppQNameInfix (sQName op), pretty b]
        pretty (P.IParam _ i t)    = myFsep $ [pretty i, text "::", pretty t]
        pretty (P.EqualP _ t1 t2)  = myFsep $ [pretty t1, text "~", pretty t2]

instance SrcInfo loc => Pretty (P.PType loc) where
        prettyPrec p (P.TyForall _ mtvs ctxt htype) = parensIf (p > 0) $
                myFsep [ppForall (fmap (map sTyVarBind) mtvs), maybePP pretty ctxt, pretty htype]
        prettyPrec p (P.TyFun _ a b) = parensIf (p > 0) $
                myFsep [prettyPrec prec_btype a, text "->", pretty b]
        prettyPrec _ (P.TyTuple _ bxd l) =
                let ds = map pretty l
                 in case bxd of
                        Boxed   -> parenList ds
                        Unboxed -> hashParenList ds
        prettyPrec _ (P.TyList _ t)  = brackets $ pretty t
        prettyPrec p (P.TyApp _ a b) =
                {-
                | a == list_tycon = brackets $ pretty b         -- special case
                | otherwise = -} parensIf (p > prec_btype) $
                                    myFsep [pretty a, prettyPrec prec_atype b]
        prettyPrec _ (P.TyVar _ name) = pretty name
        prettyPrec _ (P.TyCon _ name) = pretty name
        prettyPrec _ (P.TyParen _ t) = parens (pretty t)
        prettyPrec _ (P.TyPred _ asst) = pretty asst
        prettyPrec _ (P.TyInfix _ a op b) = myFsep [pretty a, ppQNameInfix (sQName op), pretty b]
        prettyPrec _ (P.TyKind _ t k) = parens (myFsep [pretty t, text "::", pretty k])

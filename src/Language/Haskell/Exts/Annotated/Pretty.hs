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

module Language.Haskell.Exts.Annotated.Pretty (
                -- * Pretty printing
                Pretty,
                prettyPrintStyleMode, prettyPrintWithMode, prettyPrint,
                -- * Pretty-printing styles (from "Text.PrettyPrint.HughesPJ")
                P.Style(..), P.style, P.Mode(..),
                -- * Haskell formatting modes
                PPHsMode(..), Indent, PPLayout(..), defaultMode) where

import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Annotated.SrcLoc

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
-- in "Language.Haskell.Exts.Syntax".
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
instance SrcInfo pos => Pretty (Module pos) where
        pretty (Module pos mbHead os imp decls) =
                markLine pos $
                myVcat $ map pretty os ++
                    (case mbHead of
                        Nothing -> id
                        Just h  -> \x -> [topLevel (pretty h) x])
                    (map pretty imp ++ map pretty decls)
        pretty (XmlPage pos _mn os n attrs mattr cs) =
                markLine pos $
                myVcat $ map pretty os ++
                    [let ax = maybe [] (return . pretty) mattr
                      in hcat $
                         (myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [char '>']):
                            map pretty cs ++ [myFsep $ [text "</" <> pretty n, char '>']]]
        pretty (XmlHybrid pos mbHead os imp decls n attrs mattr cs) =
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
instance Pretty (ModuleHead l) where
    pretty (ModuleHead _ m mbWarn mbExportList) = mySep [
        text "module",
        pretty m,
        maybePP pretty mbWarn,
        maybePP pretty mbExportList,
        text "where"]

instance Pretty (WarningText l) where
    pretty = ppWarnTxt

ppWarnTxt :: WarningText l -> Doc
ppWarnTxt (DeprText _ s) = mySep [text "{-# DEPRECATED", text s, text "#-}"]
ppWarnTxt (WarnText _ s) = mySep [text "{-# WARNING",    text s, text "#-}"]

instance Pretty (ModuleName l) where
        pretty (ModuleName _ modName) = text modName

instance Pretty (ExportSpecList l) where
        pretty (ExportSpecList _ especs)  = parenList $ map pretty especs

instance Pretty (ExportSpec l) where
        pretty (EVar _ name)                = pretty name
        pretty (EAbs _ name)                = pretty name
        pretty (EThingAll _ name)           = pretty name <> text "(..)"
        pretty (EThingWith _ name nameList) =
                pretty name <> (parenList . map pretty $ nameList)
        pretty (EModuleContents _ m)        = text "module" <+> pretty m

instance SrcInfo pos => Pretty (ImportDecl pos) where
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
                exports (ImportSpecList _ b specList) =
                        if b then text "hiding" <+> specs else specs
                    where specs = parenList . map pretty $ specList

instance Pretty (ImportSpec l) where
        pretty (IVar _ name)                = pretty name
        pretty (IAbs _ name)                = pretty name
        pretty (IThingAll _ name)           = pretty name <> text "(..)"
        pretty (IThingWith _ name nameList) =
                pretty name <> (parenList . map pretty $ nameList)

-------------------------  Declarations ------------------------------
instance SrcInfo pos => Pretty (Decl pos) where
        pretty (TypeDecl loc dhead htype) =
                blankline $
                markLine loc $
                mySep [text "type", pretty dhead, equals, pretty htype]

        pretty (DataDecl loc don context dhead constrList derives) =
                blankline $
                markLine loc $
                mySep [pretty don, ppContext context, pretty dhead]
                        <+> (myVcat (zipWith (<+>) (equals : repeat (char '|'))
                                                   (map pretty constrList))
                        $$$ ppDeriving derives)

        pretty (GDataDecl loc don context dhead optkind gadtList derives) =
                blankline $
                markLine loc $
                mySep ( [pretty don, ppContext context, pretty dhead]
                        ++ ppOptKind optkind ++ [text "where"])
                        $$$ ppBody classIndent (map pretty gadtList)
                        $$$ ppDeriving derives

        pretty (TypeFamDecl loc dhead optkind) =
                blankline $
                markLine loc $
                mySep ([text "type", text "family", pretty dhead]
                        ++ ppOptKind optkind)

        pretty (DataFamDecl loc context dhead optkind) =
                blankline $
                markLine loc $
                mySep ( [text "data", text "family", ppContext context, pretty dhead] ++ ppOptKind optkind)

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
        pretty (ClassDecl pos context dhead fundeps Nothing) =
                blankline $
                markLine pos $
                mySep [text "class", ppContext context, pretty dhead, ppFunDeps fundeps]
        pretty (ClassDecl pos context dhead fundeps (Just declList)) =
                blankline $
                markLine pos $
                mySep [text "class", ppContext context, pretty dhead, ppFunDeps fundeps, text "where"]
                $$$ ppBody classIndent (map pretty declList)

        -- m{spacing=False}
        -- special case for empty instance declaration
        pretty (InstDecl pos context ihead Nothing) =
                blankline $
                markLine pos $
                mySep [text "instance", ppContext context, pretty ihead]
        pretty (InstDecl pos context ihead (Just declList)) =
                blankline $
                markLine pos $
                mySep ( [text "instance", ppContext context, pretty ihead, text "where"])
                $$$ ppBody classIndent (map pretty declList)

        pretty (DerivDecl pos context ihead) =
                blankline $
                markLine pos $
                mySep [text "deriving", text "instance", ppContext context, pretty ihead]
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

        pretty (FunBind _ matches) = do
                e <- fmap layout getPPEnv
                case e of PPOffsideRule -> foldr ($$$) empty (map pretty matches)
                          _ -> foldr (\x y -> x <> semi <> y) empty (map pretty matches)

        pretty (PatBind pos pat optsig rhs whereBinds) =
                markLine pos $
                myFsep [pretty pat, maybePP ppSig optsig, pretty rhs] $$$ ppWhere whereBinds

        pretty (InfixDecl pos assoc prec opList) =
                blankline $
                markLine pos $
                mySep ([pretty assoc, maybePP int prec]
                       ++ (punctuate comma . map pretty $ opList))

        pretty (ForImp pos cconv msaf mstr name typ) =
                blankline $
                markLine pos $
                mySep [text "foreign import", pretty cconv, maybePP pretty msaf,
                       maybePP (text . show) mstr, pretty name, text "::", pretty typ]

        pretty (ForExp pos cconv mstr name typ) =
                blankline $
                markLine pos $
                mySep [text "foreign export", pretty cconv,
                       maybePP (text . show) mstr, pretty name, text "::", pretty typ]

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

        pretty (InlineSig pos inl mactiv name) =
                blankline $
                markLine pos $
                mySep [text (if inl then "{-# INLINE" else "{-# NOINLINE"), maybePP pretty mactiv, pretty name, text "#-}"]

        pretty (SpecSig pos name types) =
                blankline $
                markLine pos $
                mySep $ [text "{-# SPECIALISE", pretty name, text "::"]
                    ++ punctuate comma (map pretty types) ++ [text "#-}"]

        pretty (SpecInlineSig pos inl mactiv name types) =
                blankline $
                markLine pos $
                mySep $ [text "{-# SPECIALISE", text (if inl then "INLINE" else "NOINLINE"),
                        maybePP pretty mactiv, pretty name, text "::"]
                        ++ (punctuate comma $ map pretty types) ++ [text "#-}"]

        pretty (InstSig pos context ihead) =
                blankline $
                markLine pos $
                mySep $ [text "{-# SPECIALISE", text "instance", ppContext context, pretty ihead, text "#-}"]


instance Pretty (DeclHead l) where
    pretty (DHead l n tvs)       = mySep (pretty n : map pretty tvs)
    pretty (DHInfix l tva n tvb) = mySep [pretty tva, pretty n, pretty tvb]
    pretty (DHParen l dh)        = parens (pretty dh)

instance Pretty (InstHead l) where
    pretty (IHead l qn ts)       = mySep (pretty qn : map pretty ts)
    pretty (IHInfix l ta qn tb)  = mySep [pretty ta, pretty qn, pretty tb]
    pretty (IHParen l ih)        = parens (pretty ih)

instance Pretty (DataOrNew l) where
        pretty (DataType _) = text "data"
        pretty (NewType  _) = text "newtype"

instance Pretty (Assoc l) where
        pretty (AssocNone _)  = text "infix"
        pretty (AssocLeft _)  = text "infixl"
        pretty (AssocRight _) = text "infixr"

instance SrcInfo pos => Pretty (Match pos) where
        pretty (Match pos f ps rhs whereBinds) =
                markLine pos $
                myFsep (pretty f : map (prettyPrec 2) ps ++ [pretty rhs])
                $$$ ppWhere whereBinds
{-            where
                lhs = case ps of
                        l:r:ps' | isSymbolName f ->
                                let hd = [pretty l, ppName f, pretty r] in
                                if null ps' then hd
                                else parens (myFsep hd) : map (prettyPrec 2) ps'
                        _ -> pretty f : map (prettyPrec 2) ps -}
        pretty (InfixMatch pos a f b rhs whereBinds) =
                markLine pos $
                myFsep [pretty a, pretty f, pretty b, pretty rhs]
                $$$ ppWhere whereBinds

ppWhere :: SrcInfo loc => Maybe (Binds loc) -> Doc
ppWhere Nothing              = empty
ppWhere (Just (BDecls _ [])) = empty -- this case is really an anomaly
ppWhere (Just (BDecls _ l))  = nest 2 (text "where" $$$ ppBody whereIndent (map pretty l))
ppWhere (Just (IPBinds _ b)) = nest 2 (text "where" $$$ ppBody whereIndent (map pretty b))

ppSig :: (Type l) -> Doc
ppSig t = text "::" <+> pretty t

instance SrcInfo loc => Pretty (ClassDecl loc) where
    pretty (ClsDecl _ decl) = pretty decl

    pretty (ClsDataFam loc context dhead optkind) =
                markLine loc $
                mySep ( [text "data", ppContext context, pretty dhead] ++ ppOptKind optkind)

    pretty (ClsTyFam loc dhead optkind) =
                markLine loc $
                mySep ( [text "type", pretty dhead] ++ ppOptKind optkind)

    pretty (ClsTyDef loc ntype htype) =
                markLine loc $
                mySep [text "type", pretty ntype, equals, pretty htype]

instance SrcInfo loc => Pretty (InstDecl loc) where
        pretty (InsDecl _ decl) = pretty decl

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

        pretty (InsInline loc inl mactiv name) =
                markLine loc $
                mySep [text (if inl then "{-# INLINE" else "{-# NOINLINE"), maybePP pretty mactiv, pretty name, text "#-}"]


------------------------- FFI stuff -------------------------------------
instance Pretty (Safety l) where
        pretty (PlayRisky _)      = text "unsafe"
        pretty (PlaySafe _ b)     = text $ if b then "threadsafe" else "safe"

instance Pretty (CallConv l) where
        pretty (StdCall _) = text "stdcall"
        pretty (CCall   _) = text "ccall"

------------------------- Pragmas ---------------------------------------
ppWarnDepr :: ([Name l], String) -> Doc
ppWarnDepr (names, txt) = mySep $ (punctuate comma $ map pretty names) ++ [text $ show txt]

instance SrcInfo loc => Pretty (Rule loc) where
        pretty (Rule _ tag mactiv rvs rhs lhs) =
            mySep $ [text $ show tag, maybePP pretty mactiv,
                        maybePP ppRuleVars rvs,
                        pretty rhs, char '=', pretty lhs]

ppRuleVars :: [RuleVar l] -> Doc
ppRuleVars []  = empty
ppRuleVars rvs = mySep $ text "forall" : map pretty rvs ++ [char '.']

instance Pretty (Activation l) where
    pretty (ActiveFrom _ i)  = char '['  <> int i <> char ']'
    pretty (ActiveUntil _ i) = text "[~" <> int i <> char ']'

instance Pretty (RuleVar l) where
    pretty (RuleVar _ n) = pretty n
    pretty (TypedRuleVar _ n t) = mySep [pretty n, text "::", pretty t]

instance Pretty (OptionPragma l) where
    pretty (LanguagePragma _ ns) =
        myFsep $ text "{-# LANGUAGE" : punctuate (char ',') (map pretty ns) ++ [text "#-}"]
    pretty (IncludePragma _ s) =
        myFsep $ [text "{-# INCLUDE", text s, text "#-}"]
    pretty (CFilesPragma _ s) =
        myFsep $ [text "{-# CFILES", text s, text "#-}"]
    pretty (OptionsPragma _ (Just tool) s) =
        myFsep $ [text "{-# OPTIONS_" <> pretty tool, text s, text "#-}"]
    pretty (OptionsPragma _ _ s) =
        myFsep $ [text "{-# OPTIONS", text s, text "#-}"]
{-    pretty (UnknownTopPragma _ n s) =
        myFsep $ map text ["{-#", n, s, "#-}"] -}

instance Pretty Tool where
    pretty (UnknownTool s) = text s
    pretty t               = text $ show t

------------------------- Data & Newtype Bodies -------------------------
instance Pretty (QualConDecl l) where
        pretty (QualConDecl _pos mtvs ctxt con) =
                myFsep [ppForall mtvs, ppContext ctxt, pretty con]

instance Pretty (GadtDecl l) where
        pretty (GadtDecl _pos name ty) =
                myFsep [pretty name, text "::", pretty ty]

instance Pretty (ConDecl l) where
        pretty (RecDecl _ name fieldList) =
                pretty name <> (braceList . map pretty $ fieldList)

{-        pretty (ConDecl name@(Symbol _) [l, r]) =
                myFsep [prettyPrec prec_btype l, ppName name,
                        prettyPrec prec_btype r] -}
        pretty (ConDecl _ name typeList) =
                mySep $ ppName name : map (prettyPrec prec_atype) typeList
        pretty (InfixConDecl _ l name r) =
                myFsep [prettyPrec prec_btype l, ppNameInfix name,
                         prettyPrec prec_btype r]

instance Pretty (FieldDecl l) where
        pretty (FieldDecl _ names ty) =
                myFsepSimple $ (punctuate comma . map pretty $ names) ++
                       [text "::", pretty ty]

{-
ppField :: ([Name l],BangType l) -> Doc
ppField (names, ty) =
        myFsepSimple $ (punctuate comma . map pretty $ names) ++
                       [text "::", pretty ty]
-}

instance Pretty (BangType l) where
        prettyPrec _ (BangedTy _ ty)   = char '!' <> ppAType ty
        prettyPrec p (UnBangedTy _ ty) = prettyPrec p ty
        prettyPrec p (UnpackedTy _ ty) = text "{-# UNPACK #-}" <+> char '!' <> prettyPrec p ty

ppDeriving :: Maybe (Deriving l) -> Doc
ppDeriving = maybePP pretty

instance Pretty (Deriving l) where
        pretty (Deriving _ []) = text "deriving" <+> parenList []
        pretty (Deriving _ [IHead _ d []]) = text "deriving" <+> ppQName d
        pretty (Deriving _ ihs) = text "deriving" <+> parenList (map pretty ihs)

------------------------- Types -------------------------
ppBType :: Type l -> Doc
ppBType = prettyPrec prec_btype

ppAType :: Type l -> Doc
ppAType = prettyPrec prec_atype

-- precedences for types
prec_btype, prec_atype :: Int
prec_btype = 1  -- left argument of ->,
                -- or either argument of an infix data constructor
prec_atype = 2  -- argument of type or data constructor, or of a class

instance Pretty (Type l) where
        prettyPrec p (TyForall _ mtvs ctxt htype) = parensIf (p > 0) $
                myFsep [ppForall mtvs, ppContext ctxt, pretty htype]
        prettyPrec p (TyFun _ a b) = parensIf (p > 0) $
                myFsep [ppBType a, text "->", pretty b]
        prettyPrec _ (TyTuple _ bxd l) =
                let ds = map pretty l
                 in case bxd of
                        Boxed   -> parenList ds
                        Unboxed -> hashParenList ds
        prettyPrec _ (TyList _ t)  = brackets $ pretty t
        prettyPrec p (TyApp _ a b) =
                {-
                | a == list_tycon = brackets $ pretty b         -- special case
                | otherwise = -} parensIf (p > prec_btype) $
                                    myFsep [pretty a, ppAType b]
        prettyPrec _ (TyVar _ name) = pretty name
        prettyPrec _ (TyCon _ name) = pretty name
        prettyPrec _ (TyParen _ t) = parens (pretty t)
--        prettyPrec _ (TyPred asst) = pretty asst
        prettyPrec _ (TyInfix _ a op b) = myFsep [pretty a, ppQNameInfix op, pretty b]
        prettyPrec _ (TyKind _ t k) = parens (myFsep [pretty t, text "::", pretty k])


instance Pretty (TyVarBind l) where
        pretty (KindedVar _ var kind) = parens $ myFsep [pretty var, text "::", pretty kind]
        pretty (UnkindedVar _ var)    = pretty var

ppForall :: Maybe [TyVarBind l] -> Doc
ppForall Nothing   = empty
ppForall (Just []) = empty
ppForall (Just vs) =    myFsep (text "forall" : map pretty vs ++ [char '.'])

---------------------------- Kinds ----------------------------

instance Pretty (Kind l) where
        pretty (KindStar _)    = text "*"
        pretty (KindBang _)    = text "!"
        pretty (KindFn _ a b)  = myFsep [pretty a, text "->", pretty b]
        pretty (KindParen _ k) = myFsep [text "(", pretty k, text ")"]

ppOptKind :: Maybe (Kind l) -> [Doc]
ppOptKind Nothing  = []
ppOptKind (Just k) = [text "::", pretty k]

------------------- Functional Dependencies -------------------
instance Pretty (FunDep l) where
        pretty (FunDep _ from to) =
                myFsep $ map pretty from ++ [text "->"] ++ map pretty to


ppFunDeps :: [FunDep l] -> Doc
ppFunDeps []  = empty
ppFunDeps fds = myFsep $ (char '|':) . punctuate comma . map pretty $ fds

------------------------- Expressions -------------------------
instance SrcInfo loc => Pretty (Rhs loc) where
        pretty (UnGuardedRhs _ e) = equals <+> pretty e
        pretty (GuardedRhss _ guardList) = myVcat . map pretty $ guardList

instance SrcInfo loc => Pretty (GuardedRhs loc) where
        pretty (GuardedRhs _pos guards ppBody) =
                myFsep $ [char '|'] ++ (punctuate comma . map pretty $ guards) ++ [equals, pretty ppBody]

instance Pretty (Literal l) where
        pretty (Int _ i _)        = integer i
        pretty (Char _ c _)       = text (show c)
        pretty (String _ s _)     = text (show s)
        pretty (Frac _ r _)       = double (fromRational r)
        -- GHC unboxed literals:
        pretty (PrimChar _ c _)   = text (show c)           <> char '#'
        pretty (PrimString _ s _) = text (show s)           <> char '#'
        pretty (PrimInt _ i _)    = integer i               <> char '#'
        pretty (PrimWord _ w _)   = integer w               <> text "##"
        pretty (PrimFloat _ r _)  = float  (fromRational r) <> char '#'
        pretty (PrimDouble _ r _) = double (fromRational r) <> text "##"

instance SrcInfo loc => Pretty (Exp loc) where
        pretty (Lit _ l) = pretty l
        -- lambda stuff
        pretty (InfixApp _ a op b) = myFsep [pretty a, pretty op, pretty b]
        pretty (NegApp _ e) = myFsep [char '-', pretty e]
        pretty (App _ a b) = myFsep [pretty a, pretty b]
        pretty (Lambda _loc expList ppBody) = myFsep $
                char '\\' : map pretty expList ++ [text "->", pretty ppBody]
        -- keywords
        -- two cases for lets
        pretty (Let _ (BDecls _ declList) letBody) =
                ppLetExp declList letBody
        pretty (Let _ (IPBinds _ bindList) letBody) =
                ppLetExp bindList letBody

        pretty (If _ cond thenexp elsexp) =
                myFsep [text "if", pretty cond,
                        text "then", pretty thenexp,
                        text "else", pretty elsexp]
        pretty (Case _ cond altList) =
                myFsep [text "case", pretty cond, text "of"]
                $$$ ppBody caseIndent (map pretty altList)
        pretty (Do _ stmtList) =
                text "do" $$$ ppBody doIndent (map pretty stmtList)
        pretty (MDo _ stmtList) =
                text "mdo" $$$ ppBody doIndent (map pretty stmtList)
        -- Constructors & Vars
        pretty (Var _ name) = pretty name
        pretty (IPVar _ ipname) = pretty ipname
        pretty (Con _ name) = pretty name
        pretty (Tuple _ expList) = parenList . map pretty $ expList
        pretty (TupleSection _ mExpList) = parenList . map (maybePP pretty) $ mExpList
        -- weird stuff
        pretty (Paren _ e) = parens . pretty $ e
        pretty (LeftSection _ e op) = parens (pretty e <+> pretty op)
        pretty (RightSection _ op e) = parens (pretty op <+> pretty e)
        pretty (RecConstr _ c fieldList) =
                pretty c <> (braceList . map pretty $ fieldList)
        pretty (RecUpdate _ e fieldList) =
                pretty e <> (braceList . map pretty $ fieldList)
        -- Lists
        pretty (List _ list) =
                bracketList . punctuate comma . map pretty $ list
        pretty (EnumFrom _ e) =
                bracketList [pretty e, text ".."]
        pretty (EnumFromTo _ from to) =
                bracketList [pretty from, text "..", pretty to]
        pretty (EnumFromThen _ from thenE) =
                bracketList [pretty from <> comma, pretty thenE, text ".."]
        pretty (EnumFromThenTo _ from thenE to) =
                bracketList [pretty from <> comma, pretty thenE,
                             text "..", pretty to]
        pretty (ListComp _ e qualList) =
                bracketList ([pretty e, char '|']
                             ++ (punctuate comma . map pretty $ qualList))
        pretty (ParComp _ e qualLists) =
                bracketList (intersperse (char '|') $
                                pretty e : (punctuate comma . concatMap (map pretty) $ qualLists))
        pretty (ExpTypeSig _pos e ty) =
                myFsep [pretty e, text "::", pretty ty]
        -- Template Haskell
        pretty (BracketExp _ b)    = pretty b
        pretty (SpliceExp _ s)     = pretty s
        pretty (TypQuote _ t)      = text "\'\'" <> pretty t
        pretty (VarQuote _ x)      = text "\'" <> pretty x
        pretty (QuasiQuote _ n qt) = text ("[$" ++ n ++ "|" ++ qt ++ "|]")
        -- Hsx
        pretty (XTag _ n attrs mattr cs) =
                let ax = maybe [] (return . pretty) mattr
                 in hcat $
                     (myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [char '>']):
                        map pretty cs ++ [myFsep $ [text "</" <> pretty n, char '>']]
        pretty (XETag _ n attrs mattr) =
                let ax = maybe [] (return . pretty) mattr
                 in myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [text "/>"]
        pretty (XPcdata _ s) = text s
        pretty (XExpTag _ e) =
                myFsep $ [text "<%", pretty e, text "%>"]
        -- Pragmas
        pretty (CorePragma _ s e) = myFsep $ map text ["{-# CORE", show s, "#-}"] ++ [pretty e]
        pretty (SCCPragma  _ s e) = myFsep $ map text ["{-# SCC",  show s, "#-}"] ++ [pretty e]
        pretty (GenPragma  _ s (a,b) (c,d) e) =
                myFsep $ [text "{-# GENERATED", text $ show s,
                            int a, char ':', int b, char '-',
                            int c, char ':', int d, text "#-}", pretty e]
        -- Arrows
        pretty (Proc _ p e) = myFsep $ [text "proc", pretty p, text "->", pretty e]
        pretty (LeftArrApp _ l r)      = myFsep $ [pretty l, text "-<",  pretty r]
        pretty (RightArrApp _ l r)     = myFsep $ [pretty l, text ">-",  pretty r]
        pretty (LeftArrHighApp _ l r)  = myFsep $ [pretty l, text "-<<", pretty r]
        pretty (RightArrHighApp _ l r) = myFsep $ [pretty l, text ">>-", pretty r]


instance SrcInfo loc => Pretty (XAttr loc) where
        pretty (XAttr _ n v) =
                myFsep [pretty n, char '=', pretty v]

instance Pretty (XName l) where
        pretty (XName _ n) = text n
        pretty (XDomName _ d n) = text d <> char ':' <> text n

--ppLetExp :: [Decl] -> Exp -> Doc
ppLetExp l b = myFsep [text "let" <+> ppBody letIndent (map pretty l),
                        text "in", pretty b]

ppWith binds = nest 2 (text "with" $$$ ppBody withIndent (map pretty binds))
withIndent = whereIndent

--------------------- Template Haskell -------------------------

instance SrcInfo loc => Pretty (Bracket loc) where
        pretty (ExpBracket _ e) = ppBracket "[|" e
        pretty (PatBracket _ p) = ppBracket "[p|" p
        pretty (TypeBracket _ t) = ppBracket "[t|" t
        pretty (DeclBracket _ d) =
                myFsep $ text "[d|" : map pretty d ++ [text "|]"]

ppBracket o x = myFsep [text o, pretty x, text "|]"]

instance SrcInfo loc => Pretty (Splice loc) where
        pretty (IdSplice _ s) = char '$' <> text s
        pretty (ParenSplice _ e) =
                myFsep [text "$(", pretty e, char ')']

------------------------- Patterns -----------------------------

instance SrcInfo loc => Pretty (Pat loc) where
        prettyPrec _ (PVar _ name) = pretty name
        prettyPrec _ (PLit _ lit) = pretty lit
        prettyPrec _ (PNeg _ p) = myFsep [char '-', pretty p]
        prettyPrec p (PInfixApp _ a op b) = parensIf (p > 0) $
                myFsep [pretty a, pretty (QConOp (ann op) op), pretty b]
        prettyPrec p (PApp _ n ps) = parensIf (p > 1) $
                myFsep (pretty n : map pretty ps)
        prettyPrec _ (PTuple _ ps) = parenList . map pretty $ ps
        prettyPrec _ (PList _ ps) =
                bracketList . punctuate comma . map pretty $ ps
        prettyPrec _ (PParen _ p) = parens . pretty $ p
        prettyPrec _ (PRec _ c fields) =
                pretty c <> (braceList . map pretty $ fields)
        -- special case that would otherwise be buggy
        prettyPrec _ (PAsPat _ name (PIrrPat _ pat)) =
                myFsep [pretty name <> char '@', char '~' <> pretty pat]
        prettyPrec _ (PAsPat _ name pat) =
                hcat [pretty name, char '@', pretty pat]
        prettyPrec _ (PWildCard _) = char '_'
        prettyPrec _ (PIrrPat _ pat) = char '~' <> pretty pat
        prettyPrec _ (PatTypeSig _pos pat ty) =
                myFsep [pretty pat, text "::", pretty ty]
        prettyPrec _ (PViewPat _ e p) =
                myFsep [pretty e, text "->", pretty p]
        prettyPrec _ (PNPlusK _ n k) =
                myFsep [pretty n, text "+", text $ show k]
        -- HaRP
        prettyPrec _ (PRPat _ rs) =
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
        prettyPrec _ (PXPcdata _ s) = text s
        prettyPrec _ (PXPatTag _ p) =
                myFsep $ [text "<%", pretty p, text "%>"]
        prettyPrec _ (PXRPats _ ps) =
                myFsep $ text "<[" : map pretty ps ++ [text "%>"]
        -- Generics
        prettyPrec _ (PExplTypeArg _ qn t) =
                myFsep [pretty qn, text "{|", pretty t, text "|}"]
        -- BangPatterns
        prettyPrec _ (PBangPat _ p) = text "!" <> pretty p

instance SrcInfo loc => Pretty (PXAttr loc) where
        pretty (PXAttr _ n p) =
                myFsep [pretty n, char '=', pretty p]

instance SrcInfo loc => Pretty (PatField loc) where
        pretty (PFieldPat _ name pat) =
                myFsep [pretty name, equals, pretty pat]
        pretty (PFieldPun _ name) = pretty name
        pretty (PFieldWildcard _) = text ".."

--------------------- Regular Patterns -------------------------

instance SrcInfo loc => Pretty (RPat loc) where
        pretty (RPOp _ r op) = pretty r <> pretty op
        pretty (RPEither _ r1 r2) = parens . myFsep $
                [pretty r1, char '|', pretty r2]
        pretty (RPSeq _ rs) =
                myFsep $ text "(/" : map pretty rs ++ [text "/)"]
        pretty (RPGuard _ r gs) =
                myFsep $ text "(|" : pretty r : char '|' : map pretty gs ++ [text "|)"]
        -- special case that would otherwise be buggy
        pretty (RPCAs _ n (RPPat _ (PIrrPat _ p))) =
                myFsep [pretty n <> text "@:", char '~' <> pretty p]
        pretty (RPCAs _ n r) = hcat [pretty n, text "@:", pretty r]
        -- special case that would otherwise be buggy
        pretty (RPAs _ n (RPPat _ (PIrrPat _ p))) =
                myFsep [pretty n <> text "@:", char '~' <> pretty p]
        pretty (RPAs _ n r) = hcat [pretty n, char '@', pretty r]
        pretty (RPPat _ p) = pretty p
        pretty (RPParen _ rp) = parens . pretty $ rp

instance Pretty (RPatOp l) where
        pretty (RPStar  _) = char '*'
        pretty (RPStarG _) = text "*!"
        pretty (RPPlus  _) = char '+'
        pretty (RPPlusG _) = text "+!"
        pretty (RPOpt   _) = char '?'
        pretty (RPOptG  _) = text "?!"

------------------------- Case bodies  -------------------------
instance SrcInfo loc => Pretty (Alt loc) where
        pretty (Alt _pos e gAlts binds) =
                pretty e <+> pretty gAlts $$$ ppWhere binds

instance SrcInfo loc => Pretty (GuardedAlts loc) where
        pretty (UnGuardedAlt _ e) = text "->" <+> pretty e
        pretty (GuardedAlts _ altList) = myVcat . map pretty $ altList

instance SrcInfo loc => Pretty (GuardedAlt loc) where
        pretty (GuardedAlt _pos guards body) =
                myFsep $ char '|': (punctuate comma . map pretty $ guards) ++ [text "->", pretty body]

------------------------- Statements in monads, guards & list comprehensions -----
instance SrcInfo loc => Pretty (Stmt loc) where
        pretty (Generator _loc e from) =
                pretty e <+> text "<-" <+> pretty from
        pretty (Qualifier _ e) = pretty e
        -- two cases for lets
        pretty (LetStmt _ (BDecls _ declList)) =
                ppLetStmt declList
        pretty (LetStmt _ (IPBinds _ bindList)) =
                ppLetStmt bindList
        pretty (RecStmt _ stmtList) =
                text "rec" $$$ ppBody letIndent (map pretty stmtList)

ppLetStmt l = text "let" $$$ ppBody letIndent (map pretty l)

instance SrcInfo loc => Pretty (QualStmt loc) where
        pretty (QualStmt _ s) = pretty s
        pretty (ThenTrans    _ f)    = myFsep $ [text "then", pretty f]
        pretty (ThenBy       _ f e)  = myFsep $ [text "then", pretty f, text "by", pretty e]
        pretty (GroupBy      _ e)    = myFsep $ [text "then", text "group", text "by", pretty e]
        pretty (GroupUsing   _ f)    = myFsep $ [text "then", text "group", text "using", pretty f]
        pretty (GroupByUsing _ e f)  = myFsep $ [text "then", text "group", text "by",
                                                  pretty e, text "using", pretty f]



------------------------- Record updates
instance SrcInfo loc => Pretty (FieldUpdate loc) where
        pretty (FieldUpdate _ name e) =
                myFsep [pretty name, equals, pretty e]
        pretty (FieldPun _ name) = pretty name
        pretty (FieldWildcard _) = text ".."

------------------------- Names -------------------------
instance Pretty (QOp l) where
        pretty (QVarOp _ n) = ppQNameInfix n
        pretty (QConOp _ n) = ppQNameInfix n

ppQNameInfix :: (QName l) -> Doc
ppQNameInfix name
        | isSymbolName (getName name) = ppQName name
        | otherwise = char '`' <> ppQName name <> char '`'

instance Pretty (QName l) where
        pretty name = case name of
                UnQual _ (Symbol _ ('#':_)) -> char '(' <+> ppQName name <+> char ')'
                _ -> parensIf (isSymbolName (getName name)) (ppQName name)

ppQName :: (QName l) -> Doc
ppQName (UnQual _ name) = ppName name
ppQName (Qual _ m name) = pretty m <> char '.' <> ppName name
ppQName (Special _ sym) = text (specialName sym)

instance Pretty (Op l) where
        pretty (VarOp _ n) = ppNameInfix n
        pretty (ConOp _ n) = ppNameInfix n

ppNameInfix :: (Name l) -> Doc
ppNameInfix name
        | isSymbolName name = ppName name
        | otherwise = char '`' <> ppName name <> char '`'

instance Pretty (Name l) where
        pretty name = case name of
                Symbol _ ('#':_) -> char '(' <+> ppName name <+> char ')'
                _ -> parensIf (isSymbolName name) (ppName name)

ppName :: (Name l) -> Doc
ppName (Ident  _ s) = text s
ppName (Symbol _ s) = text s

instance Pretty (IPName l) where
        pretty (IPDup _ s) = char '?' <> text s
        pretty (IPLin _ s) = char '%' <> text s

instance SrcInfo loc => Pretty (IPBind loc) where
        pretty (IPBind _loc ipname exp) =
                myFsep [pretty ipname, equals, pretty exp]

instance Pretty (CName l) where
        pretty (VarName _ n) = pretty n
        pretty (ConName _ n) = pretty n

isSymbolName :: (Name l) -> Bool
isSymbolName (Symbol _ _) = True
isSymbolName _ = False

getName :: QName l -> Name l
getName (UnQual _ s) = s
getName (Qual _ _ s) = s
getName (Special l (Cons _)) = Symbol l ":"
getName (Special l (FunCon _)) = Symbol l "->"
getName (Special l s) = Ident l (specialName s)

specialName :: SpecialCon l -> String
specialName (UnitCon _) = "()"
specialName (ListCon _) = "[]"
specialName (FunCon  _) = "->"
specialName (TupleCon _ b n) = "(" ++ hash ++ replicate (n-1) ',' ++ hash ++ ")"
    where hash = case b of
                   Unboxed -> "#"
                   _       -> ""
specialName (Cons _) = ":"

ppContext :: Maybe (Context l) -> Doc
ppContext = maybePP pretty

instance Pretty (Context l) where
        pretty (CxEmpty _) = mySep [text "()", text "=>"]
        pretty (CxSingle _ asst) = mySep [pretty asst, text "=>"]
        pretty (CxTuple _ assts) = myFsep $ [parenList (map pretty assts), text "=>"]
        pretty (CxParen _ asst)  = parens (pretty asst)

-- hacked for multi-parameter type classes
instance Pretty (Asst l) where
        pretty (ClassA _ a ts)   = myFsep $ ppQName a : map ppAType ts
        pretty (InfixA _ a op b) = myFsep $ [pretty a, ppQNameInfix op, pretty b]
        pretty (IParam _ i t)    = myFsep $ [pretty i, text "::", pretty t]
        pretty (EqualP _ t1 t2)  = myFsep $ [pretty t1, text "~", pretty t2]

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

-- Pretty print a source location, useful for printing out error messages
instance Pretty SrcLoc where
  pretty srcLoc =
    return $ P.hsep [ colonFollow (P.text $ srcFilename srcLoc)
                    , colonFollow (P.int  $ srcLine     srcLoc)
                    , P.int $ srcColumn srcLoc
                    ]
    where
    colonFollow p = P.hcat [ p, P.colon ]

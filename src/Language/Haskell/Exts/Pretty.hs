{-# LANGUAGE CPP #-}
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
                PPHsMode(..), Indent, PPLayout(..), defaultMode
                -- * Primitive Printers
                , prettyPrim, prettyPrimWithMode
                ) where

import Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts.ParseSyntax as P

import Language.Haskell.Exts.SrcLoc hiding (loc)

import Prelude hiding (exp)
import qualified Text.PrettyPrint as P
import Data.List (intersperse)
import Data.Maybe (isJust , fromMaybe)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..), (<$>))
#endif
import qualified Control.Monad as M (ap)

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
                multiIfIndent :: Indent,
                                -- | indentation of the body of a
                                -- multi-@if@ expression
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
                      multiIfIndent = 3,
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

instance Applicative (DocM s) where
        pure = retDocM
        (<*>) = M.ap

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
thenDocM m k = DocM $ \s -> case unDocM m s of a -> unDocM (k a) s

then_DocM :: DocM s a -> DocM s b -> DocM s b
then_DocM m k = DocM $ \s -> case unDocM m s of _ -> unDocM k s

retDocM :: a -> DocM s a
retDocM a = DocM $ const a

unDocM :: DocM s a -> s -> a
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

text :: String -> Doc
text = return . P.text

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

-- rational :: Rational -> Doc
-- rational = return . P.rational

-- Simple Combining Forms

parens, brackets, braces, doubleQuotes :: Doc -> Doc
parens d = d >>= return . P.parens
brackets d = d >>= return . P.brackets
braces d = d >>= return . P.braces
-- quotes :: Doc -> Doc
-- quotes d = d >>= return . P.quotes
doubleQuotes d = d >>= return . P.doubleQuotes

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

-- Constants

semi,comma,space,equals :: Doc
semi = return P.semi
comma = return P.comma
-- colon :: Doc
-- colon = return P.colon
space = return P.space
equals = return P.equals

{-
lparen,rparen,lbrack,rbrack,lbrace,rbrace :: Doc
lparen = return  P.lparen
rparen = return  P.rparen
lbrack = return  P.lbrack
rbrack = return  P.rbrack
lbrace = return  P.lbrace
rbrace = return  P.rbrace
-}

-- Combinators

(<>),(<+>),($$) :: Doc -> Doc -> Doc
aM <> bM = do{a<-aM;b<-bM;return (a P.<> b)}
aM <+> bM = do{a<-aM;b<-bM;return (a P.<+> b)}
aM $$ bM = do{a<-aM;b<-bM;return (a P.$$ b)}
($+$) :: Doc -> Doc -> Doc
aM $+$ bM = do{a<-aM;b<-bM;return (a P.$+$ b)}

hcat,hsep,vcat,fsep :: [Doc] -> Doc
hcat dl = sequence dl >>= return . P.hcat
hsep dl = sequence dl >>= return . P.hsep
vcat dl = sequence dl >>= return . P.vcat
-- sep, cat, fcat :: [Doc] -> Doc
-- sep dl = sequence dl >>= return . P.sep
-- cat dl = sequence dl >>= return . P.cat
fsep dl = sequence dl >>= return . P.fsep
-- fcat dl = sequence dl >>= return . P.fcat

-- Some More

-- hang :: Doc -> Int -> Doc -> Doc
-- hang dM i rM = do{d<-dM;r<-rM;return $ P.hang d i r}

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
-- renderWithMode :: PPHsMode -> Doc -> String
-- renderWithMode = renderStyleMode P.style

-- | render the document with 'defaultMode'.
-- render :: Doc -> String
-- render = renderWithMode defaultMode

-- | pretty-print with a given style and mode.
prettyPrintStyleMode :: Pretty a => P.Style -> PPHsMode -> a -> String
prettyPrintStyleMode ppStyle ppMode = renderStyleMode ppStyle ppMode . pretty

-- | pretty-print with the default style and a given mode.
prettyPrintWithMode :: Pretty a => PPHsMode -> a -> String
prettyPrintWithMode = prettyPrintStyleMode P.style

-- | pretty-print with the default style and 'defaultMode'.
prettyPrint :: Pretty a => a -> String
prettyPrint = prettyPrintWithMode defaultMode

-- fullRenderWithMode :: PPHsMode -> P.Mode -> Int -> Float ->
--                       (P.TextDetails -> a -> a) -> a -> Doc -> a
-- fullRenderWithMode ppMode m i f fn e mD =
--                   P.fullRender m i f fn e $ (unDocM mD) ppMode


-- fullRender :: P.Mode -> Int -> Float -> (P.TextDetails -> a -> a)
--               -> a -> Doc -> a
-- fullRender = fullRenderWithMode defaultMode

-- | pretty-print with the default style and 'defaultMode'.
prettyPrim :: Pretty a => a -> P.Doc
prettyPrim = prettyPrimWithMode defaultMode

-- | pretty-print with the default style and a given mode.
prettyPrimWithMode :: Pretty a => PPHsMode -> a -> P.Doc
prettyPrimWithMode pphs doc = unDocM (pretty doc) pphs


-------------------------  Pretty-Print a Module --------------------
{-
instance  Pretty (Module l) where
        pretty (Module pos m os mbWarn mbExports imp decls) =
                markLine pos $ (myVcat $ map pretty os) $$
                myVcat (
                    (if m == ModuleName "" then id
                     else \x -> [topLevel (ppModuleHeader m mbWarn mbExports) x])
                    (map pretty imp ++
                      ppDecls (m /= ModuleName "" ||
                               not (null imp) ||
                               not (null os))
                              decls]-}

--------------------------  Module Header ------------------------------
instance Pretty (ModuleHead l) where
  pretty (ModuleHead _ m mbWarn mbExportList) =
    mySep [
        text "module",
        pretty m,
        maybePP ppWarnTxt mbWarn,
        maybePP pretty mbExportList,
        text "where"]

instance Pretty (ExportSpecList l) where
        pretty (ExportSpecList _ especs)  = parenList $ map pretty especs

ppWarnTxt :: WarningText l -> Doc
ppWarnTxt (DeprText _ s) = mySep [text "{-# DEPRECATED", text (show s), text "#-}"]
ppWarnTxt (WarnText _ s) = mySep [text "{-# WARNING",    text (show s), text "#-}"]

instance  Pretty (ModuleName l) where
        pretty (ModuleName _ modName) = text modName

instance  Pretty (Namespace l) where
        pretty NoNamespace {}     = empty
        pretty TypeNamespace {}   = text "type"
        pretty PatternNamespace {} = text "pattern"

instance  Pretty (ExportSpec l) where
        pretty (EVar _ name)                = pretty name
        pretty (EAbs _ ns name)             = pretty ns <+> pretty name
        pretty (EThingWith _ wc name nameList) =
          let prettyNames = map pretty nameList
              names = case wc of
                        NoWildcard {} -> prettyNames
                        EWildcard _ n  ->
                          let (before,after) = splitAt n prettyNames
                          in before ++ [text ".."] ++ after
           in pretty name <> (parenList names)
        pretty (EModuleContents _ m)        = text "module" <+> pretty m

instance  Pretty (ImportDecl l) where
        pretty (ImportDecl _ m qual src safe mbPkg mbName mbSpecs) =
                mySep [text "import",
                       if src  then text "{-# SOURCE #-}" else empty,
                       if safe then text "safe" else empty,
                       if qual then text "qualified" else empty,
                       maybePP (\s -> text (show s)) mbPkg,
                       pretty m,
                       maybePP (\m' -> text "as" <+> pretty m') mbName,
                       maybePP pretty mbSpecs]

instance Pretty (ImportSpecList l) where
        pretty (ImportSpecList _ b ispecs)  =
            (if b then text "hiding" else empty)
                <+> parenList (map pretty ispecs)

instance  Pretty (ImportSpec l) where
        pretty (IVar _ name  )              = pretty name
        pretty (IAbs _ ns name)             = pretty ns <+> pretty name
        pretty (IThingAll _ name)           = pretty name <> text "(..)"
        pretty (IThingWith _ name nameList) =
                pretty name <> (parenList . map pretty $ nameList)

instance  Pretty (TypeEqn l) where
        pretty (TypeEqn _ pat eqn) = mySep [pretty pat, equals, pretty eqn]

-------------------------  Declarations ------------------------------
class Pretty a => PrettyDeclLike a where
  wantsBlankline :: a -> Bool

instance  PrettyDeclLike (Decl l) where
  wantsBlankline (FunBind {}) = False
  wantsBlankline (PatBind {}) = False
  wantsBlankline _ = True

condBlankline :: PrettyDeclLike a => a -> Doc
condBlankline d = (if wantsBlankline d then blankline else id) $ pretty d

ppDecls :: PrettyDeclLike a => Bool -> [a] -> [Doc]
ppDecls True  ds     = map condBlankline ds
ppDecls False (d:ds) = pretty d : map condBlankline ds
ppDecls _ _ = []
--ppDecls = map condBlankline

instance Pretty (InjectivityInfo l) where
  pretty (InjectivityInfo _ from to) =
    char '|' <+> pretty from <+> text "->" <+> hsep (map pretty to)

instance Pretty (ResultSig l) where
  pretty (KindSig _ kind) = text "::" <+> pretty kind
  pretty (TyVarSig _ tv)  = char '='  <+> pretty tv

instance  Pretty (Decl l) where
        pretty (TypeDecl _ dHead htype) =
                mySep ( [text "type", pretty dHead]
                        ++ [equals, pretty htype])

        pretty (DataDecl _ don context dHead constrList derives) =
                mySep ( [pretty don, maybePP pretty context, pretty dHead])

                  <+> (myVcat (zipWith (<+>) (equals : repeat (char '|'))
                                             (map pretty constrList))
                        $$$ maybePP pretty derives)

        pretty (GDataDecl _ don context dHead optkind gadtList derives) =
                mySep ( [pretty don, maybePP pretty context, pretty dHead]
                        ++ ppOptKind optkind ++ [text "where"])
                        $$$ ppBody classIndent (map pretty gadtList)
                        $$$ ppIndent letIndent [maybePP pretty derives]

        pretty (TypeFamDecl _ dHead optkind optinj) =
                mySep ([text "type", text "family", pretty dHead
                       , maybePP pretty optkind, maybePP pretty optinj])

        pretty (ClosedTypeFamDecl _ dHead optkind optinj eqns) =
                mySep ([text "type", text "family", pretty dHead
                       , maybePP pretty optkind ,maybePP pretty optinj
                       , text "where"]) $$$ ppBody classIndent (map pretty eqns)

        pretty (DataFamDecl _ context dHead optkind) =
                mySep ( [text "data", text "family", maybePP pretty context, pretty dHead
                        , maybePP pretty optkind])

        pretty (TypeInsDecl _ ntype htype) =
                mySep [text "type", text "instance", pretty ntype, equals, pretty htype]

        pretty (DataInsDecl _ don ntype constrList derives) =
                mySep [pretty don, text "instance ", pretty ntype]
                        <+> (myVcat (zipWith (<+>) (equals : repeat (char '|'))
                                                   (map pretty constrList))
                              $$$ maybePP pretty derives)

        pretty (GDataInsDecl _ don ntype optkind gadtList derives) =
                mySep ( [pretty don, text "instance ", pretty ntype]
                        ++ ppOptKind optkind ++ [text "where"])
                        $$$ ppBody classIndent (map pretty gadtList)
                        $$$ maybePP pretty derives

        --m{spacing=False}
        -- special case for empty class declaration
        pretty (ClassDecl _ context dHead fundeps Nothing) =
                mySep ( [text "class", maybePP pretty context, pretty dHead
                        , ppFunDeps fundeps])
        pretty (ClassDecl _ context dHead fundeps declList) =
                mySep ( [text "class", maybePP pretty context, pretty dHead
                        , ppFunDeps fundeps, text "where"])
                $$$ ppBody classIndent (fromMaybe [] ((ppDecls False) <$> declList))

        -- m{spacing=False}
        -- special case for empty instance  declaration
        pretty (InstDecl _ moverlap iHead Nothing) =
                  mySep ( [text "instance", maybePP pretty moverlap, pretty iHead])
        pretty (InstDecl _ overlap iHead declList) =
                mySep ( [ text "instance", maybePP pretty overlap
                           , pretty iHead, text "where"])
                $$$ ppBody classIndent (fromMaybe [] ((ppDecls False) <$> declList))

        pretty (DerivDecl _ overlap irule) =
                  mySep ( [text "deriving"
                          , text "instance"
                          , maybePP pretty overlap
                          , pretty irule])
        pretty (DefaultDecl _ htypes) =
                text "default" <+> parenList (map pretty htypes)

        pretty (SpliceDecl _ splice) =
                pretty splice

        pretty (TypeSig _ nameList qualType) =
                mySep ((punctuate comma . map pretty $ nameList)
                      ++ [text "::", pretty qualType])

        --  Req can be ommitted if it is empty
        --  We must print prov if req is nonempty
        pretty (PatSynSig _ n mtvs prov req t) =
                let contexts = map (maybePP pretty) [prov, req]
                 in
                  mySep ( [text "pattern", pretty n, text "::", ppForall mtvs] ++
                          contexts ++ [pretty t] )


        pretty (FunBind _ matches) = do
                e <- fmap layout getPPEnv
                case e of PPOffsideRule -> foldr ($$$) empty (map pretty matches)
                          _ -> foldr (\x y -> x <> semi <> y) empty (map pretty matches)

        pretty (PatBind _ pat rhs whereBinds) =
                myFsep [pretty pat, pretty rhs] $$$ ppWhere whereBinds

        pretty (InfixDecl _ assoc prec opList) =
                mySep ([pretty assoc, maybePP int prec]
                       ++ (punctuate comma . map pretty $ opList))

        pretty (PatSyn _ pat rhs dir) =
                let sep = case dir of
                            ImplicitBidirectional {}   -> "="
                            ExplicitBidirectional {}   -> "<-"
                            Unidirectional {}          -> "<-"
                in
                 (mySep ([text "pattern", pretty pat, text sep, pretty rhs])) $$$
                    (case dir of
                      ExplicitBidirectional _ ds ->
                        nest 2 (text "where" $$$ ppBody whereIndent (ppDecls False ds))
                      _ -> empty)

        pretty (ForImp _ cconv saf str name typ) =
                mySep [text "foreign import", pretty cconv, maybePP pretty saf,
                       maybe empty (text . show) str, pretty name, text "::", pretty typ]

        pretty (ForExp _ cconv str name typ) =
                mySep [text "foreign export", pretty cconv,
                       text (show str), pretty name, text "::", pretty typ]

        pretty (RulePragmaDecl _ rules) =
                myVcat $ text "{-# RULES" : map pretty rules ++ [text " #-}"]

        pretty (DeprPragmaDecl _ deprs) =
                myVcat $ text "{-# DEPRECATED" : map ppWarnDepr deprs ++ [text " #-}"]

        pretty (WarnPragmaDecl _ deprs) =
                myVcat $ text "{-# WARNING" : map ppWarnDepr deprs ++ [text " #-}"]

        pretty (InlineSig _ inl activ name) =
                mySep [text (if inl then "{-# INLINE" else "{-# NOINLINE")
                      , maybePP pretty activ, pretty name, text "#-}"]

        pretty (InlineConlikeSig _ activ name) =
                mySep [ text "{-# INLINE CONLIKE", maybePP pretty activ
                      , pretty name, text "#-}"]

        pretty (SpecSig _ activ name types) =
                mySep $ [text "{-# SPECIALISE", maybePP pretty activ
                        , pretty name, text "::"]
                         ++ punctuate comma (map pretty types) ++ [text "#-}"]

        pretty (SpecInlineSig _ inl activ name types) =
                mySep $ [text "{-# SPECIALISE", text (if inl then "INLINE" else "NOINLINE"),
                        maybePP pretty activ, pretty name, text "::"]
                        ++ (punctuate comma $ map pretty types) ++ [text "#-}"]

        pretty (InstSig _ irule) =
                mySep $ [ text "{-# SPECIALISE", text "instance", pretty irule
                        , text "#-}"]

        pretty (AnnPragma _ annp) =
                mySep [text "{-# ANN", pretty annp, text "#-}"]

        pretty (MinimalPragma _ b) =
                let bs = case b of { Just b' -> pretty b'; _ -> empty }
                in myFsep [text "{-# MINIMAL", bs, text "#-}"]

        pretty (RoleAnnotDecl _ qn rs) =
                mySep ( [text "type", text "role", pretty qn]
                        ++ map pretty rs )
        pretty (CompletePragma _ cls opt_ts) =
                let cls_p = punctuate comma $ map pretty cls
                    ts_p  = maybe empty (\tc -> text "::" <+> pretty tc) opt_ts
                in myFsep $ [text "{-# COMPLETE"] ++ cls_p ++ [ts_p, text "#-}"]

instance Pretty (InstRule l) where
    pretty (IRule _ tvs mctxt qn)  =
            mySep [ppForall tvs
                  , maybePP pretty mctxt, pretty qn]
    pretty (IParen _ ih)        = parens (pretty ih)

instance  Pretty (InstHead l) where
    pretty (IHCon _ qn)          = pretty qn
    pretty (IHInfix _ ta qn)     = mySep [pretty ta, pretty qn]
    pretty (IHParen _ ih)        = parens (pretty ih)
    pretty (IHApp _ ih t)        = myFsep [pretty ih, pretty t]


instance  Pretty (Annotation l) where
        pretty (Ann _ n e) = myFsep [pretty n, pretty e]
        pretty (TypeAnn _ n e) = myFsep [text "type", pretty n, pretty e]
        pretty (ModuleAnn _ e) = myFsep [text "module", pretty e]

instance  Pretty (BooleanFormula l) where
        pretty (VarFormula _ n)   = pretty n
        pretty (AndFormula _ bs)  = myFsep $ punctuate (text " ,") $ map pretty bs
        pretty (OrFormula _ bs)   = myFsep $ punctuate (text " |") $ map pretty bs
        pretty (ParenFormula _ b) = parens $ pretty b

instance  Pretty (Role l) where
        pretty RoleWildcard{}     = char '_'
        pretty Nominal{}          = text "nominal"
        pretty Representational{} = text "representational"
        pretty Phantom{}          = text "phantom"

instance  Pretty (DataOrNew l) where
        pretty DataType{} = text "data"
        pretty NewType{}  = text "newtype"

instance  Pretty (Assoc l) where
        pretty AssocNone{}  = text "infix"
        pretty AssocLeft{}  = text "infixl"
        pretty AssocRight{} = text "infixr"

instance  Pretty (Match l) where
        pretty (InfixMatch _ l op rs rhs wbinds) =
          let
              lhs = case rs of
                      []  -> [] -- Should never reach
                      (r:rs') ->
                        let hd = [prettyPrec 2 l, ppNameInfix op, prettyPrec 2 r]
                        in if null rs'
                            then hd
                            else parens (myFsep hd) : map (prettyPrec 3) rs'

          in myFsep (lhs ++ [pretty rhs]) $$$ ppWhere wbinds
        pretty (Match _ f ps rhs whereBinds) =
                myFsep (pretty f : map (prettyPrec 3) ps ++ [pretty rhs])
                $$$ ppWhere whereBinds

ppWhere :: Maybe (Binds l) -> Doc
ppWhere Nothing            = empty
ppWhere (Just (BDecls _ l))  = nest 2 (text "where" $$$ ppBody whereIndent (ppDecls False l))
ppWhere (Just (IPBinds _ b)) = nest 2 (text "where" $$$ ppBody whereIndent (ppDecls False b))

instance  PrettyDeclLike (ClassDecl l) where
    wantsBlankline (ClsDecl _ d) = wantsBlankline d
    wantsBlankline (ClsDefSig {}) = True
    wantsBlankline _ = False

instance  Pretty (ClassDecl l) where
    pretty (ClsDecl _ decl) = pretty decl

    pretty (ClsDataFam _ context declHead optkind) =
                mySep ( [text "data", maybePP pretty context, pretty declHead
                        , maybePP pretty optkind])

    pretty (ClsTyFam _ declHead optkind optinj) =
                mySep ( [text "type", pretty declHead
                        , maybePP pretty optkind, maybePP pretty optinj])

    pretty (ClsTyDef _ ntype) =
                mySep [text "type", pretty ntype]

    pretty (ClsDefSig _ name typ) =
                mySep [
                    text "default",
                    pretty name,
                    text "::",
                    pretty typ]

instance Pretty (DeclHead l) where
  pretty (DHead _ n) = pretty n
  pretty (DHInfix _ tv n) =  pretty tv <+> ppNameInfix n
  pretty (DHParen _ d) = parens (pretty d)
  pretty (DHApp _ dh tv) = pretty dh <+> pretty tv



instance  PrettyDeclLike (InstDecl l) where
    wantsBlankline (InsDecl _ d) = wantsBlankline d
    wantsBlankline _ = False

instance  Pretty (InstDecl l) where
        pretty (InsDecl _ decl) = pretty decl

        pretty (InsType _ ntype htype) =
                mySep [text "type", pretty ntype, equals, pretty htype]

        pretty (InsData _ don ntype constrList derives) =
                mySep [pretty don, pretty ntype]
                        <+> (myVcat (zipWith (<+>) (equals : repeat (char '|'))
                                                   (map pretty constrList))
                              $$$ maybePP pretty derives)

        pretty (InsGData _ don ntype optkind gadtList derives) =
                mySep ( [pretty don, pretty ntype]
                        ++ ppOptKind optkind ++ [text "where"])
                        $$$ ppBody classIndent (map pretty gadtList)
                        $$$ maybePP pretty derives

--        pretty (InsInline loc inl activ name) =
--                markLine loc $
--                mySep [text (if inl then "{-# INLINE" else "{-# NOINLINE"), pretty activ, pretty name, text "#-}"]


------------------------- FFI stuff -------------------------------------
instance  Pretty (Safety l) where
        pretty PlayRisky {}        = text "unsafe"
        pretty (PlaySafe _ b)      = text $ if b then "threadsafe" else "safe"
        pretty PlayInterruptible {} = text "interruptible"

instance  Pretty (CallConv l) where
        pretty StdCall {}    = text "stdcall"
        pretty CCall {}     = text "ccall"
        pretty CPlusPlus {}  = text "cplusplus"
        pretty DotNet {}     = text "dotnet"
        pretty Jvm {}        = text "jvm"
        pretty Js {}         = text "js"
        pretty JavaScript {} = text "javascript"
        pretty CApi {}       = text "capi"

------------------------- Pragmas ---------------------------------------
ppWarnDepr :: ([Name l], String) -> Doc
ppWarnDepr (names, txt) = mySep $ punctuate comma (map pretty names) ++ [text $ show txt]

instance  Pretty (Rule l) where
        pretty (Rule _ tag activ rvs rhs lhs) =
            mySep [text $ show tag, maybePP pretty activ,
                        maybePP ppRuleVars rvs,
                        pretty rhs, char '=', pretty lhs]

ppRuleVars :: [RuleVar l] -> Doc
ppRuleVars []  = empty
ppRuleVars rvs = mySep $ text "forall" : map pretty rvs ++ [char '.']

instance  Pretty (Activation l) where
    pretty (ActiveFrom _ i)  = char '['  <> int i <> char ']'
    pretty (ActiveUntil _ i) = text "[~" <> int i <> char ']'

instance  Pretty (Overlap l) where
    pretty Overlap {}   = text "{-# OVERLAP #-}"
    pretty NoOverlap {}  = text "{-# NO_OVERLAP #-}"
    pretty Incoherent {} = text "{-# INCOHERENT #-}"

instance  Pretty (RuleVar l) where
    pretty (RuleVar _ n) = pretty n
    pretty (TypedRuleVar _ n t) = parens $ mySep [pretty n, text "::", pretty t]

-- Spaces are stripped from the pragma text but other whitespace
-- is not.
ppOptionsPragma :: Doc -> String -> Doc
ppOptionsPragma opt s =
  case s of
    ('\n':_) -> opt <> text s <> text "#-}"
    _ ->  myFsep [opt, text s <> text "#-}"]

instance  Pretty (ModulePragma l) where
    pretty (LanguagePragma _ ns) =
        myFsep $ text "{-# LANGUAGE" : punctuate (char ',') (map pretty ns) ++ [text "#-}"]
    pretty (OptionsPragma _ (Just tool) s) =
        ppOptionsPragma (text "{-# OPTIONS_" <> pretty tool) s
    pretty (OptionsPragma _ _ s) =
        ppOptionsPragma (text "{-# OPTIONS") s
    pretty (AnnModulePragma _ mann) =
        myFsep [text "{-# ANN", pretty mann, text "#-}"]


instance Pretty Tool where
    pretty (UnknownTool s) = text s
    pretty t               = text $ show t

------------------------- Data & Newtype Bodies -------------------------
instance  Pretty (QualConDecl l) where
        pretty (QualConDecl _pos tvs ctxt con) =
                myFsep [ppForall tvs, maybePP pretty ctxt, pretty con]

instance  Pretty (GadtDecl l) where
        pretty (GadtDecl _pos name names ty) =
            case names of
                Nothing ->
                    myFsep [pretty name, text "::", pretty ty]
                Just ts' ->
                    myFsep [pretty name, text "::" ,
                         braceList . map pretty $ ts', text "->", pretty ty]

instance  Pretty (ConDecl l) where
        pretty (RecDecl _ name fieldList) =
                pretty name <> braceList (map pretty fieldList)

{-        pretty (ConDecl name@(Symbol _) [l, r]) =
                myFsep [prettyPrec prec_btype l, ppName name,
                        prettyPrec prec_btype r] -}
        pretty (ConDecl _ name typeList) =
                mySep $ pretty name : map (prettyPrec prec_atype) typeList
        pretty (InfixConDecl _ l name r) =
                myFsep [prettyPrec prec_btype l, ppNameInfix name,
                         prettyPrec prec_btype r]


instance Pretty (FieldDecl l) where
  pretty (FieldDecl _ names ty) =
        myFsepSimple $ (punctuate comma . map pretty $ names) ++
                       [text "::", pretty ty]

instance  Pretty (BangType l) where
        pretty BangedTy {}  = char '!'
        pretty LazyTy {}    = char '~'
        pretty NoStrictAnnot {} = empty

instance Pretty (Unpackedness l) where
        pretty Unpack {}  = text "{-# UNPACK #-} "
        pretty NoUnpack {} = text "{-# NOUNPACK #-} "
        pretty NoUnpackPragma {} = empty

instance Pretty (Deriving l) where
  pretty (Deriving _ [])  = empty
  pretty (Deriving _ [d]) = text "deriving" <+> pretty d
  pretty (Deriving _ d)   = text "deriving" <+> parenList (map pretty d)

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

instance  Pretty (Type l) where
        prettyPrec p (TyForall _ mtvs ctxt htype) = parensIf (p > 0) $
                myFsep [ppForall mtvs, maybePP pretty ctxt, pretty htype]
        prettyPrec p (TyFun _ a b) = parensIf (p > 0) $
                myFsep [ppBType a, text "->", pretty b]
        prettyPrec _ (TyTuple _ bxd l) =
                let ds = map pretty l
                 in case bxd of
                        Boxed   -> parenList ds
                        Unboxed -> hashParenList ds
        prettyPrec _ (TyList _ t)  = brackets $ pretty t
        prettyPrec _ (TyParArray _ t) = bracketColonList [pretty t]
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
        prettyPrec _ (TyPromoted _ p) = pretty p
        prettyPrec p (TyEquals _ a b) = parensIf (p > 0) (myFsep [pretty a, text "~", pretty b])
        prettyPrec _ (TySplice _ s) = pretty s
        prettyPrec _ (TyBang _ b u t) = pretty u <> pretty b <> prettyPrec prec_atype t
        prettyPrec _ (TyWildCard _ mn) = char '_' <> maybePP pretty mn
        prettyPrec _ (TyQuasiQuote _ n qt) = text ("[" ++ n ++ "|" ++ qt ++ "|]")

instance  Pretty (Promoted l) where
  pretty p =
    case p of
      PromotedInteger _ n _ -> integer n
      PromotedString _ s _ -> doubleQuotes $ text s
      PromotedCon _ hasQuote qn ->
        addQuote hasQuote $ maybe (pretty qn) pretty (getSpecialName qn)
      PromotedList _ hasQuote list ->
        addQuote hasQuote $ bracketList . punctuate comma . map pretty $ list
      PromotedTuple _ list ->
        addQuote True $ parenList $ map pretty list
      PromotedUnit {} -> addQuote True $ text "()"
    where
      addQuote True doc = char '\'' <> doc
      addQuote False doc = doc

instance  Pretty (TyVarBind l) where
        pretty (KindedVar _ var kind) = parens $ myFsep [pretty var, text "::", pretty kind]
        pretty (UnkindedVar _ var)    = pretty var

ppForall :: Maybe [TyVarBind l] -> Doc
ppForall Nothing   = empty
ppForall (Just []) = empty
ppForall (Just vs) =    myFsep (text "forall" : map pretty vs ++ [char '.'])

---------------------------- Kinds ----------------------------

instance  Pretty (Kind l) where
        prettyPrec _ KindStar{}      = text "*"
        prettyPrec n (KindFn _ a b)  = parensIf (n > 0) $ myFsep [prettyPrec 1 a, text "->", pretty b]
        prettyPrec _ (KindParen _ k) = parens $ pretty k
        prettyPrec _ (KindVar _ n)   = pretty n
        prettyPrec _ (KindTuple _ t) = parenList . map pretty $ t
        prettyPrec _ (KindList _ l)  = brackets .  pretty $ l
        prettyPrec n (KindApp _ a b) =
          parensIf (n > 3) $ myFsep [prettyPrec 3 a, prettyPrec 4 b]

ppOptKind :: Maybe (Kind l) -> [Doc]
ppOptKind Nothing  = []
ppOptKind (Just k) = [text "::", pretty k]

------------------- Functional Dependencies -------------------
instance  Pretty (FunDep l) where
        pretty (FunDep _ from to) =
                myFsep $ map pretty from ++ [text "->"] ++ map pretty to


ppFunDeps :: [FunDep l] -> Doc
ppFunDeps []  = empty
ppFunDeps fds = myFsep $ (char '|':) . punctuate comma . map pretty $ fds

------------------------- Expressions -------------------------
instance  Pretty (Rhs l) where
        pretty (UnGuardedRhs _ e) = equals <+> pretty e
        pretty (GuardedRhss _ guardList) = myVcat . map pretty $ guardList

instance  Pretty (GuardedRhs l) where
        pretty (GuardedRhs _pos guards ppBody') =
                myFsep $ [char '|'] ++ (punctuate comma . map pretty $ guards) ++ [equals, pretty ppBody']

newtype GuardedAlts l = GuardedAlts (Rhs l)
newtype GuardedAlt l = GuardedAlt (GuardedRhs l)

instance  Pretty (GuardedAlts l) where
        pretty (GuardedAlts (UnGuardedRhs _ e)) = text "->" <+> pretty e
        pretty (GuardedAlts (GuardedRhss _ guardList)) = myVcat . map (pretty . GuardedAlt) $ guardList

instance  Pretty (GuardedAlt l) where
        pretty (GuardedAlt (GuardedRhs _pos guards ppBody')) =
                myFsep $ [char '|'] ++ (punctuate comma . map pretty $ guards) ++ [text "->", pretty ppBody']

instance  Pretty (Literal l) where
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

instance  Pretty (Exp l) where
        prettyPrec _ (Lit _ l) = pretty l
        -- lambda stuff
        -- WARNING: This stuff is fragile. See #152 for one example of how
        -- things can break.
        prettyPrec p (InfixApp _ a op b) = parensIf (p > 2) $ myFsep [prettyPrec 1 a, pretty op, prettyPrec 1 b]
        prettyPrec p (NegApp _ e) = parensIf (p > 0) $ char '-' <> prettyPrec 2 e
        prettyPrec p (App _ a b) = parensIf (p > 3) $ myFsep [prettyPrec 3 a, prettyPrec 4 b]
        prettyPrec p (Lambda _loc patList ppBody') = parensIf (p > 1) $ myFsep $
                char '\\' : map (prettyPrec 3) patList ++ [text "->", pretty ppBody']
        -- keywords
        -- two cases for lets
        prettyPrec p (Let _ (BDecls _ declList) letBody) =
                parensIf (p > 1) $ ppLetExp declList letBody
        prettyPrec p (Let _ (IPBinds _ bindList) letBody) =
                parensIf (p > 1) $ ppLetExp bindList letBody

        prettyPrec p (If _ cond thenexp elsexp) = parensIf (p > 1) $
                myFsep [text "if", pretty cond,
                        text "then", pretty thenexp,
                        text "else", pretty elsexp]
        prettyPrec p (MultiIf _ alts) = parensIf (p > 1) $
                text "if"
                $$$ ppBody multiIfIndent (map (pretty . GuardedAlt) alts)
        prettyPrec p (Case _ cond altList) = parensIf (p > 1) $
                myFsep ([text "case", pretty cond, text "of"] ++
                       if null altList then [text "{", text "}"] else [])
                $$$ ppBody caseIndent (map pretty altList)
        prettyPrec p (Do _ stmtList) = parensIf (p > 1) $
                text "do" $$$ ppBody doIndent (map pretty stmtList)
        prettyPrec p (MDo _ stmtList) = parensIf (p > 1) $
                text "mdo" $$$ ppBody doIndent (map pretty stmtList)
        -- Constructors & Vars
        prettyPrec _ (Var _ name) = pretty name
        prettyPrec _ (OverloadedLabel _ name) = text ('#':name)
        prettyPrec _ (IPVar _ ipname) = pretty ipname
        prettyPrec _ (Con _ name) = pretty name
        prettyPrec _ (Tuple _ bxd expList) =
                let ds = map pretty expList
                in case bxd of
                       Boxed   -> parenList ds
                       Unboxed -> hashParenList ds
        prettyPrec _ (TupleSection _ bxd mExpList) =
                let ds = map (maybePP pretty) mExpList
                in case bxd of
                       Boxed   -> parenList ds
                       Unboxed -> hashParenList ds
        -- weird stuff
        prettyPrec _ (Paren _ e) = parens . pretty $ e
        prettyPrec _ (LeftSection _ e op) = parens (pretty e <+> pretty op)
        prettyPrec _ (RightSection _ op e) = parens (pretty op <+> pretty e)
        prettyPrec _ (RecConstr _ c fieldList) =
                pretty c <> (braceList . map pretty $ fieldList)
        prettyPrec _ (RecUpdate _ e fieldList) =
                pretty e <> (braceList . map pretty $ fieldList)
        -- Lists and parallel arrays
        prettyPrec _ (List _ list) =
                bracketList . punctuate comma . map pretty $ list
        prettyPrec _ (ParArray _ arr) =
                bracketColonList . map pretty $ arr
        prettyPrec _ (EnumFrom _ e) =
                bracketList [pretty e, text ".."]
        prettyPrec _ (EnumFromTo _ from to) =
                bracketList [pretty from, text "..", pretty to]
        prettyPrec _ (EnumFromThen _ from thenE) =
                bracketList [pretty from <> comma, pretty thenE, text ".."]
        prettyPrec _ (EnumFromThenTo _ from thenE to) =
                bracketList [pretty from <> comma, pretty thenE,
                             text "..", pretty to]
        prettyPrec _ (ParArrayFromTo _ from to) =
                bracketColonList [pretty from, text "..", pretty to]
        prettyPrec _ (ParArrayFromThenTo _ from thenE to) =
                bracketColonList [pretty from <> comma, pretty thenE,
                             text "..", pretty to]
        prettyPrec _ (ListComp _ e qualList) =
                bracketList ([pretty e, char '|']
                             ++ (punctuate comma . map pretty $ qualList))
        prettyPrec _ (ParComp _ e qualLists) =
                bracketList (punctuate (char '|') $
                                pretty e : map (hsep . punctuate comma . map pretty) qualLists)
        prettyPrec _ (ParArrayComp _ e qualArrs) =
                bracketColonList (punctuate (char '|') $
                                pretty e : map (hsep . punctuate comma . map pretty) qualArrs)
        prettyPrec p (ExpTypeSig _pos e ty) = parensIf (p > 0) $
                myFsep [pretty e, text "::", pretty ty]
        -- Template Haskell
        prettyPrec _ (BracketExp _ b) = pretty b
        prettyPrec _ (SpliceExp _ s) = pretty s
        prettyPrec _ (TypQuote _ t)  = text "\'\'" <> pretty t
        prettyPrec _ (VarQuote _ x)  = text "\'" <> pretty x
        prettyPrec _ (QuasiQuote _ n qt) = text ("[" ++ n ++ "|" ++ qt ++ "|]")
        -- Hsx
        prettyPrec _ (XTag _ n attrs mattr cs) =
                let ax = maybe [] (return . pretty) mattr
                 in hcat $
                     (myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [char '>']):
                        map pretty cs ++ [myFsep [text "</" <> pretty n, char '>']]
        prettyPrec _ (XETag _ n attrs mattr) =
                let ax = maybe [] (return . pretty) mattr
                 in myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [text "/>"]
        prettyPrec _ (XPcdata _ s) = text s
        prettyPrec _ (XExpTag _ e) =
                myFsep [text "<%", pretty e, text "%>"]
        prettyPrec _ (XChildTag _ cs) =
                myFsep $ text "<%>" : map pretty cs ++ [text "</%>"]

        -- Pragmas
        prettyPrec _ (CorePragma _ s e) = myFsep $ map text ["{-# CORE", show s, "#-}"] ++ [pretty e]
        prettyPrec _ (SCCPragma  _ s e) = myFsep $ map text ["{-# SCC",  show s, "#-}"] ++ [pretty e]
        prettyPrec _ (GenPragma  _ s (a,b) (c,d) e) =
                myFsep [text "{-# GENERATED", text $ show s,
                            int a, char ':', int b, char '-',
                            int c, char ':', int d, text "#-}", pretty e]
        -- Arrows
        prettyPrec p (Proc _ pat e) = parensIf (p > 1) $ myFsep [text "proc", pretty pat, text "->", pretty e]
        prettyPrec p (LeftArrApp _ l r)      = parensIf (p > 0) $ myFsep [pretty l, text "-<",  pretty r]
        prettyPrec p (RightArrApp _ l r)     = parensIf (p > 0) $ myFsep [pretty l, text ">-",  pretty r]
        prettyPrec p (LeftArrHighApp _ l r)  = parensIf (p > 0) $ myFsep [pretty l, text "-<<", pretty r]
        prettyPrec p (RightArrHighApp _ l r) = parensIf (p > 0) $ myFsep [pretty l, text ">>-", pretty r]

        -- LamdaCase
        prettyPrec p (LCase _ altList) = parensIf (p > 1) $
                myFsep (text "\\case":
                       if null altList then [text "{", text "}"] else [])
                $$$ ppBody caseIndent (map pretty altList)
        prettyPrec _ ExprHole{}       = char '_'
        prettyPrec _ (TypeApp _ ty)   = char '@' <> pretty ty


instance  Pretty (XAttr l) where
        pretty (XAttr _ n v) =
                myFsep [pretty n, char '=', pretty v]

instance  Pretty (XName l) where
        pretty (XName _ n) = text n
        pretty (XDomName _ d n) = text d <> char ':' <> text n

ppLetExp :: (PrettyDeclLike a, Pretty b) => [a] -> b -> Doc
ppLetExp l b = myFsep [text "let" <+> ppBody letIndent (ppDecls False l),
                        text "in", pretty b]

--------------------- Template Haskell -------------------------

instance  Pretty (Bracket l) where
        pretty (ExpBracket _ e) = ppBracket "[|" e
        pretty (PatBracket _ p) = ppBracket "[p|" p
        pretty (TypeBracket _ t) = ppBracket "[t|" t
        pretty (DeclBracket _ d) =
                myFsep $ text "[d|" : ppDecls True d ++ [text "|]"]

ppBracket :: Pretty a => String -> a -> Doc
ppBracket o x = myFsep [text o, pretty x, text "|]"]

instance  Pretty (Splice l) where
        pretty (IdSplice _ s) = char '$' <> text s
        pretty (ParenSplice _ e) =
                myFsep [text "$(", pretty e, char ')']

------------------------- Patterns -----------------------------

instance  Pretty (Pat l) where
        prettyPrec _ (PVar _ name) = pretty name
        prettyPrec _ (PLit _ (Signless {}) lit) = pretty lit
        prettyPrec p (PLit _ (Negative{}) lit) = parensIf (p > 1) $ char '-' <> pretty lit
        prettyPrec p (PInfixApp l a op b) = parensIf (p > 0) $
                myFsep [prettyPrec 1 a, pretty (QConOp l op), prettyPrec 1 b]
        prettyPrec p (PApp _ n ps) = parensIf (p > 2 && not (null ps)) $
                myFsep (pretty n : map (prettyPrec 3) ps)
        prettyPrec _ (PTuple _ bxd ps) =
                let ds = map pretty ps
                in case bxd of
                       Boxed   -> parenList ds
                       Unboxed -> hashParenList ds
        prettyPrec _ (PList _ ps) =
                bracketList . punctuate comma . map pretty $ ps
        prettyPrec _ (PParen _ pat) = parens . pretty $ pat
        prettyPrec _ (PRec _ c fields) =
                pretty c <> (braceList . map pretty $ fields)
        -- special case that would otherwise be buggy
        prettyPrec _ (PAsPat _ name (PIrrPat _ pat)) =
                myFsep [pretty name <> char '@', char '~' <> prettyPrec 3 pat]
        prettyPrec _ (PAsPat _ name pat) =
                hcat [pretty name, char '@', prettyPrec 3 pat]
        prettyPrec _ PWildCard {} = char '_'
        prettyPrec _ (PIrrPat _ pat) = char '~' <> prettyPrec 3 pat
        prettyPrec p (PatTypeSig _pos pat ty) = parensIf (p > 0) $
                myFsep [pretty pat, text "::", pretty ty]
        prettyPrec p (PViewPat _ e pat) = parensIf (p > 0) $
                myFsep [pretty e, text "->", pretty pat]
        prettyPrec p (PNPlusK _ n k) = parensIf (p > 0) $
                myFsep [pretty n, text "+", text $ show k]
        -- HaRP
        prettyPrec _ (PRPat _ rs) =
                bracketList . punctuate comma . map pretty $ rs
        -- Hsx
        prettyPrec _ (PXTag _ n attrs mattr cp) =
            let ap = maybe [] (return . pretty) mattr
             in hcat $ -- TODO: should not introduce blanks
                  (myFsep $ (char '<' <> pretty n): map pretty attrs ++ ap ++ [char '>']):
                    map pretty cp ++ [myFsep [text "</" <> pretty n, char '>']]
        prettyPrec _ (PXETag _ n attrs mattr) =
                let ap = maybe [] (return . pretty) mattr
                 in myFsep $ (char '<' <> pretty n): map pretty attrs ++ ap ++ [text "/>"]
        prettyPrec _ (PXPcdata _ s) = text s
        prettyPrec _ (PXPatTag _ p) =
                myFsep [text "<%", pretty p, text "%>"]
        prettyPrec _ (PXRPats _ ps) =
                myFsep $ text "<[" : map pretty ps ++ [text "%>"]
        -- BangPatterns
        prettyPrec _ (PBangPat _ pat) = text "!" <> prettyPrec 3 pat
        prettyPrec _ (PQuasiQuote _ n qt) = text ("[$" ++ n ++ "|" ++ qt ++ "|]")

instance  Pretty (PXAttr l) where
        pretty (PXAttr _ n p) =
                myFsep [pretty n, char '=', pretty p]

instance  Pretty (PatField l) where
        pretty (PFieldPat _ name pat) =
                myFsep [pretty name, equals, pretty pat]
        pretty (PFieldPun _ name) = pretty name
        pretty (PFieldWildcard{}) = text ".."

--------------------- Regular Patterns -------------------------

instance  Pretty (RPat l) where
        pretty (RPOp _ r op) = pretty r <> pretty op
        pretty (RPEither _ r1 r2) = parens . myFsep $
                [pretty r1, char '|', pretty r2]
        pretty (RPSeq _ rs) =
                myFsep $ text "(|" : (punctuate comma . map pretty $ rs)
                           ++ [text "|)"]
        pretty (RPGuard _ r gs) =
                myFsep $ text "(|" : pretty r : char '|' :
                           (punctuate comma . map pretty $ gs) ++ [text "|)"]
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

instance  Pretty (RPatOp l) where
        pretty RPStar{}  = char '*'
        pretty RPStarG{} = text "*!"
        pretty RPPlus{}  = char '+'
        pretty RPPlusG{} = text "+!"
        pretty RPOpt{}   = char '?'
        pretty RPOptG{}  = text "?!"

------------------------- Case bodies  -------------------------
instance  Pretty (Alt l) where
        pretty (Alt _pos e gAlts binds) =
                pretty e <+> pretty (GuardedAlts gAlts) $$$ ppWhere binds

------------------------- Statements in monads, guards & list comprehensions -----
instance  Pretty (Stmt l) where
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

ppLetStmt :: Pretty a => [a] -> Doc
ppLetStmt l = text "let" $$$ ppBody letIndent (map pretty l)

instance  Pretty (QualStmt l) where
        pretty (QualStmt _ s) = pretty s
        pretty (ThenTrans _ f)    = myFsep [text "then", pretty f]
        pretty (ThenBy _ f e)  = myFsep [text "then", pretty f, text "by", pretty e]
        pretty (GroupBy _ e)    = myFsep [text "then", text "group", text "by", pretty e]
        pretty (GroupUsing _ f)    = myFsep [text "then", text "group", text "using", pretty f]
        pretty (GroupByUsing _ e f)  = myFsep [text "then", text "group", text "by",
                                                pretty e, text "using", pretty f]



------------------------- Record updates
instance  Pretty (FieldUpdate l) where
        pretty (FieldUpdate _ name e) =
                myFsep [pretty name, equals, pretty e]
        pretty (FieldPun _ name) = pretty name
        pretty (FieldWildcard {}) = text ".."

------------------------- Names -------------------------
instance  Pretty (QOp l) where
        pretty (QVarOp _ n) = ppQNameInfix n
        pretty (QConOp _ n) = ppQNameInfix n

ppQNameInfix :: QName l -> Doc
ppQNameInfix name
        | isSymbolQName name = ppQName name
        | otherwise = char '`' <> ppQName name <> char '`'

instance  Pretty (QName l) where
        pretty name = case name of
                UnQual _ (Symbol _ ('#':_)) -> char '(' <+> ppQName name <+> char ')'
                _ -> parensIf (isSymbolQName name) (ppQName name)

ppQName :: QName l -> Doc
ppQName (UnQual _ name) = ppName name
ppQName (Qual _ m name) = pretty m <> char '.' <> ppName name
ppQName (Special _ sym) = pretty sym

instance  Pretty (Op l) where
        pretty (VarOp _ n) = ppNameInfix n
        pretty (ConOp _ n) = ppNameInfix n

ppNameInfix :: Name l -> Doc
ppNameInfix name
        | isSymbolName name = ppName name
        | otherwise = char '`' <> ppName name <> char '`'

instance  Pretty (Name l) where
        pretty name = case name of
                Symbol _ ('#':_) -> char '(' <+> ppName name <+> char ')'
                _ -> parensIf (isSymbolName name) (ppName name)

ppName :: Name l -> Doc
ppName (Ident _ s) = text s
ppName (Symbol _ s) = text s

instance  Pretty (IPName l) where
        pretty (IPDup _ s) = char '?' <> text s
        pretty (IPLin _ s) = char '%' <> text s

instance  PrettyDeclLike (IPBind l) where
  wantsBlankline _ = False

instance  Pretty (IPBind l) where
        pretty (IPBind _loc ipname exp) =
                myFsep [pretty ipname, equals, pretty exp]

instance  Pretty (CName l) where
        pretty (VarName _ n) = pretty n
        pretty (ConName _ n) = pretty n

instance Pretty (SpecialCon l) where
        pretty (UnitCon {})         = text "()"
        pretty (ListCon {})         = text "[]"
        pretty (FunCon  {})         = text "->"
        pretty (TupleCon _ b n)   = listFun $ foldr (<>) empty (replicate (n-1) comma)
          where listFun = if b == Unboxed then hashParens else parens
        pretty (Cons {})             = text ":"
        pretty (UnboxedSingleCon {}) = text "(# #)"

isSymbolName :: Name l -> Bool
isSymbolName (Symbol {}) = True
isSymbolName _ = False

isSymbolQName :: QName l -> Bool
isSymbolQName (UnQual _ n)       = isSymbolName n
isSymbolQName (Qual _ _ n)       = isSymbolName n
isSymbolQName (Special _ (Cons {}))   = True
isSymbolQName (Special _ (FunCon {})) = True
isSymbolQName _                  = False

getSpecialName :: QName l -> Maybe (SpecialCon l)
getSpecialName (Special _ n) = Just n
getSpecialName _           = Nothing

-- Contexts are "sets" of assertions. Several members really means it's a
-- CxTuple, but we can't represent that in our list of assertions.
-- Therefore: print single member contexts without parenthesis, and treat
--            larger contexts as tuples.
instance (Pretty (Context l)) where
  pretty (CxEmpty _)      = text "()" <+> text "=>"
  pretty (CxSingle _ ctxt)  = pretty ctxt <+> text "=>"
  pretty (CxTuple _ context) = mySep [parenList (map pretty context), text "=>"]

-- hacked for multi-parameter type classes
instance  Pretty (Asst l) where
        pretty (ClassA _ a ts)   = myFsep $ pretty a : map ppAType ts
        pretty (AppA _ n ns)     = myFsep $ pretty n : map pretty ns
        pretty (InfixA _ a op b) = myFsep [pretty a, ppQNameInfix op, pretty b]
        pretty (IParam _ i t)    = myFsep [pretty i, text "::", pretty t]
        pretty (EqualP _ t1 t2)  = myFsep [pretty t1, text "~", pretty t2]
        pretty (ParenA _ a)      = parens (pretty a)
        pretty (WildCardA _ mn)  = char  '_' <> maybePP pretty mn

-- Pretty print a source location, useful for printing out error messages
instance Pretty SrcLoc where
  pretty srcLoc =
    return $ P.hcat [ colonFollow (P.text $ srcFilename srcLoc)
                    , colonFollow (P.int  $ srcLine     srcLoc)
                    , P.int $ srcColumn srcLoc
                    ]

colonFollow :: P.Doc -> P.Doc
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
instance Pretty (Module pos) where
        pretty (Module _ mbHead os imp decls) =
                myVcat $ map pretty os ++
                    (case mbHead of
                        Nothing -> id
                        Just h  -> \x -> [topLevel (pretty h) x])
                    (map pretty imp ++
                         ppDecls (isJust mbHead ||
                                  not (null imp) ||
                                  not (null os))
                           decls)
        pretty (XmlPage _ _mn os n attrs mattr cs) =
                myVcat $ map pretty os ++
                    [let ax = maybe [] (return . pretty) mattr
                      in hcat $
                         (myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [char '>']):
                            map pretty cs ++ [myFsep [text "</" <> pretty n, char '>']]]
        pretty (XmlHybrid _ mbHead os imp decls n attrs mattr cs) =
                myVcat $ map pretty os ++ [text "<%"] ++
                    (case mbHead of
                        Nothing -> id
                        Just h  -> \x -> [topLevel (pretty h) x])
                    (map pretty imp ++
                      ppDecls (isJust mbHead || not (null imp) || not (null os)) decls ++
                        [let ax = maybe [] (return . pretty) mattr
                          in hcat $
                             (myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [char '>']):
                                map pretty cs ++ [myFsep [text "</" <> pretty n, char '>']]])



------------------------- pp utils -------------------------
maybePP :: (a -> Doc) -> Maybe a -> Doc
maybePP _  Nothing = empty
maybePP pp (Just a) = pp a

parenList :: [Doc] -> Doc
parenList = parens . myFsepSimple . punctuate comma

hashParenList :: [Doc] -> Doc
hashParenList = hashParens . myFsepSimple . punctuate comma

hashParens :: Doc -> Doc
hashParens = parens . hashes
  where
    hashes doc = char '#' <+> doc <+> char '#'

braceList :: [Doc] -> Doc
braceList = braces . myFsepSimple . punctuate comma

bracketList :: [Doc] -> Doc
bracketList = brackets . myFsepSimple

bracketColonList :: [Doc] -> Doc
bracketColonList = bracketColons . myFsepSimple
    where bracketColons = brackets . colons
          colons doc = char ':' <> doc <> char ':'

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
                              then text "" $+$ dl else dl}
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

-- | Indent without braces. Useful for deriving clauses etc.
ppIndent :: (PPHsMode -> Int) -> [Doc] -> Doc
ppIndent f dl = do
            i <- fmap f getPPEnv
            nest i . vcat $ dl

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

--------------------------------------------------------------------------------
-- Pretty-printing of internal constructs, for error messages while parsing

instance SrcInfo loc => Pretty (P.PExp loc) where
        pretty (P.Lit _ l) = pretty l
        pretty (P.InfixApp _ a op b) = myFsep [pretty a, pretty op, pretty b]
        pretty (P.NegApp _ e) = myFsep [char '-', pretty e]
        pretty (P.App _ a b) = myFsep [pretty a, pretty b]
        pretty (P.Lambda _loc expList ppBody') = myFsep $
                char '\\' : map pretty expList ++ [text "->", pretty ppBody']
        pretty (P.Let _ (BDecls _ declList) letBody) =
                ppLetExp declList letBody
        pretty (P.Let _ (IPBinds _ bindList) letBody) =
                ppLetExp bindList letBody
        pretty (P.If _ cond thenexp elsexp) =
                myFsep [text "if", pretty cond,
                        text "then", pretty thenexp,
                        text "else", pretty elsexp]
        pretty (P.MultiIf _ alts) =
                text "if"
                $$$ ppBody caseIndent (map pretty alts)
        pretty (P.Case _ cond altList) =
                myFsep [text "case", pretty cond, text "of"]
                $$$ ppBody caseIndent (map pretty altList)
        pretty (P.Do _ stmtList) =
                text "do" $$$ ppBody doIndent (map pretty stmtList)
        pretty (P.MDo _ stmtList) =
                text "mdo" $$$ ppBody doIndent (map pretty stmtList)
        pretty (P.Var _ name) = pretty name
        pretty (P.OverloadedLabel _ name) = text name
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
        pretty (P.ParArray _ arr) =
                bracketColonList . punctuate comma . map pretty $ arr
        pretty (P.EnumFrom _ e) =
                bracketList [pretty e, text ".."]
        pretty (P.EnumFromTo _ from to) =
                bracketList [pretty from, text "..", pretty to]
        pretty (P.EnumFromThen _ from thenE) =
                bracketList [pretty from <> comma, pretty thenE, text ".."]
        pretty (P.EnumFromThenTo _ from thenE to) =
                bracketList [pretty from <> comma, pretty thenE,
                             text "..", pretty to]
        pretty (P.ParArrayFromTo _ from to) =
                bracketColonList [pretty from, text "..", pretty to]
        pretty (P.ParArrayFromThenTo _ from thenE to) =
                bracketColonList [pretty from <> comma, pretty thenE,
                             text "..", pretty to]
        pretty (P.ParComp _ e qualLists) =
                bracketList (intersperse (char '|') $
                                pretty e : (punctuate comma . concatMap (map pretty) $ qualLists))
        pretty (P.ParArrayComp _ e qualArrs) =
                bracketColonList (intersperse (char '|') $
                                pretty e : (punctuate comma . concatMap (map pretty) $ qualArrs))
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
                        map pretty cs ++ [myFsep [text "</" <> pretty n, char '>']]
        pretty (P.XETag _ n attrs mattr) =
                let ax = maybe [] (return . pretty) mattr
                 in myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [text "/>"]
        pretty (P.XPcdata _ s) = text s
        pretty (P.XExpTag _ e) =
                myFsep [text "<%", pretty e, text "%>"]
        pretty (P.XChildTag _ es) =
                myFsep $ text "<%>" : map pretty es ++ [text "</%>"]
        pretty (P.CorePragma _ s e) = myFsep $ map text ["{-# CORE", show s, "#-}"] ++ [pretty e]
        pretty (P.SCCPragma  _ s e) = myFsep $ map text ["{-# SCC",  show s, "#-}"] ++ [pretty e]
        pretty (P.GenPragma  _ s (a,b) (c,d) e) =
                myFsep [text "{-# GENERATED", text $ show s,
                            int a, char ':', int b, char '-',
                            int c, char ':', int d, text "#-}", pretty e]
        pretty (P.Proc _ p e) = myFsep [text "proc", pretty p, text "->", pretty e]
        pretty (P.LeftArrApp _ l r)      = myFsep [pretty l, text "-<",  pretty r]
        pretty (P.RightArrApp _ l r)     = myFsep [pretty l, text ">-",  pretty r]
        pretty (P.LeftArrHighApp _ l r)  = myFsep [pretty l, text "-<<", pretty r]
        pretty (P.RightArrHighApp _ l r) = myFsep [pretty l, text ">>-", pretty r]
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
        pretty (P.SeqRP _ rs) =
            myFsep $ text "(|" : (punctuate comma . map pretty $ rs) ++ [text "|)"]
        pretty (P.GuardRP _ r gs) =
                myFsep $ text "(|" : pretty r : char '|' :
                           (punctuate comma . map pretty $ gs) ++ [text "|)"]
        pretty (P.EitherRP _ r1 r2) = parens . myFsep $ [pretty r1, char '|', pretty r2]
        pretty (P.CAsRP _ n (P.IrrPat _ e)) =
                myFsep [pretty n <> text "@:", char '~' <> pretty e]
        pretty (P.CAsRP _ n r) = hcat [pretty n, text "@:", pretty r]
        pretty (P.XRPats _ ps) =
                myFsep $ text "<[" : map pretty ps ++ [text "%>"]
        pretty (P.BangPat _ e) = text "!" <> pretty e
        pretty (P.LCase _ altList) = text "\\case" $$$ ppBody caseIndent (map pretty altList)
        pretty (P.TypeApp _ ty) = char '@' <> pretty ty

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
        pretty (P.CxTuple _ assts) = myFsep [parenList (map pretty assts), text "=>"]

instance SrcInfo loc => Pretty (P.PAsst loc) where
        pretty (P.ClassA _ a ts)   = myFsep $ pretty ( a) : map (prettyPrec prec_atype) ts
        pretty (P.AppA _ n ns)     = myFsep $ pretty n : map (prettyPrec prec_atype) ns
        pretty (P.InfixA _ a op b) = myFsep [pretty a, ppQNameInfix op, pretty b]
        pretty (P.IParam _ i t)    = myFsep [pretty i, text "::", pretty t]
        pretty (P.EqualP _ t1 t2)  = myFsep [pretty t1, text "~", pretty t2]
        pretty (P.ParenA _ a)      = parens (pretty a)
        pretty (P.WildCardA _ mn)  = char '_' <> maybePP pretty mn

instance SrcInfo loc => Pretty (P.PType loc) where
        prettyPrec p (P.TyForall _ mtvs ctxt htype) = parensIf (p > 0) $
                myFsep [ppForall mtvs, maybePP pretty ctxt, pretty htype]
        prettyPrec p (P.TyFun _ a b) = parensIf (p > 0) $
                myFsep [prettyPrec prec_btype a, text "->", pretty b]
        prettyPrec _ (P.TyTuple _ bxd l) =
                let ds = map pretty l
                 in case bxd of
                        Boxed   -> parenList ds
                        Unboxed -> hashParenList ds
        prettyPrec _ (P.TyList _ t)  = brackets $ pretty t
        prettyPrec _ (P.TyParArray _ t) = bracketColonList [pretty t]
        prettyPrec p (P.TyApp _ a b) =
                {-
                | a == list_tycon = brackets $ pretty b         -- special case
                | otherwise = -} parensIf (p > prec_btype) $
                                    myFsep [pretty a, prettyPrec prec_atype b]
        prettyPrec _ (P.TyVar _ name) = pretty name
        prettyPrec _ (P.TyCon _ name) = pretty name
        prettyPrec _ (P.TyParen _ t) = parens (pretty t)
        prettyPrec _ (P.TyPred _ asst) = pretty asst
        prettyPrec _ (P.TyInfix _ a op b) = myFsep [pretty a, ppQNameInfix op, pretty b]
        prettyPrec _ (P.TyKind _ t k) = parens (myFsep [pretty t, text "::", pretty k])
        prettyPrec _ (P.TyPromoted _ p) = pretty p
        prettyPrec _ (P.TySplice _ s) = pretty s
        prettyPrec _ (P.TyBang _ b u t) = pretty u <+> pretty b <> prettyPrec prec_atype t
        prettyPrec _ (P.TyWildCard _ mn) = char '_' <> maybePP pretty mn
        prettyPrec _ (P.TyQuasiQuote _ n qt) = text ("[$" ++ n ++ "|" ++ qt ++ "|]")

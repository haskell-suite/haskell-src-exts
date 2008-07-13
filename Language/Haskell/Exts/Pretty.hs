{-# OPTIONS_GHC -w #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Pretty
-- Original    :  Language.Haskell.Pretty
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
instance Pretty HsModule where
        pretty (HsModule pos m mbExports imp decls) =
                markLine pos $
                topLevel (ppHsModuleHeader m mbExports)
                         (map pretty imp ++ map pretty decls)

--------------------------  Module Header ------------------------------
ppHsModuleHeader :: Module -> Maybe [HsExportSpec] ->  Doc
ppHsModuleHeader m mbExportList = mySep [
        text "module",
        pretty m,
        maybePP (parenList . map pretty) mbExportList,
        text "where"]

instance Pretty Module where
        pretty (Module modName) = text modName

instance Pretty HsExportSpec where
        pretty (HsEVar name)                = pretty name
        pretty (HsEAbs name)                = pretty name
        pretty (HsEThingAll name)           = pretty name <> text "(..)"
        pretty (HsEThingWith name nameList) =
                pretty name <> (parenList . map pretty $ nameList)
        pretty (HsEModuleContents m)        = text "module" <+> pretty m

instance Pretty HsImportDecl where
        pretty (HsImportDecl pos m qual mbName mbSpecs) =
                markLine pos $
                mySep [text "import",
                       if qual then text "qualified" else empty,
                       pretty m,
                       maybePP (\m' -> text "as" <+> pretty m') mbName,
                       maybePP exports mbSpecs]
            where
                exports (b,specList) =
                        if b then text "hiding" <+> specs else specs
                    where specs = parenList . map pretty $ specList

instance Pretty HsImportSpec where
        pretty (HsIVar name)                = pretty name
        pretty (HsIAbs name)                = pretty name
        pretty (HsIThingAll name)           = pretty name <> text "(..)"
        pretty (HsIThingWith name nameList) =
                pretty name <> (parenList . map pretty $ nameList)

-------------------------  Declarations ------------------------------
instance Pretty HsDecl where
        pretty (HsTypeDecl loc name nameList htype) =
                blankline $
                markLine loc $
                mySep ( [text "type", pretty name]
                        ++ map pretty nameList
                        ++ [equals, pretty htype])

        pretty (HsDataDecl loc don context name nameList constrList derives) =
                blankline $
                markLine loc $
                mySep ( [pretty don, ppHsContext context, pretty name]
                        ++ map pretty nameList)
                        <+> (myVcat (zipWith (<+>) (equals : repeat (char '|'))
                                                   (map pretty constrList))
                        $$$ ppHsDeriving derives)

        pretty (HsGDataDecl loc don context name nameList optkind gadtList) =
                blankline $
                markLine loc $
                mySep ( [pretty don, ppHsContext context, pretty name]
                        ++ map pretty nameList ++ ppOptKind optkind ++ [text "where"])
                        $$$ ppBody classIndent (map pretty gadtList)

        pretty (HsTypeFamDecl loc name nameList optkind) =
                blankline $
                markLine loc $
                mySep ([text "type", text "family", pretty name]
                        ++ map pretty nameList
                        ++ ppOptKind optkind)
        
        pretty (HsDataFamDecl loc context name nameList optkind) =
                blankline $
                markLine loc $
                mySep ( [text "data", text "family", ppHsContext context, pretty name]
                        ++ map pretty nameList ++ ppOptKind optkind)

        pretty (HsTypeInsDecl loc ntype htype) =
                blankline $
                markLine loc $
                mySep [text "type", text "instance", pretty ntype, equals, pretty htype]
        
        pretty (HsDataInsDecl loc don ntype constrList derives) =
                blankline $
                markLine loc $
                mySep [pretty don, text "instance", pretty ntype]
                        <+> (myVcat (zipWith (<+>) (equals : repeat (char '|'))
                                                   (map pretty constrList))
                              $$$ ppHsDeriving derives)

        pretty (HsGDataInsDecl loc don ntype optkind gadtList) =
                blankline $
                markLine loc $
                mySep ( [pretty don, text "instance", pretty ntype]
                        ++ ppOptKind optkind ++ [text "where"])
                        $$$ ppBody classIndent (map pretty gadtList)


{-        pretty (HsNewTypeDecl pos context name nameList constr derives) =
                blankline $
                markLine pos $
                mySep ( [text "newtype", ppHsContext context, pretty name]
                        ++ map pretty nameList)
                        <+> equals <+> (pretty constr $$$ ppHsDeriving derives)
-}
        --m{spacing=False}
        -- special case for empty class declaration
        pretty (HsClassDecl pos context name nameList fundeps []) =
                blankline $
                markLine pos $
                mySep ( [text "class", ppHsContext context, pretty name]
                        ++ map pretty nameList ++ [ppFunDeps fundeps])
        pretty (HsClassDecl pos context name nameList fundeps declList) =
                blankline $
                markLine pos $
                mySep ( [text "class", ppHsContext context, pretty name]
                        ++ map pretty nameList ++ [ppFunDeps fundeps, text "where"])
                $$$ ppBody classIndent (map pretty declList)

        -- m{spacing=False}
        -- special case for empty instance declaration
        pretty (HsInstDecl pos context name args []) =
                blankline $
                markLine pos $
                mySep ( [text "instance", ppHsContext context, pretty name]
                        ++ map ppHsAType args)
        pretty (HsInstDecl pos context name args declList) =
                blankline $
                markLine pos $
                mySep ( [text "instance", ppHsContext context, pretty name]
                        ++ map ppHsAType args ++ [text "where"])
                $$$ ppBody classIndent (map pretty declList)

        pretty (HsDerivDecl pos context name args) =
                blankline $
                markLine pos $
                mySep ( [text "deriving", text "instance", ppHsContext context, pretty name]
                        ++ map ppHsAType args)
        pretty (HsDefaultDecl pos htypes) =
                blankline $
                markLine pos $
                text "default" <+> parenList (map pretty htypes)

        pretty (HsSpliceDecl pos splice) =
                blankline $
                markLine pos $
                pretty splice

        pretty (HsTypeSig pos nameList qualType) =
                blankline $
                markLine pos $
                mySep ((punctuate comma . map pretty $ nameList)
                      ++ [text "::", pretty qualType])

        pretty (HsFunBind matches) =
                foldr ($$$) empty (map pretty matches)

        pretty (HsPatBind pos pat rhs whereBinds) =
                markLine pos $
                myFsep [pretty pat, pretty rhs] $$$ ppWhere whereBinds

        pretty (HsInfixDecl pos assoc prec opList) =
                blankline $
                markLine pos $
                mySep ([pretty assoc, int prec]
                       ++ (punctuate comma . map pretty $ opList))

        pretty (HsForImp pos cconv saf str name typ) =
                blankline $
                markLine pos $
                mySep [text "foreign import", pretty cconv, pretty saf,
                       text (show str), pretty name, text "::", pretty typ]

        pretty (HsForExp pos cconv str name typ) =
                blankline $
                markLine pos $
                mySep [text "foreign export", pretty cconv,
                       text (show str), pretty name, text "::", pretty typ]

instance Pretty DataOrNew where
        pretty DataType = text "data"
        pretty NewType  = text "newtype"

instance Pretty HsAssoc where
        pretty HsAssocNone  = text "infix"
        pretty HsAssocLeft  = text "infixl"
        pretty HsAssocRight = text "infixr"

instance Pretty HsMatch where
        pretty (HsMatch pos f ps rhs whereBinds) =
                markLine pos $
                myFsep (lhs ++ [pretty rhs])
                $$$ ppWhere whereBinds
            where
                lhs = case ps of
                        l:r:ps' | isSymbolName f ->
                                let hd = [pretty l, ppHsName f, pretty r] in
                                if null ps' then hd
                                else parens (myFsep hd) : map (prettyPrec 2) ps'
                        _ -> pretty f : map (prettyPrec 2) ps

ppWhere :: HsBinds -> Doc
ppWhere (HsBDecls []) = empty
ppWhere (HsBDecls l)  = nest 2 (text "where" $$$ ppBody whereIndent (map pretty l))
ppWhere (HsIPBinds b) = nest 2 (text "where" $$$ ppBody whereIndent (map pretty b))


instance Pretty HsClassDecl where
    pretty (HsClsDecl decl) = pretty decl

    pretty (HsClsDataFam loc context name nameList optkind) =
                markLine loc $
                mySep ( [text "data", ppHsContext context, pretty name]
                        ++ map pretty nameList ++ ppOptKind optkind)

    pretty (HsClsTyFam loc name nameList optkind) =
                markLine loc $
                mySep ( [text "type", pretty name]
                        ++ map pretty nameList ++ ppOptKind optkind)
    
    pretty (HsClsTyDef loc ntype htype) =
                markLine loc $
                mySep [text "type", pretty ntype, equals, pretty htype] 

instance Pretty HsInstDecl where
        pretty (HsInsDecl decl) = pretty decl

        pretty (HsInsType loc ntype htype) =
                markLine loc $
                mySep [text "type", pretty ntype, equals, pretty htype]

        pretty (HsInsData loc don ntype constrList derives) =
                markLine loc $
                mySep [pretty don, pretty ntype]
                        <+> (myVcat (zipWith (<+>) (equals : repeat (char '|'))
                                                   (map pretty constrList))
                              $$$ ppHsDeriving derives)

        pretty (HsInsGData loc don ntype optkind gadtList) =
                markLine loc $
                mySep ( [pretty don, pretty ntype]
                        ++ ppOptKind optkind ++ [text "where"])
                        $$$ ppBody classIndent (map pretty gadtList)


------------------------- FFI stuff -------------------------------------
instance Pretty HsSafety where
        pretty PlayRisky        = text "unsafe"
        pretty (PlaySafe b)     = text $ if b then "threadsafe" else "safe"

instance Pretty HsCallConv where
        pretty StdCall  = text "stdcall"
        pretty CCall    = text "ccall"

------------------------- Data & Newtype Bodies -------------------------
instance Pretty HsQualConDecl where
        pretty (HsQualConDecl _pos tvs ctxt con) =
                myFsep [ppForall (Just tvs), ppHsContext ctxt, pretty con]

instance Pretty HsGadtDecl where
        pretty (HsGadtDecl _pos name ty) =
                myFsep [pretty name, text "::", pretty ty]

instance Pretty HsConDecl where
        pretty (HsRecDecl name fieldList) =
                pretty name <> (braceList . map ppField $ fieldList)

        pretty (HsConDecl name@(HsSymbol _) [l, r]) =
                myFsep [prettyPrec prec_btype l, ppHsName name,
                        prettyPrec prec_btype r]
        pretty (HsConDecl name typeList) =
                mySep $ ppHsName name : map (prettyPrec prec_atype) typeList

ppField :: ([HsName],HsBangType) -> Doc
ppField (names, ty) =
        myFsepSimple $ (punctuate comma . map pretty $ names) ++
                       [text "::", pretty ty]

instance Pretty HsBangType where
        prettyPrec _ (HsBangedTy ty) = char '!' <> ppHsAType ty
        prettyPrec p (HsUnBangedTy ty) = prettyPrec p ty

ppHsDeriving :: [HsQName] -> Doc
ppHsDeriving []  = empty
ppHsDeriving [d] = text "deriving" <+> ppHsQName d
ppHsDeriving ds  = text "deriving" <+> parenList (map ppHsQName ds)

------------------------- Types -------------------------
{-
instance Pretty HsQualType where
        pretty (HsQualType context htype) =
                myFsep [ppHsContext context, pretty htype]
-}
ppHsBType :: HsType -> Doc
ppHsBType = prettyPrec prec_btype

ppHsAType :: HsType -> Doc
ppHsAType = prettyPrec prec_atype

-- precedences for types
prec_btype, prec_atype :: Int
prec_btype = 1  -- left argument of ->,
                -- or either argument of an infix data constructor
prec_atype = 2  -- argument of type or data constructor, or of a class

instance Pretty HsType where
        prettyPrec p (HsTyForall mtvs ctxt htype) = parensIf (p > 0) $
                myFsep [ppForall mtvs, ppHsContext ctxt, pretty htype]
        prettyPrec p (HsTyFun a b) = parensIf (p > 0) $
                myFsep [ppHsBType a, text "->", pretty b]
        prettyPrec _ (HsTyTuple bxd l) =
                let ds = map pretty l
                 in case bxd of
                        Boxed   -> parenList ds
                        Unboxed -> hashParenList ds
        prettyPrec p (HsTyApp a b)
                | a == list_tycon = brackets $ pretty b         -- special case
                | otherwise = parensIf (p > prec_btype) $
                        myFsep [pretty a, ppHsAType b]
        prettyPrec _ (HsTyVar name) = pretty name
        prettyPrec _ (HsTyCon name) = pretty name
        prettyPrec _ (HsTyPred asst) = pretty asst
        prettyPrec _ (HsTyInfix a op b) = parens (myFsep [pretty op, pretty a, pretty b])
        prettyPrec _ (HsTyKind t k) = parens (myFsep [pretty t, text "::", pretty k])


instance Pretty HsTyVarBind where
        pretty (HsKindedVar var kind) = myFsep [pretty var, text "::", pretty kind]
        pretty (HsUnkindedVar var)    = pretty var

ppForall :: Maybe [HsTyVarBind] -> Doc
ppForall Nothing   = empty
ppForall (Just []) = empty
ppForall (Just vs) =    myFsep (text "forall" : map pretty vs ++ [char '.'])

---------------------------- Kinds ----------------------------

instance Pretty HsKind where
        pretty HsKindStar     = text "*"
        pretty HsKindBang     = text "!"
        pretty (HsKindFn a b) = myFsep [pretty a, text "->", pretty b]

ppOptKind :: Maybe HsKind -> [Doc]
ppOptKind Nothing = []
ppOptKind (Just k) = [text "::", pretty k]

------------------- Functional Dependencies -------------------
instance Pretty HsFunDep where
        pretty (HsFunDep from to) =
                myFsep $ map pretty from ++ [text "->"] ++ map pretty to


ppFunDeps :: [HsFunDep] -> Doc
ppFunDeps [] = empty
ppFunDeps fds = myFsep $ (char '|':) . punctuate comma . map pretty $ fds

------------------------- Expressions -------------------------
instance Pretty HsRhs where
        pretty (HsUnGuardedRhs e) = equals <+> pretty e
        pretty (HsGuardedRhss guardList) = myVcat . map pretty $ guardList

instance Pretty HsGuardedRhs where
        pretty (HsGuardedRhs _pos guards ppBody) =
                myFsep $ [char '|'] ++ (punctuate comma . map pretty $ guards) ++ [equals, pretty ppBody]

instance Pretty HsLiteral where
        pretty (HsInt i)        = integer i
        pretty (HsChar c)       = text (show c)
        pretty (HsString s)     = text (show s)
        pretty (HsFrac r)       = double (fromRational r)
        -- GHC unboxed literals:
        pretty (HsCharPrim c)   = text (show c)           <> char '#'
        pretty (HsStringPrim s) = text (show s)           <> char '#'
        pretty (HsIntPrim i)    = integer i               <> char '#'
        pretty (HsFloatPrim r)  = float  (fromRational r) <> char '#'
        pretty (HsDoublePrim r) = double (fromRational r) <> text "##"

instance Pretty HsExp where
        pretty (HsLit l) = pretty l
        -- lambda stuff
        pretty (HsInfixApp a op b) = myFsep [pretty a, pretty op, pretty b]
        pretty (HsNegApp e) = myFsep [char '-', pretty e]
        pretty (HsApp a b) = myFsep [pretty a, pretty b]
        pretty (HsLambda _loc expList ppBody) = myFsep $
                char '\\' : map pretty expList ++ [text "->", pretty ppBody]
        -- keywords
        -- two cases for lets
        pretty (HsLet (HsBDecls declList) letBody) =
                ppLetExp declList letBody
        pretty (HsLet (HsIPBinds bindList) letBody) =
                ppLetExp bindList letBody
        pretty (HsDLet bindList letBody) =
                myFsep [text "dlet" <+> ppBody letIndent (map pretty bindList),
                        text "in", pretty letBody]
        pretty (HsWith exp bindList) =
                pretty exp $$$ ppWith bindList
        pretty (HsIf cond thenexp elsexp) =
                myFsep [text "if", pretty cond,
                        text "then", pretty thenexp,
                        text "else", pretty elsexp]
        pretty (HsCase cond altList) =
                myFsep [text "case", pretty cond, text "of"]
                $$$ ppBody caseIndent (map pretty altList)
        pretty (HsDo stmtList) =
                text "do" $$$ ppBody doIndent (map pretty stmtList)
        pretty (HsMDo stmtList) =
                text "mdo" $$$ ppBody doIndent (map pretty stmtList)
        -- Constructors & Vars
        pretty (HsVar name) = pretty name
        pretty (HsIPVar ipname) = pretty ipname
        pretty (HsCon name) = pretty name
        pretty (HsTuple expList) = parenList . map pretty $ expList
        -- weird stuff
        pretty (HsParen e) = parens . pretty $ e
        pretty (HsLeftSection e op) = parens (pretty e <+> pretty op)
        pretty (HsRightSection op e) = parens (pretty op <+> pretty e)
        pretty (HsRecConstr c fieldList) =
                pretty c <> (braceList . map pretty $ fieldList)
        pretty (HsRecUpdate e fieldList) =
                pretty e <> (braceList . map pretty $ fieldList)
        -- patterns
        -- special case that would otherwise be buggy
        pretty (HsAsPat name (HsIrrPat e)) =
                myFsep [pretty name <> char '@', char '~' <> pretty e]
        pretty (HsAsPat name e) = hcat [pretty name, char '@', pretty e]
        pretty HsWildCard = char '_'
        pretty (HsIrrPat e) = char '~' <> pretty e
        -- Lists
        pretty (HsList list) =
                bracketList . punctuate comma . map pretty $ list
        pretty (HsEnumFrom e) =
                bracketList [pretty e, text ".."]
        pretty (HsEnumFromTo from to) =
                bracketList [pretty from, text "..", pretty to]
        pretty (HsEnumFromThen from thenE) =
                bracketList [pretty from <> comma, pretty thenE, text ".."]
        pretty (HsEnumFromThenTo from thenE to) =
                bracketList [pretty from <> comma, pretty thenE,
                             text "..", pretty to]
        pretty (HsListComp e stmtList) =
                bracketList ([pretty e, char '|']
                             ++ (punctuate comma . map pretty $ stmtList))
        pretty (HsExpTypeSig _pos e ty) =
                myFsep [pretty e, text "::", pretty ty]
        -- Template Haskell
--        pretty (HsReifyExp r) = pretty r
        pretty (HsBracketExp b) = pretty b
        pretty (HsSpliceExp s) = pretty s
        pretty (HsTypQuote t)  = text "\'\'" <> pretty t
        pretty (HsVarQuote x)  = text "\'" <> pretty x
        -- regular patterns
        pretty (HsSeqRP rs) =
                myFsep $ text "(/" : map pretty rs ++ [text "/)"]
        pretty (HsEitherRP r1 r2) = parens . myFsep $
                [pretty r1, char '|', pretty r2]
        pretty (HsGuardRP r gs) = 
                myFsep $ text "(|" : pretty r : char '|' : map pretty gs ++ [text "|)"]
        -- special case that would otherwise be buggy
        pretty (HsCAsRP n (HsIrrPat e)) =
                myFsep [pretty n <> text "@:", char '~' <> pretty e]
        pretty (HsCAsRP n r) = hcat [pretty n, text "@:", pretty r]
        -- Hsx
        pretty (HsXTag _ n attrs mattr cs) =
                let ax = maybe [] (return . pretty) mattr
                 in hcat $
                     (myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [char '>']):
                        map pretty cs ++ [myFsep $ [text "</" <> pretty n, char '>']]
        pretty (HsXETag _ n attrs mattr) =
                let ax = maybe [] (return . pretty) mattr
                 in myFsep $ (char '<' <> pretty n): map pretty attrs ++ ax ++ [text "/>"]
        pretty (HsXPcdata s) = text s
        pretty (HsXExpTag e) =
                myFsep $ [text "<%", pretty e, text "%>"]
        pretty (HsXRPats es) =
                myFsep $ text "<[" : map pretty es ++ [text "]>"]

instance Pretty HsXAttr where
        pretty (HsXAttr n v) =
                myFsep [pretty n, char '=', pretty v]

instance Pretty HsXName where
        pretty (HsXName n) = text n
        pretty (HsXDomName d n) = text d <> char ':' <> text n

--ppLetExp :: [HsDecl] -> HsExp -> Doc
ppLetExp l b = myFsep [text "let" <+> ppBody letIndent (map pretty l),
                        text "in", pretty b]

ppWith binds = nest 2 (text "with" $$$ ppBody withIndent (map pretty binds))
withIndent = whereIndent

--------------------- Template Haskell -------------------------
{-
instance Pretty HsReify where
        pretty (HsReifyDecl name) = ppReify "reifyDecl" name
        pretty (HsReifyType name) = ppReify "reifyType" name
        pretty (HsReifyFixity name) = ppReify "reifyFixity" name

ppReify t n = myFsep [text t, pretty n]
--}

instance Pretty HsBracket where
        pretty (HsExpBracket e) = ppBracket "[|" e
        pretty (HsPatBracket p) = ppBracket "[p|" p
        pretty (HsTypeBracket t) = ppBracket "[t|" t
        pretty (HsDeclBracket d) =
                myFsep $ text "[d|" : map pretty d ++ [text "|]"]

ppBracket o x = myFsep [text o, pretty x, text "|]"]

instance Pretty HsSplice where
        pretty (HsIdSplice s) = char '$' <> text s
        pretty (HsParenSplice e) =
                myFsep [text "$(", pretty e, char ')']

------------------------- Patterns -----------------------------

instance Pretty HsPat where
        prettyPrec _ (HsPVar name) = pretty name
        prettyPrec _ (HsPLit lit) = pretty lit
        prettyPrec _ (HsPNeg p) = myFsep [char '-', pretty p]
        prettyPrec p (HsPInfixApp a op b) = parensIf (p > 0) $
                myFsep [pretty a, pretty (HsQConOp op), pretty b]
        prettyPrec p (HsPApp n ps) = parensIf (p > 1) $
                myFsep (pretty n : map pretty ps)
        prettyPrec _ (HsPTuple ps) = parenList . map pretty $ ps
        prettyPrec _ (HsPList ps) =
                bracketList . punctuate comma . map pretty $ ps
        prettyPrec _ (HsPParen p) = parens . pretty $ p
        prettyPrec _ (HsPRec c fields) =
                pretty c <> (braceList . map pretty $ fields)
        -- special case that would otherwise be buggy
        prettyPrec _ (HsPAsPat name (HsPIrrPat pat)) =
                myFsep [pretty name <> char '@', char '~' <> pretty pat]
        prettyPrec _ (HsPAsPat name pat) =
                hcat [pretty name, char '@', pretty pat]
        prettyPrec _ HsPWildCard = char '_'
        prettyPrec _ (HsPIrrPat pat) = char '~' <> pretty pat
        prettyPrec _ (HsPatTypeSig _pos pat ty) =
                myFsep [pretty pat, text "::", pretty ty]

        -- HaRP
        prettyPrec _ (HsPRPat rs) = 
                bracketList . punctuate comma . map pretty $ rs
        -- Hsx
        prettyPrec _ (HsPXTag _ n attrs mattr cp) =
            let ap = maybe [] (return . pretty) mattr
             in hcat $ -- TODO: should not introduce blanks
                  (myFsep $ (char '<' <> pretty n): map pretty attrs ++ ap ++ [char '>']):
                    map pretty cp ++ [myFsep $ [text "</" <> pretty n, char '>']]
        prettyPrec _ (HsPXETag _ n attrs mattr) =
                let ap = maybe [] (return . pretty) mattr
                 in myFsep $ (char '<' <> pretty n): map pretty attrs ++ ap ++ [text "/>"]
        prettyPrec _ (HsPXPcdata s) = text s
        prettyPrec _ (HsPXPatTag p) =
                myFsep $ [text "<%", pretty p, text "%>"]
        prettyPrec _ (HsPXRPats ps) =
                myFsep $ text "<[" : map pretty ps ++ [text "%>"]

{-
prettyChildren :: HsPat -> [Doc]
prettyChildren p = case p of
        HsPList ps  -> map prettyChild ps
        HsPRPat _ _ -> [pretty p]
        _           -> error "The pattern representing the children of an xml pattern \
                                \ should always be a list."

prettyChild :: HsPat -> Doc
prettyChild p = case p of
        HsPXTag _ _ _ _ _ -> pretty p
        HsPXETag _ _ _ _  -> pretty p
        HsPXPatTag _      -> pretty p
        HsPXPcdata _      -> pretty p
        _                 -> pretty $ HsPXPatTag p
-}

instance Pretty HsPXAttr where
        pretty (HsPXAttr n p) =
                myFsep [pretty n, char '=', pretty p]

instance Pretty HsPatField where
        pretty (HsPFieldPat name pat) =
                myFsep [pretty name, equals, pretty pat]

--------------------- Regular Patterns -------------------------

instance Pretty HsRPat where
        pretty (HsRPOp r op) = pretty r <> pretty op
        pretty (HsRPEither r1 r2) = parens . myFsep $
                [pretty r1, char '|', pretty r2]
        pretty (HsRPSeq rs) =
                myFsep $ text "(/" : map pretty rs ++ [text "/)"]
        pretty (HsRPGuard r gs) =
                myFsep $ text "(|" : pretty r : char '|' : map pretty gs ++ [text "|)"]
        -- special case that would otherwise be buggy
        pretty (HsRPCAs n (HsRPPat (HsPIrrPat p))) =
                myFsep [pretty n <> text "@:", char '~' <> pretty p]
        pretty (HsRPCAs n r) = hcat [pretty n, text "@:", pretty r]
        -- special case that would otherwise be buggy
        pretty (HsRPAs n (HsRPPat (HsPIrrPat p))) =
                myFsep [pretty n <> text "@:", char '~' <> pretty p]
        pretty (HsRPAs n r) = hcat [pretty n, char '@', pretty r]
        pretty (HsRPPat p) = pretty p
        pretty (HsRPParen rp) = parens . pretty $ rp

instance Pretty HsRPatOp where
        pretty HsRPStar  = char '*'
        pretty HsRPStarG = text "*!"
        pretty HsRPPlus  = char '+'
        pretty HsRPPlusG = text "+!"
        pretty HsRPOpt   = char '?'
        pretty HsRPOptG  = text "?!"

------------------------- Case bodies  -------------------------
instance Pretty HsAlt where
        pretty (HsAlt _pos e gAlts binds) =
                pretty e <+> pretty gAlts $$$ ppWhere binds

instance Pretty HsGuardedAlts where
        pretty (HsUnGuardedAlt e) = text "->" <+> pretty e
        pretty (HsGuardedAlts altList) = myVcat . map pretty $ altList

instance Pretty HsGuardedAlt where
        pretty (HsGuardedAlt _pos guards body) =
                myFsep $ char '|': (punctuate comma . map pretty $ guards) ++ [text "->", pretty body]

------------------------- Statements in monads, guards & list comprehensions -----
instance Pretty HsStmt where
        pretty (HsGenerator _loc e from) =
                pretty e <+> text "<-" <+> pretty from
        pretty (HsQualifier e) = pretty e
        -- two cases for lets
        pretty (HsLetStmt (HsBDecls declList)) =
                ppLetStmt declList
        pretty (HsLetStmt (HsIPBinds bindList)) =
                ppLetStmt bindList

ppLetStmt l = text "let" $$$ ppBody letIndent (map pretty l)

------------------------- Record updates
instance Pretty HsFieldUpdate where
        pretty (HsFieldUpdate name e) =
                myFsep [pretty name, equals, pretty e]

------------------------- Names -------------------------
instance Pretty HsQOp where
        pretty (HsQVarOp n) = ppHsQNameInfix n
        pretty (HsQConOp n) = ppHsQNameInfix n

ppHsQNameInfix :: HsQName -> Doc
ppHsQNameInfix name
        | isSymbolName (getName name) = ppHsQName name
        | otherwise = char '`' <> ppHsQName name <> char '`'

instance Pretty HsQName where
        pretty name = case name of
                UnQual (HsSymbol ('#':_)) -> char '(' <+> ppHsQName name <+> char ')'
                _ -> parensIf (isSymbolName (getName name)) (ppHsQName name)

ppHsQName :: HsQName -> Doc
ppHsQName (UnQual name) = ppHsName name
ppHsQName (Qual m name) = pretty m <> char '.' <> ppHsName name
ppHsQName (Special sym) = text (specialName sym)

instance Pretty HsOp where
        pretty (HsVarOp n) = ppHsNameInfix n
        pretty (HsConOp n) = ppHsNameInfix n

ppHsNameInfix :: HsName -> Doc
ppHsNameInfix name
        | isSymbolName name = ppHsName name
        | otherwise = char '`' <> ppHsName name <> char '`'

instance Pretty HsName where
        pretty name = case name of
                HsSymbol ('#':_) -> char '(' <+> ppHsName name <+> char ')'
                _ -> parensIf (isSymbolName name) (ppHsName name)

ppHsName :: HsName -> Doc
ppHsName (HsIdent s) = text s
ppHsName (HsSymbol s) = text s

instance Pretty HsIPName where
        pretty (HsIPDup s) = char '?' <> text s
        pretty (HsIPLin s) = char '%' <> text s

instance Pretty HsIPBind where
        pretty (HsIPBind _loc ipname exp) =
                myFsep [pretty ipname, equals, pretty exp]

instance Pretty HsCName where
        pretty (HsVarName n) = pretty n
        pretty (HsConName n) = pretty n

isSymbolName :: HsName -> Bool
isSymbolName (HsSymbol _) = True
isSymbolName _ = False

getName :: HsQName -> HsName
getName (UnQual s) = s
getName (Qual _ s) = s
getName (Special HsCons) = HsSymbol ":"
getName (Special HsFunCon) = HsSymbol "->"
getName (Special s) = HsIdent (specialName s)

specialName :: HsSpecialCon -> String
specialName HsUnitCon = "()"
specialName HsListCon = "[]"
specialName HsFunCon = "->"
specialName (HsTupleCon n) = "(" ++ replicate (n-1) ',' ++ ")"
specialName HsCons = ":"

ppHsContext :: HsContext -> Doc
ppHsContext []      = empty
ppHsContext context = mySep [parenList (map pretty context), text "=>"]

-- hacked for multi-parameter type classes
instance Pretty HsAsst where
        pretty (HsClassA a ts)  = myFsep $ ppHsQName a : map ppHsAType ts
        pretty (HsIParam i t)   = myFsep $ [pretty i, text "::", pretty t]
        pretty (HsEqualP t1 t2) = myFsep $ [pretty t1, text "~", pretty t2]

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

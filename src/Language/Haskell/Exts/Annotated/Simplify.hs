-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Annotated.Simplify
-- Copyright   :  (c) Niklas Broberg 2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains code for translating from the annotated
-- complex AST in Language.Haskell.Exts.Annotated.Syntax
-- to the simpler, sparsely annotated AST in Language.Haskell.Exts.Syntax.
--
-- A function @sXYZ@ translates an annotated AST node of type @XYZ l@ into
-- a simple AST node of type @XYZ@. I would have prefered to use a MPTC
-- with an fd/type family to get a single exported function name, but
-- I wish to stay Haskell 2010 compliant. Let's hope for Haskell 2011.
--
-----------------------------------------------------------------------------
module Language.Haskell.Exts.Annotated.Simplify where

import Language.Haskell.Exts.Annotated.Syntax
import qualified Language.Haskell.Exts.Syntax as S

import Language.Haskell.Exts.SrcLoc

-- | Translate an annotated AST node representing a Haskell module, into
--   a simpler version that retains (almost) only abstract information.
--   In particular, XML and hybrid XML pages enabled by the XmlSyntax extension
--   are translated into standard Haskell modules with a @page@ function.
sModule :: SrcInfo loc => Module loc -> S.Module
sModule md = case md of
    Module l mmh oss ids ds ->
        let (mn, mwt, mes) = sModuleHead mmh
         in S.Module (getPointLoc l) mn (map sModulePragma oss) mwt mes (map sImportDecl ids) (map sDecl ds)
    XmlPage l mn oss xn attrs mat es   ->
        let loc = getPointLoc l
         in S.Module loc (sModuleName mn) (map sModulePragma oss)
                      Nothing
                      (Just [S.EVar $ S.UnQual $ S.Ident "page"])
                        []
                        [pageFun loc $ S.XTag loc (sXName xn) (map sXAttr attrs) (fmap sExp mat) (map sExp es)]
    XmlHybrid l mmh oss ids ds xn attrs mat es  ->
        let loc1 = getPointLoc l
            loc2 = getPointLoc (ann xn)
            (mn, mwt, mes) = sModuleHead mmh
         in S.Module loc1 mn (map sModulePragma oss) mwt mes (map sImportDecl ids)
                (map sDecl ds ++ [pageFun loc2 $ S.XTag loc2 (sXName xn) (map sXAttr attrs) (fmap sExp mat) (map sExp es)])
  where pageFun :: SrcLoc -> S.Exp -> S.Decl
        pageFun loc e = S.PatBind loc namePat Nothing rhs (S.BDecls [])
            where namePat = S.PVar $ S.Ident "page"
                  rhs = S.UnGuardedRhs e


-- | Translate an annotated AST node representing a Haskell declaration
--   into a simpler version. Note that in the simpler version, all declaration
--   nodes are still annotated by 'SrcLoc's.
sDecl :: SrcInfo loc => Decl loc -> S.Decl
sDecl decl = case decl of
     TypeDecl     l dh t        ->
        let (n, tvs) = sDeclHead dh
         in S.TypeDecl (getPointLoc l) n tvs (sType t)
     TypeFamDecl  l dh mk       ->
        let (n, tvs) = sDeclHead dh
         in S.TypeFamDecl (getPointLoc l) n tvs (fmap sKind mk)
     DataDecl     l dn mctxt dh constrs mder    ->
        let (n, tvs) = sDeclHead dh
         in S.DataDecl (getPointLoc l) (sDataOrNew dn) (maybe [] sContext mctxt) n tvs (map sQualConDecl constrs) (maybe [] sDeriving mder)
     GDataDecl    l dn mctxt dh mk gds mder     ->
        let (n, tvs) = sDeclHead dh
         in S.GDataDecl (getPointLoc l) (sDataOrNew dn) (maybe [] sContext mctxt) n tvs (fmap sKind mk) (map sGadtDecl gds) (maybe [] sDeriving mder)
     DataFamDecl  l mctxt dh mk ->
        let (n, tvs) = sDeclHead dh
         in S.DataFamDecl (getPointLoc l) (maybe [] sContext mctxt) n tvs (fmap sKind mk)
     TypeInsDecl  l t1 t2       -> S.TypeInsDecl (getPointLoc l) (sType t1) (sType t2)
     DataInsDecl  l dn t constrs mder           ->
        S.DataInsDecl (getPointLoc l) (sDataOrNew dn) (sType t) (map sQualConDecl constrs) (maybe [] sDeriving mder)
     GDataInsDecl l dn t mk gds mder            ->
        S.GDataInsDecl (getPointLoc l) (sDataOrNew dn) (sType t) (fmap sKind mk) (map sGadtDecl gds) (maybe [] sDeriving mder)
     ClassDecl    l mctxt dh fds mcds           ->
        let (n, tvs) = sDeclHead dh
         in S.ClassDecl (getPointLoc l) (maybe [] sContext mctxt) n tvs (map sFunDep fds) (maybe [] (map sClassDecl) mcds)
     InstDecl     l mctxt ih mids               ->
        let (qn, ts) = sInstHead ih
         in S.InstDecl (getPointLoc l) (maybe [] sContext mctxt) qn ts (maybe [] (map sInstDecl) mids)
     DerivDecl    l mctxt ih    ->
        let (qn, ts) = sInstHead ih
         in S.DerivDecl (getPointLoc l) (maybe [] sContext mctxt) qn ts
     InfixDecl    l ass prec ops    -> S.InfixDecl (getPointLoc l) (sAssoc ass) (maybe 9 id prec) (map sOp ops)
     DefaultDecl  l ts          -> S.DefaultDecl (getPointLoc l) (map sType ts)
     SpliceDecl   l sp          -> S.SpliceDecl (getPointLoc l) (sExp sp)
     TypeSig      l ns t        -> S.TypeSig (getPointLoc l) (map sName ns) (sType t)
     FunBind      _ ms          -> S.FunBind (map sMatch ms)
     PatBind      l p mt rhs mbs    ->
        S.PatBind (getPointLoc l) (sPat p) (fmap sType mt) (sRhs rhs) (maybe (S.BDecls []) sBinds mbs)
     ForImp       l cc msaf mstr n t    ->
        S.ForImp (getPointLoc l) (sCallConv cc) (maybe (S.PlaySafe False) sSafety msaf) (maybe "" id mstr) (sName n) (sType t)
     ForExp       l cc      mstr n t    ->
        S.ForExp (getPointLoc l) (sCallConv cc) (maybe "" id mstr) (sName n) (sType t)
     RulePragmaDecl   l rs      -> S.RulePragmaDecl (getPointLoc l) (map sRule rs)
     DeprPragmaDecl   l nsstrs  -> S.DeprPragmaDecl (getPointLoc l) (map (\(ns, str) -> (map sName ns, str)) nsstrs)
     WarnPragmaDecl   l nsstrs  -> S.WarnPragmaDecl (getPointLoc l) (map (\(ns, str) -> (map sName ns, str)) nsstrs)
     InlineSig        l b mact qn   -> S.InlineSig (getPointLoc l) b (maybe S.AlwaysActive sActivation mact) (sQName qn)
     InlineConlikeSig l   mact qn   -> S.InlineConlikeSig (getPointLoc l) (maybe S.AlwaysActive sActivation mact) (sQName qn)
     SpecSig          l   mact qn ts   -> S.SpecSig (getPointLoc l) (maybe S.AlwaysActive sActivation mact) (sQName qn) (map sType ts)
     SpecInlineSig    l b mact qn ts    ->
        S.SpecInlineSig (getPointLoc l) b (maybe S.AlwaysActive sActivation mact) (sQName qn) (map sType ts)
     InstSig          l mctxt ih    ->
        let (qn, ts) = sInstHead ih
         in S.InstSig (getPointLoc l) (maybe [] sContext mctxt) qn ts
     AnnPragma        l ann         ->
        S.AnnPragma (getPointLoc l) (sAnnotation ann)

sAnnotation :: SrcInfo loc => Annotation loc -> S.Annotation
sAnnotation ann = case ann of
    Ann       _ n e   -> S.Ann     (sName n) (sExp e)
    TypeAnn   _ n e   -> S.TypeAnn (sName n) (sExp e)
    ModuleAnn _   e   -> S.ModuleAnn         (sExp e)

sModuleName :: ModuleName l -> S.ModuleName
sModuleName (ModuleName _ str)  = S.ModuleName str

sSpecialCon :: SpecialCon l -> S.SpecialCon
sSpecialCon sc = case sc of
    UnitCon _           -> S.UnitCon
    ListCon _           -> S.ListCon
    FunCon  _           -> S.FunCon
    TupleCon _ b k      -> S.TupleCon b k
    Cons _              -> S.Cons
    UnboxedSingleCon _  -> S.UnboxedSingleCon

sQName :: QName l -> S.QName
sQName qn = case qn of
    Qual    _ mn n  -> S.Qual (sModuleName mn) (sName n)
    UnQual  _    n  -> S.UnQual (sName n)
    Special _ sc    -> S.Special (sSpecialCon sc)

sName :: Name l -> S.Name
sName (Ident _ str) = S.Ident str
sName (Symbol _ str) = S.Symbol str

sIPName :: IPName l -> S.IPName
sIPName (IPDup _ str) = S.IPDup str
sIPName (IPLin _ str) = S.IPLin str

sQOp :: QOp l -> S.QOp
sQOp (QVarOp _ qn) = S.QVarOp (sQName qn)
sQOp (QConOp _ qn) = S.QConOp (sQName qn)

sOp :: Op l -> S.Op
sOp (VarOp _ n) = S.VarOp (sName n)
sOp (ConOp _ n) = S.ConOp (sName n)

sCName :: CName l -> S.CName
sCName (VarName _ n) = S.VarName (sName n)
sCName (ConName _ n) = S.ConName (sName n)

sModuleHead :: Maybe (ModuleHead l) -> (S.ModuleName, Maybe (S.WarningText), Maybe [S.ExportSpec])
sModuleHead mmh = case mmh of
    Nothing -> (S.main_mod, Nothing, Just [S.EVar (S.UnQual S.main_name)])
    Just (ModuleHead _ mn mwt mel) -> (sModuleName mn, fmap sWarningText mwt, fmap sExportSpecList mel)

sExportSpecList :: ExportSpecList l -> [S.ExportSpec]
sExportSpecList (ExportSpecList _ ess) = map sExportSpec ess

sExportSpec :: ExportSpec l -> S.ExportSpec
sExportSpec es = case es of
    EVar _ qn           -> S.EVar (sQName qn)
    EAbs _ qn           -> S.EAbs (sQName qn)
    EThingAll _ qn      -> S.EThingAll (sQName qn)
    EThingWith _ qn cns -> S.EThingWith (sQName qn) (map sCName cns)
    EModuleContents _ mn    -> S.EModuleContents (sModuleName mn)

sImportDecl :: SrcInfo loc => ImportDecl loc -> S.ImportDecl
sImportDecl (ImportDecl l mn qu src mpkg as misl) =
    S.ImportDecl (getPointLoc l) (sModuleName mn) qu src mpkg (fmap sModuleName as) (fmap sImportSpecList misl)

sImportSpecList :: ImportSpecList l -> (Bool, [S.ImportSpec])
sImportSpecList (ImportSpecList _ b iss) = (b, map sImportSpec iss)

sImportSpec :: ImportSpec l -> S.ImportSpec
sImportSpec is = case is of
    IVar _ n            -> S.IVar (sName n)
    IAbs _ n            -> S.IAbs (sName n)
    IThingAll _ n       -> S.IThingAll (sName n)
    IThingWith _ n cns  -> S.IThingWith (sName n) (map sCName cns)

sAssoc :: Assoc l -> S.Assoc
sAssoc a = case a of
    AssocNone  _ -> S.AssocNone
    AssocLeft  _ -> S.AssocLeft
    AssocRight _ -> S.AssocRight

sDeclHead :: DeclHead l -> (S.Name, [S.TyVarBind])
sDeclHead dh = case dh of
    DHead _ n tvs       -> (sName n, map sTyVarBind tvs)
    DHInfix _ tva n tvb -> (sName n, map sTyVarBind [tva,tvb])
    DHParen _ dh        -> sDeclHead dh

sInstHead :: InstHead l -> (S.QName, [S.Type])
sInstHead ih = case ih of
    IHead _ qn ts      -> (sQName qn, map sType ts)
    IHInfix _ ta qn tb -> (sQName qn, map sType [ta,tb])
    IHParen _ ih       -> sInstHead ih

sDataOrNew :: DataOrNew l -> S.DataOrNew
sDataOrNew (DataType _) = S.DataType
sDataOrNew (NewType _) = S.NewType

sDeriving :: (Deriving l) -> [(S.QName, [S.Type])]
sDeriving (Deriving _ ihs) = map sInstHead ihs

sBinds :: SrcInfo loc => Binds loc -> S.Binds
sBinds bs = case bs of
    BDecls  _ decls     -> S.BDecls (map sDecl decls)
    IPBinds _ ipbds     -> S.IPBinds (map sIPBind ipbds)

sIPBind :: SrcInfo loc => IPBind loc -> S.IPBind
sIPBind (IPBind l ipn e) = S.IPBind (getPointLoc l) (sIPName ipn) (sExp e)

sMatch :: SrcInfo loc => Match loc -> S.Match
sMatch (Match l n ps rhs mwhere) =
    S.Match (getPointLoc l) (sName n) (map sPat ps) Nothing (sRhs rhs) (maybe (S.BDecls []) sBinds mwhere)
sMatch (InfixMatch l pa n pbs rhs mwhere) =
    S.Match (getPointLoc l) (sName n) (map sPat (pa:pbs)) Nothing (sRhs rhs) (maybe (S.BDecls []) sBinds mwhere)

sQualConDecl :: SrcInfo loc => QualConDecl loc -> S.QualConDecl
sQualConDecl (QualConDecl l mtvs mctxt cd) =
    S.QualConDecl (getPointLoc l) (maybe [] (map sTyVarBind) mtvs) (maybe [] sContext mctxt) (sConDecl cd)

sConDecl :: ConDecl l -> S.ConDecl
sConDecl cd = case cd of
    ConDecl _ n bts     -> S.ConDecl (sName n) (map sBangType bts)
    InfixConDecl _ bta n btb -> S.InfixConDecl (sBangType bta) (sName n) (sBangType btb)
    RecDecl _ n fds -> S.RecDecl (sName n) (map sFieldDecl fds)

sFieldDecl :: FieldDecl l -> ([S.Name], S.BangType)
sFieldDecl (FieldDecl _ ns bt) = (map sName ns, sBangType bt)

sGadtDecl :: SrcInfo loc => GadtDecl loc -> S.GadtDecl
sGadtDecl (GadtDecl l n t) = S.GadtDecl (getPointLoc l) (sName n) (sType t)

sClassDecl :: SrcInfo loc => ClassDecl loc -> S.ClassDecl
sClassDecl cd = case cd of
    ClsDecl _ d  -> S.ClsDecl (sDecl d)
    ClsDataFam l mctxt dh mk    ->
        let (n, tvs) = sDeclHead dh
         in S.ClsDataFam (getPointLoc l) (maybe [] sContext mctxt) n tvs (fmap sKind mk)
    ClsTyFam l dh mk    ->
        let (n, tvs) = sDeclHead dh
         in S.ClsTyFam (getPointLoc l) n tvs (fmap sKind mk)
    ClsTyDef l t1 t2    ->
        S.ClsTyDef (getPointLoc l) (sType t1) (sType t2)

sInstDecl :: SrcInfo loc => InstDecl loc -> S.InstDecl
sInstDecl id = case id of
    InsDecl   _ d   -> S.InsDecl (sDecl d)
    InsType   l t1 t2   -> S.InsType (getPointLoc l) (sType t1) (sType t2)
    InsData   l dn t constrs mder   ->
        S.InsData (getPointLoc l) (sDataOrNew dn) (sType t) (map sQualConDecl constrs) (maybe [] sDeriving mder)
    InsGData  l dn t mk gds mder    ->
        S.InsGData (getPointLoc l) (sDataOrNew dn) (sType t) (fmap sKind mk) (map sGadtDecl gds) (maybe [] sDeriving mder)
--    InsInline l b mact qn   -> S.InsInline (getPointLoc l) b (maybe S.AlwaysActive sActivation mact) (sQName qn)

sBangType :: BangType l -> S.BangType
sBangType bt = case bt of
    BangedTy   _ t  -> S.BangedTy (sType t)
    UnBangedTy _ t  -> S.UnBangedTy (sType t)
    UnpackedTy _ t  -> S.UnpackedTy (sType t)

sRhs :: SrcInfo loc => Rhs loc -> S.Rhs
sRhs (UnGuardedRhs _ e) = S.UnGuardedRhs (sExp e)
sRhs (GuardedRhss _ grhss) = S.GuardedRhss (map sGuardedRhs grhss)

sGuardedRhs :: SrcInfo loc => GuardedRhs loc -> S.GuardedRhs
sGuardedRhs (GuardedRhs l ss e) = S.GuardedRhs (getPointLoc l) (map sStmt ss) (sExp e)

sType :: Type l -> S.Type
sType t = case t of
    TyForall _ mtvs mctxt t     -> S.TyForall (fmap (map sTyVarBind) mtvs) (maybe [] sContext mctxt) (sType t)
    TyFun _ t1 t2               -> S.TyFun (sType t1) (sType t2)
    TyTuple _ bx ts             -> S.TyTuple bx (map sType ts)
    TyList _ t                  -> S.TyList (sType t)
    TyApp _ t1 t2               -> S.TyApp (sType t1) (sType t2)
    TyVar _ n                   -> S.TyVar (sName n)
    TyCon _ qn                  -> S.TyCon (sQName qn)
    TyParen _ t                 -> S.TyParen (sType t)
    TyInfix _ ta qn tb          -> S.TyInfix (sType ta) (sQName qn) (sType tb)
    TyKind _ t k                -> S.TyKind (sType t) (sKind k)

sTyVarBind :: TyVarBind l -> S.TyVarBind
sTyVarBind (KindedVar _ n k) = S.KindedVar (sName n) (sKind k)
sTyVarBind (UnkindedVar _ n) = S.UnkindedVar (sName n)

sKind :: Kind l -> S.Kind
sKind k = case k of
    KindStar  _     -> S.KindStar
    KindBang  _     -> S.KindBang
    KindFn _ k1 k2  -> S.KindFn (sKind k1) (sKind k2)
    KindParen _ k   -> S.KindParen (sKind k)
    KindVar _ n     -> S.KindVar (sName n)

sFunDep :: FunDep l -> S.FunDep
sFunDep (FunDep _ as bs) = S.FunDep (map sName as) (map sName bs)

sContext :: Context l -> S.Context
sContext ctxt = case ctxt of
    CxSingle _ asst     -> [sAsst asst]
    CxTuple  _ assts    -> map sAsst assts
    CxParen  _ ct       -> sContext ct
    CxEmpty  _          -> []

sAsst :: Asst l -> S.Asst
sAsst asst = case asst of
    ClassA _ qn ts      -> S.ClassA (sQName qn) (map sType ts)
    InfixA _ ta qn tb   -> S.InfixA (sType ta) (sQName qn) (sType tb)
    IParam _ ipn t      -> S.IParam (sIPName ipn) (sType t)
    EqualP _ t1 t2      -> S.EqualP (sType t1) (sType t2)

sLiteral :: Literal l -> S.Literal
sLiteral lit = case lit of
    Char       _ c _ -> S.Char c
    String     _ s _ -> S.String s
    Int        _ i _ -> S.Int i
    Frac       _ r _ -> S.Frac r
    PrimInt    _ i _ -> S.PrimInt i
    PrimWord   _ i _ -> S.PrimWord i
    PrimFloat  _ r _ -> S.PrimFloat r
    PrimDouble _ r _ -> S.PrimDouble r
    PrimChar   _ c _ -> S.PrimChar c
    PrimString _ s _ -> S.PrimString s

sExp :: SrcInfo loc => Exp loc -> S.Exp
sExp e = case e of
    Var _ qn            -> S.Var (sQName qn)
    IPVar _ ipn         -> S.IPVar (sIPName ipn)
    Con _ qn            -> S.Con (sQName qn)
    Lit _ lit           -> S.Lit (sLiteral lit)
    InfixApp _ e1 op e2 -> S.InfixApp (sExp e1) (sQOp op) (sExp e2)
    App _ e1 e2         -> S.App (sExp e1) (sExp e2)
    NegApp _ e          -> S.NegApp (sExp e)
    Lambda l ps e       -> S.Lambda (getPointLoc l) (map sPat ps) (sExp e)
    Let _ bs e          -> S.Let (sBinds bs) (sExp e)
    If _ e1 e2 e3       -> S.If (sExp e1) (sExp e2) (sExp e3)
    Case _ e alts       -> S.Case (sExp e) (map sAlt alts)
    Do _ ss             -> S.Do (map sStmt ss)
    MDo _ ss            -> S.MDo (map sStmt ss)
    Tuple _ bx es       -> S.Tuple bx (map sExp es)
    TupleSection _ bx mes -> S.TupleSection bx (map (fmap sExp) mes)
    List _ es           -> S.List (map sExp es)
    Paren _ e           -> S.Paren (sExp e)
    LeftSection _ e op  -> S.LeftSection (sExp e) (sQOp op)
    RightSection _ op e -> S.RightSection (sQOp op) (sExp e)
    RecConstr _ qn fups -> S.RecConstr (sQName qn) (map sFieldUpdate fups)
    RecUpdate _ e fups  -> S.RecUpdate (sExp e) (map sFieldUpdate fups)
    EnumFrom _ e        -> S.EnumFrom (sExp e)
    EnumFromTo _ e1 e2  -> S.EnumFromTo (sExp e1) (sExp e2)
    EnumFromThen _ e1 e2    -> S.EnumFromThen (sExp e1) (sExp e2)
    EnumFromThenTo _ e1 e2 e3   -> S.EnumFromThenTo (sExp e1) (sExp e2) (sExp e3)
    ListComp _ e qss    -> S.ListComp (sExp e) (map sQualStmt qss)
    ParComp  _ e qsss   -> S.ParComp (sExp e) (map (map sQualStmt) qsss)
    ExpTypeSig l e t    -> S.ExpTypeSig (getPointLoc l) (sExp e) (sType t)
    VarQuote _ qn       -> S.VarQuote (sQName qn)
    TypQuote _ qn       -> S.TypQuote (sQName qn)
    BracketExp _ br     -> S.BracketExp (sBracket br)
    SpliceExp _ sp      -> S.SpliceExp (sSplice sp)
    QuasiQuote _ nm qt  -> S.QuasiQuote nm qt
    XTag l xn attrs mat es  -> S.XTag  (getPointLoc l) (sXName xn) (map sXAttr attrs) (fmap sExp mat) (map sExp es)
    XETag l xn attrs mat    -> S.XETag (getPointLoc l) (sXName xn) (map sXAttr attrs) (fmap sExp mat)
    XPcdata _ str       -> S.XPcdata str
    XExpTag _ e         -> S.XExpTag (sExp e)
    XChildTag l es      -> S.XChildTag (getPointLoc l) (map sExp es)
    CorePragma _ str e  -> S.CorePragma str (sExp e)
    SCCPragma  _ str e  -> S.SCCPragma  str (sExp e)
    GenPragma  _ str i12 i34 e  -> S.GenPragma str i12 i34 (sExp e)
    Proc            l p  e  -> S.Proc (getPointLoc l) (sPat p) (sExp e)
    LeftArrApp      _ e1 e2 -> S.LeftArrApp (sExp e1) (sExp e2)
    RightArrApp     _ e1 e2 -> S.RightArrApp (sExp e1) (sExp e2)
    LeftArrHighApp  _ e1 e2 -> S.LeftArrHighApp (sExp e1) (sExp e2)
    RightArrHighApp _ e1 e2 -> S.RightArrHighApp (sExp e1) (sExp e2)


sXName :: XName l -> S.XName
sXName (XName _ str) = S.XName str
sXName (XDomName _ dom str) = S.XDomName dom str

sXAttr :: SrcInfo loc => XAttr loc -> S.XAttr
sXAttr (XAttr _ xn e) = S.XAttr (sXName xn) (sExp e)

sBracket:: SrcInfo loc => Bracket loc -> S.Bracket
sBracket br = case br of
    ExpBracket _ e  -> S.ExpBracket (sExp e)
    PatBracket _ p  -> S.PatBracket (sPat p)
    TypeBracket _ t -> S.TypeBracket (sType t)
    DeclBracket _ ds -> S.DeclBracket (map sDecl ds)

sSplice :: SrcInfo loc => Splice loc -> S.Splice
sSplice (IdSplice _ str) = S.IdSplice str
sSplice (ParenSplice _ e) = S.ParenSplice (sExp e)

sSafety :: Safety l -> S.Safety
sSafety (PlayRisky _) = S.PlayRisky
sSafety (PlaySafe _ b) = S.PlaySafe b
sSafety (PlayInterruptible _) = S.PlayInterruptible

sCallConv :: CallConv l -> S.CallConv
sCallConv (StdCall   _) = S.StdCall
sCallConv (CCall     _) = S.CCall
sCallConv (CPlusPlus _) = S.CPlusPlus
sCallConv (DotNet    _) = S.DotNet
sCallConv (Jvm       _) = S.Jvm
sCallConv (Js        _) = S.Js
sCallConv (CApi      _) = S.CApi

sModulePragma :: SrcInfo loc => ModulePragma loc -> S.ModulePragma
sModulePragma pr = case pr of
    LanguagePragma   l ns   -> S.LanguagePragma (getPointLoc l) (map sName ns)
    OptionsPragma    l mt str -> S.OptionsPragma (getPointLoc l) mt str
    AnnModulePragma  l ann -> S.AnnModulePragma (getPointLoc l) (sAnnotation ann)

sActivation :: Activation l -> S.Activation
sActivation act = case act of
    ActiveFrom   _ k    -> S.ActiveFrom k
    ActiveUntil  _ k    -> S.ActiveUntil k

sRule :: SrcInfo loc => Rule loc -> S.Rule
sRule (Rule _ str mact mrvs e1 e2) =
    S.Rule str (maybe S.AlwaysActive sActivation mact) (fmap (map sRuleVar) mrvs) (sExp e1) (sExp e2)

sRuleVar :: RuleVar l -> S.RuleVar
sRuleVar (RuleVar _ n) = S.RuleVar (sName n)
sRuleVar (TypedRuleVar _ n t) = S.TypedRuleVar (sName n) (sType t)

sWarningText :: WarningText l -> S.WarningText
sWarningText (DeprText _ str) = S.DeprText str
sWarningText (WarnText _ str) = S.WarnText str

sPat :: SrcInfo loc => Pat loc -> S.Pat
sPat pat = case pat of
    PVar _ n            -> S.PVar (sName n)
    PLit _ lit          -> S.PLit (sLiteral lit)
    PNeg _ p            -> S.PNeg (sPat p)
    PNPlusK _ n k       -> S.PNPlusK (sName n) k
    PInfixApp _ pa qn pb -> S.PInfixApp (sPat pa) (sQName qn) (sPat pb)
    PApp _ qn ps        -> S.PApp (sQName qn) (map sPat ps)
    PTuple _ bx ps      -> S.PTuple bx (map sPat ps)
    PList _ ps          -> S.PList (map sPat ps)
    PParen _ p          -> S.PParen (sPat p)
    PRec _ qn pfs       -> S.PRec (sQName qn) (map sPatField pfs)
    PAsPat _ n p        -> S.PAsPat (sName n) (sPat p)
    PWildCard _         -> S.PWildCard
    PIrrPat _ p         -> S.PIrrPat (sPat p)
    PatTypeSig l p t    -> S.PatTypeSig (getPointLoc l) (sPat p) (sType t)
    PViewPat _ e p      -> S.PViewPat (sExp e) (sPat p)
    PRPat _ rps         -> S.PRPat (map sRPat rps)
    PXTag l xn attrs mat ps -> S.PXTag (getPointLoc l) (sXName xn) (map sPXAttr attrs) (fmap sPat mat) (map sPat ps)
    PXETag l xn attrs mat   -> S.PXETag (getPointLoc l) (sXName xn) (map sPXAttr attrs) (fmap sPat mat)
    PXPcdata _ str      -> S.PXPcdata str
    PXPatTag _ p        -> S.PXPatTag (sPat p)
    PXRPats  _ rps      -> S.PXRPats (map sRPat rps)
    PExplTypeArg _ qn t -> S.PExplTypeArg (sQName qn) (sType t)
    PQuasiQuote _ nm qt -> S.PQuasiQuote nm qt
    PBangPat _ p        -> S.PBangPat (sPat p)

sPXAttr :: SrcInfo loc => PXAttr loc -> S.PXAttr
sPXAttr (PXAttr _ xn p) = S.PXAttr (sXName xn) (sPat p)

sRPatOp :: RPatOp l -> S.RPatOp
sRPatOp rpop = case rpop of
    RPStar  _ -> S.RPStar
    RPStarG _ -> S.RPStarG
    RPPlus  _ -> S.RPPlus
    RPPlusG _ -> S.RPPlusG
    RPOpt   _ -> S.RPOpt
    RPOptG  _ -> S.RPOptG

sRPat :: SrcInfo loc => RPat loc -> S.RPat
sRPat rp = case rp of
    RPOp _ rp rop       -> S.RPOp (sRPat rp) (sRPatOp rop)
    RPEither _ rp1 rp2  -> S.RPEither (sRPat rp1) (sRPat rp2)
    RPSeq _ rps         -> S.RPSeq (map sRPat rps)
    RPGuard _ p ss      -> S.RPGuard (sPat p) (map sStmt ss)
    RPCAs _ n rp        -> S.RPCAs (sName n) (sRPat rp)
    RPAs _ n rp         -> S.RPAs (sName n) (sRPat rp)
    RPParen _ rp        -> S.RPParen (sRPat rp)
    RPPat _ p           -> S.RPPat (sPat p)

sPatField :: SrcInfo loc => PatField loc -> S.PatField
sPatField pf = case pf of
    PFieldPat _ qn p    -> S.PFieldPat (sQName qn) (sPat p)
    PFieldPun _ n       -> S.PFieldPun (sName n)
    PFieldWildcard _    -> S.PFieldWildcard

sStmt :: SrcInfo loc => Stmt loc -> S.Stmt
sStmt stmt = case stmt of
    Generator l p e     -> S.Generator (getPointLoc l) (sPat p) (sExp e)
    Qualifier _ e       -> S.Qualifier (sExp e)
    LetStmt _ bs        -> S.LetStmt (sBinds bs)
    RecStmt _ ss        -> S.RecStmt (map sStmt ss)

sQualStmt :: SrcInfo loc => QualStmt loc -> S.QualStmt
sQualStmt qs = case qs of
    QualStmt     _ stmt     -> S.QualStmt (sStmt stmt)
    ThenTrans    _ e        -> S.ThenTrans (sExp e)
    ThenBy       _ e1 e2    -> S.ThenBy (sExp e1) (sExp e2)
    GroupBy      _ e        -> S.GroupBy (sExp e)
    GroupUsing   _ e        -> S.GroupUsing (sExp e)
    GroupByUsing _ e1 e2    -> S.GroupByUsing (sExp e1) (sExp e2)

sFieldUpdate :: SrcInfo loc => FieldUpdate loc -> S.FieldUpdate
sFieldUpdate fu = case fu of
    FieldUpdate _ qn e      -> S.FieldUpdate (sQName qn) (sExp e)
    FieldPun _ n            -> S.FieldPun (sName n)
    FieldWildcard _         -> S.FieldWildcard

sAlt :: SrcInfo loc => Alt loc -> S.Alt
sAlt (Alt l p galts mbs) = S.Alt (getPointLoc l) (sPat p) (sGuardedAlts galts) (maybe (S.BDecls []) sBinds mbs)

sGuardedAlts :: SrcInfo loc => GuardedAlts loc -> S.GuardedAlts
sGuardedAlts galts = case galts of
    UnGuardedAlt _ e    -> S.UnGuardedAlt (sExp e)
    GuardedAlts  _ gs   -> S.GuardedAlts (map sGuardedAlt gs)

sGuardedAlt :: SrcInfo loc => GuardedAlt loc -> S.GuardedAlt
sGuardedAlt (GuardedAlt l ss e) = S.GuardedAlt (getPointLoc l) (map sStmt ss) (sExp e)

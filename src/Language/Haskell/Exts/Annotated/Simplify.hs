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

-- | Translate an annotated AST node representing a Haskell module, into
--   a simpler version that retains (almost) only abstract information.
--   In particular, XML and hybrid XML pages enabled by the XmlSyntax extension
--   are translated into standard Haskell modules with a @page@ function.
sModule :: Module loc -> S.Module
sModule md = case md of
    Module _ mmh oss ids ds ->
        let (mn, mwt, mes) = sModuleHead mmh
         in S.Module mn (map sModulePragma oss) mwt mes (map sImportDecl ids) (map sDecl ds)
    XmlPage _ mn oss xn attrs mat es   ->
        S.Module (sModuleName mn) (map sModulePragma oss)
                      Nothing
                      (Just [S.EVar $ S.UnQual $ S.Ident "page"])
                        []
                        [pageFun $ S.XTag (sXName xn) (map sXAttr attrs) (fmap sExp mat) (map sExp es)]
    XmlHybrid _ mmh oss ids ds xn attrs mat es  ->
        let (mn, mwt, mes) = sModuleHead mmh
         in S.Module mn (map sModulePragma oss) mwt mes (map sImportDecl ids)
                (map sDecl ds ++ [pageFun $ S.XTag (sXName xn) (map sXAttr attrs) (fmap sExp mat) (map sExp es)])
  where pageFun :: S.Exp -> S.Decl
        pageFun e = S.PatBind namePat Nothing rhs (S.BDecls [])
            where namePat = S.PVar $ S.Ident "page"
                  rhs = S.UnGuardedRhs e


-- | Translate an annotated AST node representing a Haskell declaration
--   into a simpler version.
sDecl :: Decl loc -> S.Decl
sDecl decl = case decl of
     TypeDecl     _ dh t        ->
        let (n, tvs) = sDeclHead dh
         in S.TypeDecl n tvs (sType t)
     TypeFamDecl  _ dh mk       ->
        let (n, tvs) = sDeclHead dh
         in S.TypeFamDecl n tvs (fmap sKind mk)
     DataDecl     _ dn mctxt dh constrs mder    ->
        let (n, tvs) = sDeclHead dh
         in S.DataDecl (sDataOrNew dn) (maybe [] sContext mctxt) n tvs (map sQualConDecl constrs) (maybe [] sDeriving mder)
     GDataDecl    _ dn mctxt dh mk gds mder     ->
        let (n, tvs) = sDeclHead dh
         in S.GDataDecl (sDataOrNew dn) (maybe [] sContext mctxt) n tvs (fmap sKind mk) (map sGadtDecl gds) (maybe [] sDeriving mder)
     DataFamDecl  _ mctxt dh mk ->
        let (n, tvs) = sDeclHead dh
         in S.DataFamDecl (maybe [] sContext mctxt) n tvs (fmap sKind mk)
     TypeInsDecl  _ t1 t2       -> S.TypeInsDecl (sType t1) (sType t2)
     DataInsDecl  _ dn t constrs mder           ->
        S.DataInsDecl (sDataOrNew dn) (sType t) (map sQualConDecl constrs) (maybe [] sDeriving mder)
     GDataInsDecl _ dn t mk gds mder            ->
        S.GDataInsDecl (sDataOrNew dn) (sType t) (fmap sKind mk) (map sGadtDecl gds) (maybe [] sDeriving mder)
     ClassDecl    _ mctxt dh fds mcds           ->
        let (n, tvs) = sDeclHead dh
         in S.ClassDecl (maybe [] sContext mctxt) n tvs (map sFunDep fds) (maybe [] (map sClassDecl) mcds)
     InstDecl     _ mctxt ih mids               ->
        let (qn, ts) = sInstHead ih
         in S.InstDecl (maybe [] sContext mctxt) qn ts (maybe [] (map sInstDecl) mids)
     DerivDecl    _ mctxt ih    ->
        let (qn, ts) = sInstHead ih
         in S.DerivDecl (maybe [] sContext mctxt) qn ts
     InfixDecl    _ ass prec ops    -> S.InfixDecl (sAssoc ass) (maybe 9 id prec) (map sOp ops)
     DefaultDecl  _ ts          -> S.DefaultDecl (map sType ts)
     SpliceDecl   _ sp          -> S.SpliceDecl (sExp sp)
     TypeSig      _ ns t        -> S.TypeSig (map sName ns) (sType t)
     FunBind      _ ms          -> S.FunBind (map sMatch ms)
     PatBind      _ p mt rhs mbs    ->
        S.PatBind (sPat p) (fmap sType mt) (sRhs rhs) (maybe (S.BDecls []) sBinds mbs)
     ForImp       _ cc msaf mstr n t    ->
        S.ForImp (sCallConv cc) (maybe (S.PlaySafe False) sSafety msaf) (maybe "" id mstr) (sName n) (sType t)
     ForExp       _ cc      mstr n t    ->
        S.ForExp (sCallConv cc) (maybe "" id mstr) (sName n) (sType t)
     RulePragmaDecl   _ rs      -> S.RulePragmaDecl (map sRule rs)
     DeprPragmaDecl   _ nsstrs  -> S.DeprPragmaDecl (map (\(ns, str) -> (map sName ns, str)) nsstrs)
     WarnPragmaDecl   _ nsstrs  -> S.WarnPragmaDecl (map (\(ns, str) -> (map sName ns, str)) nsstrs)
     InlineSig        _ b mact qn   -> S.InlineSig b (maybe S.AlwaysActive sActivation mact) (sQName qn)
     InlineConlikeSig _   mact qn   -> S.InlineConlikeSig (maybe S.AlwaysActive sActivation mact) (sQName qn)
     SpecSig          _   mact qn ts   -> S.SpecSig (maybe S.AlwaysActive sActivation mact) (sQName qn) (map sType ts)
     SpecInlineSig    _ b mact qn ts    ->
        S.SpecInlineSig b (maybe S.AlwaysActive sActivation mact) (sQName qn) (map sType ts)
     InstSig          _ mctxt ih    ->
        let (qn, ts) = sInstHead ih
         in S.InstSig (maybe [] sContext mctxt) qn ts
     AnnPragma        _ ann         ->
        S.AnnPragma (sAnnotation ann)

sAnnotation :: Annotation loc -> S.Annotation
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

sImportDecl :: ImportDecl loc -> S.ImportDecl
sImportDecl (ImportDecl _ mn qu src mpkg as misl) =
    S.ImportDecl (sModuleName mn) qu src mpkg (fmap sModuleName as) (fmap sImportSpecList misl)

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

sBinds :: Binds loc -> S.Binds
sBinds bs = case bs of
    BDecls  _ decls     -> S.BDecls (map sDecl decls)
    IPBinds _ ipbds     -> S.IPBinds (map sIPBind ipbds)

sIPBind :: IPBind loc -> S.IPBind
sIPBind (IPBind _ ipn e) = S.IPBind (sIPName ipn) (sExp e)

sMatch :: Match loc -> S.Match
sMatch (Match _ n ps rhs mwhere) =
    S.Match (sName n) (map sPat ps) Nothing (sRhs rhs) (maybe (S.BDecls []) sBinds mwhere)
sMatch (InfixMatch _ pa n pbs rhs mwhere) =
    S.Match (sName n) (map sPat (pa:pbs)) Nothing (sRhs rhs) (maybe (S.BDecls []) sBinds mwhere)

sQualConDecl :: QualConDecl loc -> S.QualConDecl
sQualConDecl (QualConDecl _ mtvs mctxt cd) =
    S.QualConDecl (maybe [] (map sTyVarBind) mtvs) (maybe [] sContext mctxt) (sConDecl cd)

sConDecl :: ConDecl l -> S.ConDecl
sConDecl cd = case cd of
    ConDecl _ n bts     -> S.ConDecl (sName n) (map sBangType bts)
    InfixConDecl _ bta n btb -> S.InfixConDecl (sBangType bta) (sName n) (sBangType btb)
    RecDecl _ n fds -> S.RecDecl (sName n) (map sFieldDecl fds)

sFieldDecl :: FieldDecl l -> ([S.Name], S.BangType)
sFieldDecl (FieldDecl _ ns bt) = (map sName ns, sBangType bt)

sGadtDecl :: GadtDecl loc -> S.GadtDecl
sGadtDecl (GadtDecl _ n t) = S.GadtDecl (sName n) (sType t)

sClassDecl :: ClassDecl loc -> S.ClassDecl
sClassDecl cd = case cd of
    ClsDecl _ d  -> S.ClsDecl (sDecl d)
    ClsDataFam _ mctxt dh mk    ->
        let (n, tvs) = sDeclHead dh
         in S.ClsDataFam (maybe [] sContext mctxt) n tvs (fmap sKind mk)
    ClsTyFam _ dh mk    ->
        let (n, tvs) = sDeclHead dh
         in S.ClsTyFam n tvs (fmap sKind mk)
    ClsTyDef _ t1 t2    ->
        S.ClsTyDef (sType t1) (sType t2)

sInstDecl :: InstDecl loc -> S.InstDecl
sInstDecl id = case id of
    InsDecl   _ d   -> S.InsDecl (sDecl d)
    InsType   _ t1 t2   -> S.InsType (sType t1) (sType t2)
    InsData   _ dn t constrs mder   ->
        S.InsData (sDataOrNew dn) (sType t) (map sQualConDecl constrs) (maybe [] sDeriving mder)
    InsGData  _ dn t mk gds mder    ->
        S.InsGData (sDataOrNew dn) (sType t) (fmap sKind mk) (map sGadtDecl gds) (maybe [] sDeriving mder)
--    InsInline _ b mact qn   -> S.InsInline b (maybe S.AlwaysActive sActivation mact) (sQName qn)

sBangType :: BangType l -> S.BangType
sBangType bt = case bt of
    BangedTy   _ t  -> S.BangedTy (sType t)
    UnBangedTy _ t  -> S.UnBangedTy (sType t)
    UnpackedTy _ t  -> S.UnpackedTy (sType t)

sRhs :: Rhs loc -> S.Rhs
sRhs (UnGuardedRhs _ e) = S.UnGuardedRhs (sExp e)
sRhs (GuardedRhss _ grhss) = S.GuardedRhss (map sGuardedRhs grhss)

sGuardedRhs :: GuardedRhs loc -> S.GuardedRhs
sGuardedRhs (GuardedRhs _ ss e) = S.GuardedRhs (map sStmt ss) (sExp e)

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
    TyPromoted _ t              -> S.TyPromoted (sPromoted t)

sPromoted :: Promoted l -> S.Promoted
sPromoted p = case p of
    PromotedInteger _ n _ -> S.PromotedInteger n
    PromotedString _ s _ -> S.PromotedString s
    PromotedCon _ b qn -> S.PromotedCon b (sQName qn)
    PromotedList _ b ps -> S.PromotedList b (map sPromoted ps)
    PromotedTuple _ ps -> S.PromotedTuple (map sPromoted ps)
    PromotedUnit _ -> S.PromotedUnit


sTyVarBind :: TyVarBind l -> S.TyVarBind
sTyVarBind (KindedVar _ n k) = S.KindedVar (sName n) (sKind k)
sTyVarBind (UnkindedVar _ n) = S.UnkindedVar (sName n)

sKind :: Kind l -> S.Kind
sKind k = case k of
    KindStar  _     -> S.KindStar
    KindBang  _     -> S.KindBang
    KindFn _ k1 k2  -> S.KindFn (sKind k1) (sKind k2)
    KindParen _ k   -> S.KindParen (sKind k)
    KindVar _ n     -> S.KindVar (sQName n)
    KindApp _ k1 k2 -> S.KindApp (sKind k1) (sKind k2)
    KindTuple _ ks  -> S.KindTuple (map sKind ks)
    KindList  _ ks  -> S.KindList  (map sKind ks)

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

sExp :: Exp loc -> S.Exp
sExp e = case e of
    Var _ qn            -> S.Var (sQName qn)
    IPVar _ ipn         -> S.IPVar (sIPName ipn)
    Con _ qn            -> S.Con (sQName qn)
    Lit _ lit           -> S.Lit (sLiteral lit)
    InfixApp _ e1 op e2 -> S.InfixApp (sExp e1) (sQOp op) (sExp e2)
    App _ e1 e2         -> S.App (sExp e1) (sExp e2)
    NegApp _ e          -> S.NegApp (sExp e)
    Lambda _ ps e       -> S.Lambda (map sPat ps) (sExp e)
    Let _ bs e          -> S.Let (sBinds bs) (sExp e)
    If _ e1 e2 e3       -> S.If (sExp e1) (sExp e2) (sExp e3)
    MultiIf _ alts      -> S.MultiIf (map sIfAlt alts)
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
    ExpTypeSig _ e t    -> S.ExpTypeSig (sExp e) (sType t)
    VarQuote _ qn       -> S.VarQuote (sQName qn)
    TypQuote _ qn       -> S.TypQuote (sQName qn)
    BracketExp _ br     -> S.BracketExp (sBracket br)
    SpliceExp _ sp      -> S.SpliceExp (sSplice sp)
    QuasiQuote _ nm qt  -> S.QuasiQuote nm qt
    XTag _ xn attrs mat es  -> S.XTag  (sXName xn) (map sXAttr attrs) (fmap sExp mat) (map sExp es)
    XETag _ xn attrs mat    -> S.XETag (sXName xn) (map sXAttr attrs) (fmap sExp mat)
    XPcdata _ str       -> S.XPcdata str
    XExpTag _ e         -> S.XExpTag (sExp e)
    XChildTag _ es      -> S.XChildTag (map sExp es)
    CorePragma _ str e  -> S.CorePragma str (sExp e)
    SCCPragma  _ str e  -> S.SCCPragma  str (sExp e)
    GenPragma  _ str i12 i34 e  -> S.GenPragma str i12 i34 (sExp e)
    Proc            _ p  e  -> S.Proc (sPat p) (sExp e)
    LeftArrApp      _ e1 e2 -> S.LeftArrApp (sExp e1) (sExp e2)
    RightArrApp     _ e1 e2 -> S.RightArrApp (sExp e1) (sExp e2)
    LeftArrHighApp  _ e1 e2 -> S.LeftArrHighApp (sExp e1) (sExp e2)
    RightArrHighApp _ e1 e2 -> S.RightArrHighApp (sExp e1) (sExp e2)
    LCase _ alts -> S.LCase (map sAlt alts)


sXName :: XName l -> S.XName
sXName (XName _ str) = S.XName str
sXName (XDomName _ dom str) = S.XDomName dom str

sXAttr :: XAttr loc -> S.XAttr
sXAttr (XAttr _ xn e) = S.XAttr (sXName xn) (sExp e)

sBracket:: Bracket loc -> S.Bracket
sBracket br = case br of
    ExpBracket _ e  -> S.ExpBracket (sExp e)
    PatBracket _ p  -> S.PatBracket (sPat p)
    TypeBracket _ t -> S.TypeBracket (sType t)
    DeclBracket _ ds -> S.DeclBracket (map sDecl ds)

sSplice :: Splice loc -> S.Splice
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

sModulePragma :: ModulePragma loc -> S.ModulePragma
sModulePragma pr = case pr of
    LanguagePragma   _ ns   -> S.LanguagePragma (map sName ns)
    OptionsPragma    _ mt str -> S.OptionsPragma mt str
    AnnModulePragma  _ ann -> S.AnnModulePragma (sAnnotation ann)

sActivation :: Activation l -> S.Activation
sActivation act = case act of
    ActiveFrom   _ k    -> S.ActiveFrom k
    ActiveUntil  _ k    -> S.ActiveUntil k

sRule :: Rule loc -> S.Rule
sRule (Rule _ str mact mrvs e1 e2) =
    S.Rule str (maybe S.AlwaysActive sActivation mact) (fmap (map sRuleVar) mrvs) (sExp e1) (sExp e2)

sRuleVar :: RuleVar l -> S.RuleVar
sRuleVar (RuleVar _ n) = S.RuleVar (sName n)
sRuleVar (TypedRuleVar _ n t) = S.TypedRuleVar (sName n) (sType t)

sWarningText :: WarningText l -> S.WarningText
sWarningText (DeprText _ str) = S.DeprText str
sWarningText (WarnText _ str) = S.WarnText str

sPat :: Pat loc -> S.Pat
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
    PatTypeSig _ p t    -> S.PatTypeSig (sPat p) (sType t)
    PViewPat _ e p      -> S.PViewPat (sExp e) (sPat p)
    PRPat _ rps         -> S.PRPat (map sRPat rps)
    PXTag _ xn attrs mat ps -> S.PXTag (sXName xn) (map sPXAttr attrs) (fmap sPat mat) (map sPat ps)
    PXETag _ xn attrs mat   -> S.PXETag (sXName xn) (map sPXAttr attrs) (fmap sPat mat)
    PXPcdata _ str      -> S.PXPcdata str
    PXPatTag _ p        -> S.PXPatTag (sPat p)
    PXRPats  _ rps      -> S.PXRPats (map sRPat rps)
    PQuasiQuote _ nm qt -> S.PQuasiQuote nm qt
    PBangPat _ p        -> S.PBangPat (sPat p)

sPXAttr :: PXAttr loc -> S.PXAttr
sPXAttr (PXAttr _ xn p) = S.PXAttr (sXName xn) (sPat p)

sRPatOp :: RPatOp l -> S.RPatOp
sRPatOp rpop = case rpop of
    RPStar  _ -> S.RPStar
    RPStarG _ -> S.RPStarG
    RPPlus  _ -> S.RPPlus
    RPPlusG _ -> S.RPPlusG
    RPOpt   _ -> S.RPOpt
    RPOptG  _ -> S.RPOptG

sRPat :: RPat loc -> S.RPat
sRPat rp = case rp of
    RPOp _ rp rop       -> S.RPOp (sRPat rp) (sRPatOp rop)
    RPEither _ rp1 rp2  -> S.RPEither (sRPat rp1) (sRPat rp2)
    RPSeq _ rps         -> S.RPSeq (map sRPat rps)
    RPGuard _ p ss      -> S.RPGuard (sPat p) (map sStmt ss)
    RPCAs _ n rp        -> S.RPCAs (sName n) (sRPat rp)
    RPAs _ n rp         -> S.RPAs (sName n) (sRPat rp)
    RPParen _ rp        -> S.RPParen (sRPat rp)
    RPPat _ p           -> S.RPPat (sPat p)

sPatField :: PatField loc -> S.PatField
sPatField pf = case pf of
    PFieldPat _ qn p    -> S.PFieldPat (sQName qn) (sPat p)
    PFieldPun _ n       -> S.PFieldPun (sName n)
    PFieldWildcard _    -> S.PFieldWildcard

sStmt :: Stmt loc -> S.Stmt
sStmt stmt = case stmt of
    Generator _ p e     -> S.Generator (sPat p) (sExp e)
    Qualifier _ e       -> S.Qualifier (sExp e)
    LetStmt _ bs        -> S.LetStmt (sBinds bs)
    RecStmt _ ss        -> S.RecStmt (map sStmt ss)

sQualStmt :: QualStmt loc -> S.QualStmt
sQualStmt qs = case qs of
    QualStmt     _ stmt     -> S.QualStmt (sStmt stmt)
    ThenTrans    _ e        -> S.ThenTrans (sExp e)
    ThenBy       _ e1 e2    -> S.ThenBy (sExp e1) (sExp e2)
    GroupBy      _ e        -> S.GroupBy (sExp e)
    GroupUsing   _ e        -> S.GroupUsing (sExp e)
    GroupByUsing _ e1 e2    -> S.GroupByUsing (sExp e1) (sExp e2)

sFieldUpdate :: FieldUpdate loc -> S.FieldUpdate
sFieldUpdate fu = case fu of
    FieldUpdate _ qn e      -> S.FieldUpdate (sQName qn) (sExp e)
    FieldPun _ n            -> S.FieldPun (sName n)
    FieldWildcard _         -> S.FieldWildcard

sAlt :: Alt loc -> S.Alt
sAlt (Alt _ p galts mbs) = S.Alt (sPat p) (sGuardedAlts galts) (maybe (S.BDecls []) sBinds mbs)

sGuardedAlts :: GuardedAlts loc -> S.GuardedAlts
sGuardedAlts galts = case galts of
    UnGuardedAlt _ e    -> S.UnGuardedAlt (sExp e)
    GuardedAlts  _ gs   -> S.GuardedAlts (map sGuardedAlt gs)

sGuardedAlt :: GuardedAlt loc -> S.GuardedAlt
sGuardedAlt (GuardedAlt _ ss e) = S.GuardedAlt (map sStmt ss) (sExp e)

sIfAlt :: IfAlt loc -> S.IfAlt
sIfAlt (IfAlt _ e1 e2) = S.IfAlt (sExp e1) (sExp e2)

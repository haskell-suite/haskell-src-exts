module Language.Haskell.Exts.Fixity where

import Language.Haskell.Exts.Syntax

import Data.Char (isUpper)

------------------------------------------------------------
--

data Fixity = Fixity Assoc Int Op

class AppFixity ast where
  applyFixities :: [Fixity] -> ast -> ast


instance AppFixity Exp where
  applyFixities fixs = infFix fixs . leafFix fixs
    where -- This is the real meat case. We can assume a left-associative list to begin with.
          infFix fixs (InfixApp a op2 z) =
              let e = infFix fixs a
               in case e of
                   InfixApp x op1 y ->
                      let (a1,p1) = askFixity fixs op1
                          (a2,p2) = askFixity fixs op2
                       in if (p1 == p2 && (a1 /= a2 || a1 == AssocNone)) -- Ambiguous infix expression!
                              || (p1 > p2 || p1 == p2 && (a1 == AssocLeft || a2 == AssocNone)) -- Already right order
                           then InfixApp e op2 z
                           else InfixApp x op1 (infFix fixs $ InfixApp y op2 z)
                   _  -> InfixApp e op2 z

          infFix _ e = e


-- Internal: lookup associativity and precedence of an operator
askFixity :: [Fixity] -> QOp -> (Assoc, Int)
askFixity xs = \k -> lookupWithDefault (AssocLeft, 9) (f k) mp
    where
        lookupWithDefault def k mp = case lookup k mp of
            Nothing -> def
            Just x  -> x

        mp = [(x,(a,p)) | Fixity a p x <- xs]

        f (QVarOp x) = VarOp (g x)
        f (QConOp x) = ConOp (g x)

        g (Qual _ x) = x
        g (UnQual x) = x
        g (Special Cons) = Symbol ":"


preludeFixities :: [Fixity]
preludeFixities = concat
    [infixr_ 9  ["."]
    ,infixl_ 9  ["!!"]
    ,infixr_ 8  ["^","^^","**"]
    ,infixl_ 7  ["*","/","`quot`","`rem`","`div`","`mod`",":%","%"]
    ,infixl_ 6  ["+","-"]
    ,infixr_ 5  [":","++"]
    ,infix_  4  ["==","/=","<","<=",">=",">","`elem`","`notElem`"]
    ,infixr_ 3  ["&&"]
    ,infixr_ 2  ["||"]
    ,infixl_ 1  [">>",">>="]
    ,infixr_ 1  ["=<<"]
    ,infixr_ 0  ["$","$!","`seq`"]
    ]

baseFixities :: [Fixity]
baseFixities = preludeFixities ++ concat
    [infixl_ 9 ["!","//","!:"]
    ,infixl_ 8 ["`shift`","`rotate`","`shiftL`","`shiftR`","`rotateL`","`rotateR`"]
    ,infixl_ 7 [".&."]
    ,infixl_ 6 ["`xor`"]
    ,infix_  6 [":+"]
    ,infixl_ 5 [".|."]
    ,infixr_ 5 ["+:+","<++","<+>"] -- fixity conflict for +++ between ReadP and Arrow
    ,infix_  5 ["\\\\"]
    ,infixl_ 4 ["<$>","<$","<*>","<*","*>","<**>"]
    ,infix_  4 ["`elemP`","`notElemP`"]
    ,infixl_ 3 ["<|>"]
    ,infixr_ 3 ["&&&","***"]
    ,infixr_ 2 ["+++","|||"]
    ,infixr_ 1 ["<=<",">=>",">>>","<<<","^<<","<<^","^>>",">>^"]
    ,infixl_ 0 ["`on`"]
    ,infixr_ 0 ["`par`","`pseq`"]
    ]


infixr_ = fixity AssocRight
infixl_ = fixity AssocLeft
infix_  = fixity AssocNone

fixity :: Assoc -> Int -> [String] -> [Fixity]
fixity a p = map (Fixity a p . op)
    where
        op ('`':xs) = (if isUpper (head xs) then ConOp else VarOp) $ Ident $ init xs
        op xs = (if head xs == ':' then ConOp else VarOp) $ Symbol xs






-------------------------------------------------------------------
-- Boilerplate - yuck!!

instance AppFixity Module where
    applyFixities fixs (Module loc n prs mwt ext imp decls) =
        Module loc n prs mwt ext imp $ appFixDecls fixs decls

instance AppFixity Decl where
    applyFixities fixs decl = case decl of
        ClassDecl loc ctxt n vars deps cdecls   -> ClassDecl loc ctxt n vars deps $ map fix cdecls
        InstDecl loc ctxt n ts idecls           -> InstDecl loc ctxt n ts $ map fix idecls
        SpliceDecl loc spl      -> SpliceDecl loc $ fix spl
        FunBind matches         -> FunBind $ map fix matches
        PatBind loc p mt rhs bs -> PatBind loc (fix p) mt (fix rhs) (fix bs)
        _                       -> decl
      where fix x = applyFixities fixs x

appFixDecls :: [Fixity] -> [Decl] -> [Decl]
appFixDecls fixs decls =
    let extraFixs = getFixities decls
     in map (applyFixities (fixs++extraFixs)) decls
  where getFixities = concatMap getFixity
        getFixity (InfixDecl _ a p ops) = map (Fixity a p) ops
        getFixity _ = []

instance AppFixity ClassDecl where
    applyFixities fixs (ClsDecl decl) = ClsDecl $ applyFixities fixs decl
    applyFixities _ cdecl = cdecl

instance AppFixity InstDecl where
    applyFixities fixs (InsDecl decl) = InsDecl $ applyFixities fixs decl
    applyFixities _ idecl = idecl

instance AppFixity Match where
    applyFixities fixs (Match loc n ps mt rhs bs) = Match loc n (map fix ps) mt (fix rhs) (fix bs)
      where fix x = applyFixities fixs x

instance AppFixity Rhs where
    applyFixities fixs rhs = case rhs of
        UnGuardedRhs e      -> UnGuardedRhs $ fix e
        GuardedRhss grhss   -> GuardedRhss $ map fix grhss
      where fix x = applyFixities fixs x

instance AppFixity GuardedRhs where
    applyFixities fixs (GuardedRhs loc stmts e) = GuardedRhs loc (map fix stmts) $ fix e
      where fix x = applyFixities fixs x

instance AppFixity Pat where
    applyFixities fixs p = case p of
        PNeg p                -> PNeg $ fix p
        PInfixApp a op b      -> PInfixApp (fix a) op (fix b)
        PApp n ps             -> PApp n $ map fix ps
        PTuple ps             -> PTuple $ map fix ps
        PList ps              -> PList $ map fix ps
        PParen p              -> PParen $ fix p
        PRec n pfs            -> PRec n $ map fix pfs
        PAsPat n p            -> PAsPat n $ fix p
        PIrrPat p             -> PIrrPat $ fix p
        PatTypeSig loc p t    -> PatTypeSig loc (fix p) t
        PViewPat e p          -> PViewPat (fix e) (fix p)
        PRPat rps             -> PRPat $ map fix rps
        PXTag loc n ats mp ps -> PXTag loc n (map fix ats) (fmap fix mp) (map fix ps)
        PXETag loc n ats mp   -> PXETag loc n (map fix ats) (fmap fix mp)
        PXPatTag p            -> PXPatTag $ fix p
        PXRPats rps           -> PXRPats $ map fix rps
        PBangPat p            -> PBangPat $ fix p
        _                     -> p
      where fix x = applyFixities fixs x

instance AppFixity PatField where
    applyFixities fixs (PFieldPat n p) = PFieldPat n $ applyFixities fixs p
    applyFixities _ pf = pf

instance AppFixity RPat where
    applyFixities fixs rp = case rp of
        RPOp rp op          -> RPOp (fix rp) op
        RPEither a b        -> RPEither (fix a) (fix b)
        RPSeq rps           -> RPSeq $ map fix rps
        RPGuard p stmts     -> RPGuard (fix p) $ map fix stmts
        RPCAs n rp          -> RPCAs n $ fix rp
        RPAs n rp           -> RPAs n $ fix rp
        RPParen rp          -> RPParen $ fix rp
        RPPat p             -> RPPat $ fix p
      where fix x = applyFixities fixs x

instance AppFixity PXAttr where
    applyFixities fixs (PXAttr n p) = PXAttr n $ applyFixities fixs p

instance AppFixity Stmt where
    applyFixities fixs stmt = case stmt of
        Generator loc p e   -> Generator loc (fix p) (fix e)
        Qualifier e         -> Qualifier $ fix e
        LetStmt bs          -> LetStmt $ fix bs    -- special behavior
        RecStmt stmts       -> RecStmt $ map fix stmts
      where fix x = applyFixities fixs x

instance AppFixity Binds where
    applyFixities fixs bs = case bs of
        BDecls decls        -> BDecls $ appFixDecls fixs decls  -- special behavior
        IPBinds ips         -> IPBinds $ map fix ips
      where fix x = applyFixities fixs x


instance AppFixity IPBind where
    applyFixities fixs (IPBind loc n e) = IPBind loc n $ applyFixities fixs e

instance AppFixity FieldUpdate where
    applyFixities fixs (FieldUpdate n e) = FieldUpdate n $ applyFixities fixs e
    applyFixities _ fup = fup

instance AppFixity Alt where
    applyFixities fixs (Alt loc p galts bs) = Alt loc (fix p) (fix galts) (fix bs)
      where fix x = applyFixities fixs x

instance AppFixity GuardedAlts where
    applyFixities fixs galts = case galts of
        UnGuardedAlt e      -> UnGuardedAlt $ fix e
        GuardedAlts  galts  -> GuardedAlts $ map fix galts
      where fix x = applyFixities fixs x

instance AppFixity GuardedAlt where
    applyFixities fixs (GuardedAlt loc stmts e) = GuardedAlt loc (map fix stmts) (fix e)
      where fix x = applyFixities fixs x

instance AppFixity QualStmt where
    applyFixities fixs qstmt = case qstmt of
        QualStmt     s      -> QualStmt $ fix s
        ThenTrans    e      -> ThenTrans $ fix e
        ThenBy       e1 e2  -> ThenBy (fix e1) (fix e2)
        GroupBy      e      -> GroupBy (fix e)
        GroupUsing   e      -> GroupUsing (fix e)
        GroupByUsing e1 e2  -> GroupByUsing (fix e1) (fix e2)
      where fix x = applyFixities fixs x

instance AppFixity Bracket where
    applyFixities fixs br = case br of
        ExpBracket e    -> ExpBracket $ fix e
        PatBracket p    -> PatBracket $ fix p
        DeclBracket ds  -> DeclBracket $ map fix ds
        _               -> br
      where fix x = applyFixities fixs x

instance AppFixity Splice where
    applyFixities fixs (ParenSplice e) = ParenSplice $ applyFixities fixs e
    applyFixities _ s = s

instance AppFixity XAttr where
    applyFixities fixs (XAttr n e) = XAttr n $ applyFixities fixs e


-- the boring boilerplate stuff for expressions too
-- Recursively fixes the "leaves" of the infix chains,
-- without yet touching the chain itself. We assume all chains are
-- left-associate to begin with.
leafFix fixs e = case e of
    InfixApp e1 op e2       -> InfixApp (leafFix fixs e1) op (fix e2)
    App e1 e2               -> App (fix e1) (fix e2)
    NegApp e                -> NegApp $ fix e
    Lambda loc pats e       -> Lambda loc (map fix pats) $ fix e
    Let bs e                -> Let (fix bs) $ fix e
    If e a b                -> If (fix e) (fix a) (fix b)
    Case e alts             -> Case (fix e) $ map fix alts
    Do stmts                -> Do $ map fix stmts
    MDo stmts               -> MDo $ map fix stmts
    Tuple exps              -> Tuple $ map fix exps
    List exps               -> List $ map fix  exps
    Paren e                 -> Paren $ fix e
    LeftSection e op        -> LeftSection (fix e) op
    RightSection op e       -> RightSection op $ fix e
    RecConstr n fups        -> RecConstr n $ map fix fups
    RecUpdate e fups        -> RecUpdate (fix e) $ map fix fups
    EnumFrom e              -> EnumFrom $ fix e
    EnumFromTo e1 e2        -> EnumFromTo (fix e1) (fix e2)
    EnumFromThen e1 e2      -> EnumFromThen (fix e1) (fix e2)
    EnumFromThenTo e1 e2 e3 -> EnumFromThenTo (fix e1) (fix e2) (fix e3)
    ListComp e quals        -> ListComp (fix e) $ map fix quals
    ParComp  e qualss       -> ParComp (fix e) $ map (map fix) qualss
    ExpTypeSig loc e t      -> ExpTypeSig loc (fix e) t
    BracketExp b            -> BracketExp $ fix b
    SpliceExp s             -> SpliceExp $ fix s
    XTag loc n ats mexp cs  -> XTag loc n (map fix ats) (fmap fix mexp) (map fix cs)
    XETag loc n ats mexp    -> XETag loc n (map fix ats) (fmap fix mexp)
    XExpTag e               -> XExpTag $ fix e
    Proc p e                -> Proc (fix p) (fix e)
    LeftArrApp e1 e2        -> LeftArrApp (fix e1) (fix e2)
    RightArrApp e1 e2       -> RightArrApp (fix e1) (fix e2)
    LeftArrHighApp e1 e2    -> LeftArrHighApp (fix e1) (fix e2)
    RightArrHighApp e1 e2   -> RightArrHighApp (fix e1) (fix e2)
    CorePragma s e          -> CorePragma s (fix e)
    SCCPragma s e           -> SCCPragma s (fix e)
    GenPragma s ab cd e     -> GenPragma s ab cd (fix e)

    _                       -> e
  where
    fix x = applyFixities fixs x
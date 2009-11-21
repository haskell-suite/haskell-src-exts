-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Annotated.ExactPrint
-- Copyright   :  (c) Niklas Broberg 2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Exact-printer for Haskell abstract syntax. The input is a (semi-concrete)
-- abstract syntax tree, annotated with exact source information to enable
-- printing the tree exactly as it was parsed.
--
-----------------------------------------------------------------------------
module Language.Haskell.Exts.Annotated.ExactPrint
        ( exactPrint
        , ExactP
        ) where

import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Comments

import Control.Monad (when)

-- import Debug.Trace (trace)

------------------------------------------------------
-- The EP monad and basic combinators

type Pos = (Int,Int)

pos :: (SrcInfo loc) => loc -> Pos
pos ss = (startLine ss, startColumn ss)

newtype EP x = EP (Pos -> [Comment] -> (x, Pos, [Comment], ShowS))

instance Monad EP where
  return x = EP $ \l cs -> (x, l, cs, id)

  EP m >>= k = EP $ \l0 c0 -> let
        (a, l1, c1, s1) = m l0 c0
        EP f = k a
        (b, l2, c2, s2) = f l1 c1
    in (b, l2, c2, s1 . s2)

runEP :: EP () -> [Comment] -> String
runEP (EP f) cs = let (_,_,_,s) = f (1,1) cs in s ""

getPos :: EP Pos
getPos = EP (\l cs -> (l,l,cs,id))

setPos :: Pos -> EP ()
setPos l = EP (\_ cs -> ((),l,cs,id))

printString :: String -> EP ()
printString str = EP (\(l,c) cs -> ((), (l,c+length str), cs, showString str))

getComment :: EP (Maybe Comment)
getComment = EP $ \l cs ->
    let x = case cs of
             c:_ -> Just c
             _   -> Nothing
     in (x, l, cs, id)

dropComment :: EP ()
dropComment = EP $ \l cs ->
    let cs' = case cs of
               (_:cs) -> cs
               _      -> cs
     in ((), l, cs', id)

newLine :: EP ()
newLine = do
    (l,_) <- getPos
    printString "\n"
    setPos (l+1,1)

padUntil :: Pos -> EP ()
padUntil (l,c) = do
    (l1,c1) <- getPos
    case  {- trace (show ((l,c), (l1,c1))) -} () of
     _ {-()-} | l1 >= l && c1 <= c -> printString $ replicate (c - c1) ' '
              | l1 < l             -> newLine >> padUntil (l,c)
              | otherwise          -> return ()


mPrintComments :: Pos -> EP ()
mPrintComments p = do
    mc <- getComment
    case mc of
     Nothing -> return ()
     Just (Comment multi s str) ->
        when (pos s < p) $ do
            dropComment
            padUntil (pos s)
            printComment multi str
            setPos (srcSpanEndLine s, srcSpanEndColumn s)
            mPrintComments p

printComment :: Bool -> String -> EP ()
printComment b str
    | b         = printString $ "{-" ++ str ++ "-}"
    | otherwise = printString $ "--" ++ str

printWhitespace :: Pos -> EP ()
printWhitespace p = mPrintComments p >> padUntil p

printStringAt :: Pos -> String -> EP ()
printStringAt p str = printWhitespace p >> printString str

------------------------------------------------------------------------------
-- Printing of source elements

exactPrint :: (ExactP ast) => ast SrcSpanInfo -> [Comment] -> String
exactPrint ast cs = runEP (exactP ast) cs

exactPC :: (Annotated ast, ExactP ast) => ast SrcSpanInfo -> EP ()
exactPC ast = let p = pos (ann ast) in mPrintComments p >> padUntil p >> exactP ast

printSeq :: [(Pos, EP ())] -> EP ()
printSeq [] = return ()
printSeq ((p,pr):xs) = printWhitespace p >> pr >> printSeq xs

printStrs :: SrcInfo loc => [(loc, String)] -> EP ()
printStrs = printSeq . map (\(loc, str) -> (pos loc, printString str))

printPoints :: SrcSpanInfo -> [String] -> EP ()
printPoints l = printStrs . zip (srcInfoPoints l)

printInterleaved, printInterleaved' :: (Annotated ast, ExactP ast, SrcInfo loc) => [(loc, String)] -> [ast SrcSpanInfo] -> EP ()
printInterleaved sistrs asts = printSeq $
    interleave (map (\(loc, str) -> (pos loc, printString str)) sistrs)
               (map (\a -> (pos $ ann a, exactP a)) asts)

printInterleaved' sistrs (a:asts) = exactPC a >> printInterleaved sistrs asts

printStreams :: [(Pos, EP ())] -> [(Pos, EP ())] -> EP ()
printStreams [] ys = printSeq ys
printStreams xs [] = printSeq xs
printStreams (x@(p1,ep1):xs) (y@(p2,ep2):ys)
    | p1 <= p2 = printWhitespace p1 >> ep1 >> printStreams xs (y:ys)
    | otherwise = printWhitespace p2 >> ep2 >> printStreams (x:xs) ys


interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x:y: interleave xs ys

maybeEP :: (a -> EP ()) -> Maybe a -> EP ()
maybeEP = maybe (return ())

bracketList :: (Annotated ast, ExactP ast) => (String, String, String) -> [SrcSpan] -> [ast SrcSpanInfo] -> EP ()
bracketList (a,b,c) poss asts = printInterleaved (pList poss (a,b,c)) asts

pList (p:ps) (a,b,c) = (p,a) : pList' ps (b,c)
pList' [] _ = []
pList' [p] (_,c) = [(p,c)]
pList' (p:ps) (b,c) = (p, b) : pList' ps (b,c)

parenList, squareList, curlyList :: (Annotated ast, ExactP ast) => [SrcSpan] -> [ast SrcSpanInfo] -> EP ()
parenList = bracketList ("(",",",")")
squareList = bracketList ("[",",","]")
curlyList = bracketList ("{",",","}")

layoutList :: (Functor ast, Show (ast ()), Annotated ast, ExactP ast) => [SrcSpan] -> [ast SrcSpanInfo] -> EP ()
layoutList poss asts = printInterleaved (lList poss) asts

lList (p:ps) = (if isNullSpan p then (p,"") else (p,"{")) : lList' ps
lList' [] = []
lList' [p] = [if isNullSpan p then (p,"") else (p,"}")]
lList' (p:ps) = (if isNullSpan p then (p,"") else (p,";")) : lList' ps


--------------------------------------------------
-- Exact printing

class ExactP ast where
  exactP :: ast SrcSpanInfo -> EP ()

instance ExactP Literal where
  exactP lit = case lit of
    Char       _ _ rw -> printString ('\'':rw ++ "\'")
    String     _ _ rw -> printString ('\"':rw ++ "\"")
    Int        _ _ rw -> printString (rw)
    Frac       _ _ rw -> printString (rw)
    PrimInt    _ _ rw -> printString (rw ++ "#" )
    PrimWord   _ _ rw -> printString (rw ++ "##")
    PrimFloat  _ _ rw -> printString (rw ++ "#" )
    PrimDouble _ _ rw -> printString (rw ++ "##")
    PrimChar   _ _ rw -> printString ('\'':rw ++ "\'#" )
    PrimString _ _ rw -> printString ('\"':rw ++ "\"#" )

instance ExactP ModuleName where
  exactP (ModuleName l str) = printString str

instance ExactP SpecialCon where
  exactP sc = case sc of
    UnitCon l   -> printPoints l ["(",")"]
    ListCon l   -> printPoints l ["[","]"]
    FunCon  l   -> printPoints l ["(","->",")"]
    TupleCon l b n -> printPoints l $
        case b of
         Unboxed -> "(#": replicate n "," ++ ["#)"]
         _       -> "(" : replicate n "," ++ [")"]
    Cons l      -> printString ":"
    UnboxedSingleCon l -> printPoints l ["(#","#)"]

isSymbol :: Name l -> Bool
isSymbol (Symbol _ _) = True
isSymbol _ = False

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

instance ExactP QName where
  exactP qn
    | isSymbol (getName qn) = do
        let [a,b,c] = srcInfoPoints (ann qn)
        printString "("
        printWhitespace (pos b)
        epQName qn
        printStringAt (pos c) ")"
    | otherwise = epQName qn

epQName :: QName SrcSpanInfo -> EP ()
epQName qn = case qn of
    Qual    l mn n  -> exactP mn >> printString "." >> epName n
    UnQual  l    n  -> epName n
    Special l sc    -> exactP sc

epInfixQName :: QName SrcSpanInfo -> EP ()
epInfixQName qn
    | isSymbol (getName qn) = printWhitespace (pos (ann qn)) >> epQName qn
    | otherwise = do
        let [a,b,c] = srcInfoPoints (ann qn)
        printStringAt (pos a) "`"
        printWhitespace (pos b)
        epQName qn
        printStringAt (pos c) "`"

instance ExactP Name where
  exactP n = case n of
    Ident  l str    -> printString str
    Symbol l str    -> do
        let [a,b,c] = srcInfoPoints l
        printString "("
        printWhitespace (pos b)
        printString str
        printStringAt (pos c) ")"

epName :: Name SrcSpanInfo -> EP ()
epName (Ident  _ str) = printString str
epName (Symbol _ str) = printString str

epInfixName :: Name SrcSpanInfo -> EP ()
epInfixName n
    | isSymbol n = printWhitespace (pos (ann n)) >> epName n
    | otherwise = do
        let [a,b,c] = srcInfoPoints (ann n)
        printStringAt (pos a) "`"
        printWhitespace (pos b)
        epName n
        printStringAt (pos c) "`"

instance ExactP IPName where
  exactP ipn = case ipn of
    IPDup l str -> printString $ '?':str
    IPLin l str -> printString $ '%':str

instance ExactP QOp where
  exactP qop = case qop of
    QVarOp l qn -> epInfixQName qn
    QConOp l qn -> epInfixQName qn

instance ExactP Op where
  exactP op = case op of
    VarOp l n -> epInfixName n
    ConOp l n -> epInfixName n



instance ExactP CName where
  exactP cn = case cn of
    VarName l n -> exactP n
    ConName l n -> exactP n

instance ExactP ExportSpec where
  exactP espec = case espec of
     EVar l qn      -> exactP qn
     EAbs l qn      -> exactP qn
     EThingAll l qn -> exactP qn >> printPoints l ["(","..",")"]
     EThingWith l qn cns    ->
        let k = length (srcInfoPoints l)
         in exactP qn >> printInterleaved (zip (srcInfoPoints l) $ "(":replicate (k-2) "," ++ [")"]) cns
     EModuleContents l mn -> printString "module" >> exactPC mn

instance ExactP ExportSpecList where
  exactP (ExportSpecList l ess) =
    let k = length (srcInfoPoints l)
     in printInterleaved (zip (srcInfoPoints l) $ "(": replicate (k-2) "," ++ [")"]) ess

instance ExactP ImportSpecList where
  exactP (ImportSpecList l hid ispecs) = do
    let pts = srcInfoPoints l
    pts <- if hid then do
             let (x:pts') = pts
             printStringAt (pos x) "hiding"
             return pts'
            else return pts
    let k = length pts
    printInterleaved (zip pts $ "(": replicate (k-2) "," ++ [")"]) ispecs

instance ExactP ImportSpec where
  exactP ispec = case ispec of
    IVar l n    -> exactP n
    IAbs l n    -> exactP n
    IThingAll l n   -> exactP n >> printPoints l ["(","..",")"]
    IThingWith l n cns    ->
        let k = length (srcInfoPoints l)
         in exactP n >> printInterleaved (zip (srcInfoPoints l) $ "(":replicate (k-2) "," ++ [")"]) cns

instance ExactP ImportDecl where
  exactP (ImportDecl l mn qf src mpkg mas mispecs) = do
    printString "import"
    let (a:pts) = srcInfoPoints l
    pts <- if src then do
             let (x:y:pts') = pts
             printStringAt (pos x) "{-# SOURCE"
             printStringAt (pos y) "#-}"
             return pts'
            else return pts
    pts <- if qf then do
             let (x:pts') = pts
             printStringAt (pos x) "qualified"
             return pts'
            else return pts
    pts <- case mpkg of
            Just pkg -> do
              let (x:pts') = pts
              printStringAt (pos x) $ show pkg
              return pts'
            _ -> return pts
    exactPC mn
    pts <- case mas of
            Just as -> do
             let (x:pts') = pts
             printStringAt (pos x) "as"
             exactPC as
             return pts'
            _ -> return pts
    case mispecs of
     Nothing -> return ()
     Just ispecs -> exactPC ispecs

instance ExactP Module where
  exactP mdl = case mdl of
    Module l mmh oss ids decls -> do
        let (oPts, pts) = splitAt (max (length oss + 1) 2) (srcInfoPoints l)
        layoutList oPts oss
        maybeEP exactPC mmh
        printStreams (map (\(p,s) -> (pos p, printString s)) $ lList pts)
                     (map (\i -> (pos $ ann i, exactPC i)) ids ++ map (\d -> (pos $ ann d, exactPC d)) (sepFunBinds decls))
    XmlPage l _mn oss xn attrs mat es  -> do
        let (oPts, [a,b,c,d,e]) = splitAt (max (length oss + 1) 2) $ srcInfoPoints l
        layoutList oPts oss
        printStringAt (pos a) "<"
        exactPC xn
        mapM_ exactPC attrs
        maybeEP exactPC mat
        printStringAt (pos b) ">"
        mapM_ exactPC es
        printStringAt (pos c) "</"
        printWhitespace (pos d)
        exactP xn
        printStringAt (pos e) ">"
    XmlHybrid l mmh oss ids decls xn attrs mat es -> do
        let (oPts, pts) = splitAt (max (length oss + 1) 2) (srcInfoPoints l)
        layoutList oPts oss
        maybeEP exactPC mmh
        let (dPts, [a,b,c,d,e]) = splitAt (length pts - 5) pts
        printStreams (map (\(p,s) -> (pos p, printString s)) $ lList dPts)
                     (map (\i -> (pos $ ann i, exactPC i)) ids ++ map (\d -> (pos $ ann d, exactPC d)) (sepFunBinds decls))

        printStringAt (pos a) "<"
        exactPC xn
        mapM_ exactPC attrs
        maybeEP exactPC mat
        printStringAt (pos b) ">"
        mapM_ exactPC es
        printStringAt (pos c) "</"
        printWhitespace (pos d)
        exactP xn
        printStringAt (pos e) ">"

instance ExactP ModuleHead where
  exactP (ModuleHead l mn mwt mess) = do
    let [a,b] = srcInfoPoints l
    printStringAt (pos a) "module"
    exactPC mn
    maybeEP exactPC mwt
    maybeEP exactPC mess
    printStringAt (pos b) "where"

instance ExactP OptionPragma where
  exactP op = case op of
    LanguagePragma   l ns       ->
        let pts = srcInfoPoints l
            k = length ns - 1 -- number of commas
            m = length pts - k - 2 -- number of virtual semis, likely 0
         in printInterleaved (zip pts ("{-# LANGUAGE":replicate k "," ++ replicate m "" ++ ["#-}"])) ns
    IncludePragma    l str      ->
        let k = length (srcInfoPoints l)
         in printPoints l $ ("{-# INCLUDE " ++ str) : replicate (k-2) "" ++ ["#-}"]
    CFilesPragma     l str      ->
        let k = length (srcInfoPoints l)
         in printPoints l $ ("{-# CFILES " ++ str) : replicate (k-2) "" ++ ["#-}"]
    OptionsPragma    l mt str   ->
        let k = length (srcInfoPoints l)
            opstr = "{-# OPTIONS" ++ case mt of { Just t -> "_" ++ show t ; _ -> "" } ++ " " ++ str
         in printPoints l $ opstr : replicate (k-2) "" ++ ["#-}"]

instance ExactP WarningText where
    exactP (DeprText l str) = printPoints l ["{-# DEPRECATED", str, "#-}"]
    exactP (WarnText l str) = printPoints l ["{-# WARNING",    str, "#-}"]

instance ExactP Assoc where
  exactP a = case a of
    AssocNone  l -> printString "infix"
    AssocLeft  l -> printString "infixl"
    AssocRight l -> printString "infixr"

instance ExactP DataOrNew where
  exactP (DataType l) = printString "data"
  exactP (NewType  l) = printString "newtype"

instance ExactP Decl where
  exactP decl = case decl of
    TypeDecl     l dh t      -> do
        let [a,b] = srcInfoPoints l
        printStringAt (pos a) "type"
        exactPC dh
        printStringAt (pos b) "="
        exactPC t
    TypeFamDecl  l dh mk     -> do
        let (a:b:ps) = srcInfoPoints l
        printStringAt (pos a) "type"
        printStringAt (pos b) "family"
        exactPC dh
        maybeEP (\k -> printStringAt (pos (head ps)) "::" >> exactPC k) mk
    DataDecl     l dn mctxt dh constrs mder -> do
        exactP dn
        maybeEP exactPC mctxt
        exactPC dh
        -- the next line works for empty data types since the srcInfoPoints will be empty then
        printInterleaved (zip (srcInfoPoints l) ("=": repeat "|")) constrs
        maybeEP exactPC mder
    GDataDecl    l dn mctxt dh mk gds mder -> do
        let pts = srcInfoPoints l
        exactP dn
        maybeEP exactPC mctxt
        exactPC dh
        (x:pts) <- case mk of
                    Nothing -> return pts
                    Just kd -> let (p:pts') = pts in do
                        printStringAt (pos p) "::"
                        exactPC kd
                        return pts'
        printStringAt (pos x) "where"
        layoutList pts gds
        maybeEP exactPC mder
    DataFamDecl  l mctxt dh mk -> do
        printString "data"
        maybeEP exactPC mctxt
        exactPC dh
        maybeEP (\kd -> printStringAt (pos (head (srcInfoPoints l))) "::" >> exactPC kd) mk
    TypeInsDecl  l t1 t2 -> do
        let [a,b,c] = srcInfoPoints l
        printString "type"
        printStringAt (pos b) "instance"
        exactPC t1
        printStringAt (pos c) "="
        exactPC t2
    DataInsDecl  l dn t constrs mder    -> do
        let (p:pts) = srcInfoPoints l
        exactP dn
        printStringAt (pos p) "instance"
        exactPC t
        printInterleaved (zip (srcInfoPoints l) ("=": repeat "|")) constrs
        maybeEP exactPC mder
    GDataInsDecl l dn t mk gds mder     -> do
        let (p:pts) = srcInfoPoints l
        exactP dn
        printStringAt (pos p) "instance"
        exactPC t
        (x:pts) <- case mk of
                    Nothing -> return pts
                    Just kd -> let (p:pts') = pts in do
                        printStringAt (pos p) "::"
                        exactPC kd
                        return pts'
        printStringAt (pos x) "where"
        layoutList pts gds
        maybeEP exactPC mder
    ClassDecl    l mctxt dh fds mcds    -> do
        let (a:pts) = srcInfoPoints l
        printString "class"
        maybeEP exactPC mctxt
        exactPC dh
        pts <- case fds of
                [] -> return pts
                _  -> do
                  let (pts1, pts2) = splitAt (length fds) pts
                  printInterleaved (zip pts1 ("|":repeat ",")) fds
                  return pts2
        maybeEP (\cds -> do
            let (p:pts') = pts
            printStringAt (pos p) "where"
            layoutList pts' $ sepClassFunBinds cds
            ) mcds
    InstDecl     l mctxt ih mids        -> do
        let (a:pts) = srcInfoPoints l
        printString "instance"
        maybeEP exactPC mctxt
        exactPC ih
        maybeEP (\ids -> do
            let (p:pts') = pts
            printStringAt (pos p) "where"
            layoutList pts' $ sepInstFunBinds ids
            ) mids
    DerivDecl    l mctxt ih             -> do
        let [a,b] = srcInfoPoints l
        printString "deriving"
        printStringAt (pos b) "instance"
        maybeEP exactPC mctxt
        exactPC ih
    InfixDecl    l assoc mprec ops      -> do
        let pts = srcInfoPoints l
        exactP assoc
        pts <- case mprec of
                Nothing -> return pts
                Just prec -> do
                    let (p:pts') = pts
                    printStringAt (pos p) (show prec)
                    return pts'
        printInterleaved' (zip pts (repeat ",")) ops
    DefaultDecl  l ts   -> do
        let (a:pts) = srcInfoPoints l
        printString "default"
        printInterleaved (zip (init pts) ("(":repeat ",")) ts
        printStringAt (pos (last pts)) ")"
    SpliceDecl   l spl  -> exactP spl
    TypeSig      l ns t -> do
        let pts = srcInfoPoints l
        printInterleaved' (zip pts (replicate (length pts - 1) "," ++ ["::"])) ns
        exactPC t
    FunBind      l ms   -> mapM_ exactPC ms
    PatBind      l p mt rhs mbs -> do
        let pts = srcInfoPoints l
        exactP p
        pts <- case mt of
                Nothing -> return pts
                Just t  -> let (x:pts') = pts in do
                    printStringAt (pos x) "::"
                    exactPC t
                    return pts'
        exactPC rhs
        maybeEP (\bs -> printStringAt (pos (head pts)) "where" >> exactPC bs) mbs
    ForImp       l cc msf mstr n t   -> do
        let (a:b:pts) = srcInfoPoints l
        printString "foreign"
        printStringAt (pos b) "import"
        exactPC cc
        maybeEP exactPC msf
        (y:_) <- case mstr of
                  Nothing -> return pts
                  Just str -> let (x:pts') = pts in do
                      printStringAt (pos x) (show str)
                      return pts'
        exactPC n
        printStringAt (pos y) "::"
        exactPC t
    ForExp       l cc mstr n t      -> do
        let (a:b:pts) = srcInfoPoints l
        printString "foreign"
        printStringAt (pos b) "export"
        exactPC cc
        (y:_) <- case mstr of
                  Nothing -> return pts
                  Just str -> let (x:pts') = pts in do
                      printStringAt (pos x) (show str)
                      return pts'
        exactPC n
        printStringAt (pos y) "::"
        exactPC t
    RulePragmaDecl   l rs   -> do
        let [a,b] = srcInfoPoints l
        printString "{-# RULES"
        mapM_ exactPC rs
        printStringAt (pos b) "#-}"
    DeprPragmaDecl   l nstrs -> do
        let pts = srcInfoPoints l
        printString "{-# DEPRECATED"
        printWarndeprs (map pos (init pts)) nstrs
        printStringAt (pos (last pts)) "#-}"
    WarnPragmaDecl   l nstrs -> do
        let pts = srcInfoPoints l
        printString "{-# WARNING"
        printWarndeprs (map pos (init pts)) nstrs
        printStringAt (pos (last pts)) "#-}"
    InlineSig        l inl mact qn    -> do
        let [a,b] = srcInfoPoints l
        printString $ if inl then "{-# INLINE" else "{-# NOINLINE"
        maybeEP exactPC mact
        exactPC qn
        printStringAt (pos b) "#-}"
    SpecSig          l qn ts        -> do
        let (a:pts) = srcInfoPoints l
        printString "{-# SPECIALISE"
        exactPC qn
        printInterleaved (zip pts ("::" : repeat "," ++ ["#-}"])) ts
    SpecInlineSig    l b mact qn ts -> do
        let (a:pts) = srcInfoPoints l
        printString $ "{-# SPECIALISE " ++ if b then "INLINE" else "NOINLINE"
        maybeEP exactPC mact
        exactPC qn
        printInterleaved (zip pts ("::" : repeat "," ++ ["#-}"])) ts
    InstSig          l mctxt ih     -> do
        let [a,b,c] = srcInfoPoints l
        printString $ "{-# SPECIALISE"
        printStringAt (pos b) "instance"
        maybeEP exactPC mctxt
        exactPC ih
        printStringAt (pos c) "#-}"

printWarndeprs :: [Pos] -> [([Name SrcSpanInfo], String)] -> EP ()
printWarndeprs _ [] = return ()
printWarndeprs ps ((ns,str):nsts) = printWd ps ns str nsts
  where printWd :: [Pos] -> [Name SrcSpanInfo] -> String -> [([Name SrcSpanInfo], String)] -> EP ()
        printWd (p:ps) []  str nsts = printStringAt p (show str) >> printWarndeprs ps nsts
        printWd ps     [n] str nsts = exactPC n >> printWd ps [] str nsts
        printWd (p:ps) (n:ns) str nsts = exactPC n >> printStringAt p "," >> printWd ps ns str nsts


sepFunBinds :: [Decl SrcSpanInfo] -> [Decl SrcSpanInfo]
sepFunBinds [] = []
sepFunBinds (FunBind _ ms:ds) = map (\m -> FunBind (ann m) [m]) ms ++ sepFunBinds ds
sepFunBinds (d:ds) = d : sepFunBinds ds

sepClassFunBinds :: [ClassDecl SrcSpanInfo] -> [ClassDecl SrcSpanInfo]
sepClassFunBinds [] = []
sepClassFunBinds (ClsDecl _ (FunBind _ ms):ds) = map (\m -> ClsDecl (ann m) $ FunBind (ann m) [m]) ms ++ sepClassFunBinds ds
sepClassFunBinds (d:ds) = d : sepClassFunBinds ds

sepInstFunBinds :: [InstDecl SrcSpanInfo] -> [InstDecl SrcSpanInfo]
sepInstFunBinds [] = []
sepInstFunBinds (InsDecl _ (FunBind _ ms):ds) = map (\m -> InsDecl (ann m) $ FunBind (ann m) [m]) ms ++ sepInstFunBinds ds
sepInstFunBinds (d:ds) = d : sepInstFunBinds ds

instance ExactP DeclHead where
  exactP dh = case dh of
    DHead l n tvs       -> exactP n >> mapM_ exactPC tvs
    DHInfix l tva n tvb -> exactP tva >> epInfixName n >> exactPC tvb
    DHParen l dh        ->
        let [_,b] = srcInfoPoints l
         in printString "(" >> exactPC dh >> printStringAt (pos b) ")"

instance ExactP InstHead where
  exactP ih = case ih of
    IHead l qn ts       -> exactP qn >> mapM_ exactPC ts
    IHInfix l ta qn tb  -> exactP ta >> epInfixQName qn >> exactPC tb
    IHParen l ih        ->
        let [_,b] = srcInfoPoints l
         in printString "(" >> exactPC ih >> printStringAt (pos b) ")"

instance ExactP TyVarBind where
  exactP (KindedVar   l n k) = do
        let [a,b,c] = srcInfoPoints l
        printString "("
        exactPC n
        printStringAt (pos b) "::"
        exactPC k
        printStringAt (pos c) ")"
  exactP (UnkindedVar l n) = exactP n

instance ExactP Kind where
  exactP kd = case kd of
    KindStar  l     -> printString "*"
    KindBang  l     -> printString "!"
    KindFn    l k1 k2 -> do
        let [a] = srcInfoPoints l
        exactP k1
        printStringAt (pos a) "->"
        exactPC k2
    KindParen l kd  -> do
        let [a,b] = srcInfoPoints l
        printString "("
        exactPC kd
        printStringAt (pos b) ")"
    KindVar l n     -> exactP n       

instance ExactP Type where
  exactP t = case t of
    TyForall l mtvs mctxt t -> do
        let pts = srcInfoPoints l
        pts <- case mtvs of
                Nothing -> return pts
                Just tvs -> do
                    let (a:b:pts') = pts
                    printString "forall"
                    mapM_ exactPC tvs
                    printStringAt (pos b) "."
                    return pts'
        maybeEP exactPC mctxt
        exactPC t
    TyFun   l t1 t2 -> do
        let [a] = srcInfoPoints l
        exactP t1
        printStringAt (pos a) "->"
        exactPC t2
    TyTuple l bx ts -> do
        let pts = srcInfoPoints l
            (o,e) = case bx of
                     Boxed   -> ("(" , ")")
                     Unboxed -> ("(#","#)")
        printInterleaved (zip pts (o: replicate (length pts - 2) "," ++ [e])) ts
    TyList  l t     -> do
        let [a,b] = srcInfoPoints l
        printString "["
        exactPC t
        printStringAt (pos b) "]"
    TyApp   l t1 t2 -> exactP t1 >> exactPC t2
    TyVar   l n     -> exactP n
    TyCon   l qn    -> exactP qn
    TyParen l t     -> do
        let [a,b] = srcInfoPoints l
        printString "("
        exactPC t
        printStringAt (pos b) ")"
    TyInfix l t1 qn t2 -> exactP t1 >> epInfixQName qn >> exactPC t2
    TyKind  l t kd -> do
        let [a,b,c] = srcInfoPoints l
        printString "("
        exactPC t
        printStringAt (pos b) "::"
        exactPC kd
        printStringAt (pos c) ")"

instance ExactP Context where
  exactP ctxt = do
    printContext ctxt
    printStringAt (pos (last (srcInfoPoints (ann ctxt)))) "=>"

printContext ctxt = do
    let l = ann ctxt
        pts = init $ srcInfoPoints l
    case ctxt of
     CxParen l ctxt -> do
        let [a,b] = pts
        printStringAt (pos a) "("
        printContext ctxt
        printStringAt (pos b) ")"
     CxSingle l asst -> exactP asst
     CxEmpty l -> do
        let [a,b] = pts
        printStringAt (pos a) "("
        printStringAt (pos b) ")"
     CxTuple l assts -> parenList pts assts


instance ExactP Asst where
  exactP asst = case asst of
    ClassA l qn ts -> exactP qn >> mapM_ exactPC ts
    InfixA l ta qn tb -> exactP ta >> epInfixQName qn >> exactPC tb
    IParam l ipn t    -> do
        let [a] = srcInfoPoints l
        exactP ipn
        printStringAt (pos a) "::"
        exactPC t
    EqualP l t1 t2  -> do
        let [a] = srcInfoPoints l
        exactP t1
        printStringAt (pos a) "~"
        exactPC t2

instance ExactP Deriving where
  exactP (Deriving l ihs) = do
    let (x:pts) = srcInfoPoints l
    printString "deriving"
    case pts of
     [] -> exactPC $ head ihs
     _  -> parenList pts ihs

instance ExactP ClassDecl where
  exactP cdecl = case cdecl of
    ClsDecl    l d -> exactP d
    ClsDataFam l mctxt dh mk -> do
        let (x:pts) = srcInfoPoints l
        printString "data"
        maybeEP exactPC mctxt
        exactPC dh
        maybeEP (\kd -> printStringAt (pos (head pts)) "::" >> exactPC kd) mk
    ClsTyFam   l dh mk  -> do
        let (x:pts) = srcInfoPoints l
        printString "type"
        exactPC dh
        maybeEP (\kd -> printStringAt (pos (head pts)) "::" >> exactPC kd) mk
    ClsTyDef   l t1 t2  -> do
        let [a,b] = srcInfoPoints l
        printString "type"
        exactPC t1
        printStringAt (pos b) "="
        exactPC t2

instance ExactP InstDecl where
  exactP idecl = case idecl of
    InsDecl   l d -> exactP d
    InsType   l t1 t2 -> do
        let [a,b] = srcInfoPoints l
        printString "type"
        exactPC t1
        printStringAt (pos b) "="
        exactPC t2
    InsData   l dn t constrs mder -> do
        exactP dn
        exactPC t
        printInterleaved (zip (srcInfoPoints l) ("=": repeat "|")) constrs
        maybeEP exactPC mder
    InsGData  l dn t mk gds mder  -> do
        let pts = srcInfoPoints l
        exactP dn
        exactPC t
        (x:pts) <- case mk of
                    Nothing -> return pts
                    Just kd -> let (p:pts') = pts in do
                        printStringAt (pos p) "::"
                        exactPC kd
                        return pts'
        printStringAt (pos x) "where"
        mapM_ exactPC gds
        maybeEP exactPC mder
    InsInline l inl mact qn   -> do
        let [a,b] = srcInfoPoints l
        printString $ if inl then "{-# INLINE" else "{-# NOINLINE"
        maybeEP exactPC mact
        exactPC qn
        printStringAt (pos b) "#-}"

instance ExactP FunDep where
  exactP (FunDep l nxs nys) = do
    let [a] = srcInfoPoints l
    mapM_ exactPC nxs
    printStringAt (pos a) "->"
    mapM_ exactPC nys

instance ExactP QualConDecl where
  exactP (QualConDecl l mtvs mctxt cd) = do
        let pts = srcInfoPoints l
        pts <- case mtvs of
                Nothing -> return pts
                Just tvs -> do
                    let (a:b:pts') = pts
                    printString "forall"
                    mapM_ exactPC tvs
                    printStringAt (pos b) "."
                    return pts'
        maybeEP exactPC mctxt
        exactPC cd

instance ExactP ConDecl where
  exactP cd = case cd of
    ConDecl l n bts -> exactP n >> mapM_ exactPC bts
    InfixConDecl l bta n btb -> exactP bta >> epInfixName n >> exactP btb
    RecDecl l n fds -> exactP n >> curlyList (srcInfoPoints l) fds

instance ExactP GadtDecl where
  exactP (GadtDecl l n t) = do
    let [a] = srcInfoPoints l
    exactP n
    printStringAt (pos a) "::"
    exactPC t

instance ExactP BangType where
  exactP bt = case bt of
    UnBangedTy l t  -> exactP t
    BangedTy   l t  -> printString "!" >> exactPC t
    UnpackedTy l t  -> do
      let [a,b,c] = srcInfoPoints l
      printString "{-# UNPACK"
      printStringAt (pos b) "#-}"
      printStringAt (pos c) "!"
      exactPC t

instance ExactP Splice where
  exactP (IdSplice l str) = printString $ '$':str
  exactP (ParenSplice l e) = do
    let [a,b] = srcInfoPoints l
    printString "$("
    exactPC e
    printStringAt (pos b) ")"

instance ExactP Exp where
  exactP exp = case exp of
    Var l qn        -> exactP qn
    IPVar l ipn     -> exactP ipn
    Con l qn        -> exactP qn
    Lit l lit       -> exactP lit
    InfixApp l e1 op e2 -> exactP e1 >> exactPC op >> exactPC e2
    App l e1 e2     -> exactP e1 >> exactPC e2
    NegApp l e      -> printString "-" >> exactPC e
    Lambda l ps e   -> do
        let [a,b] = srcInfoPoints l
        printString "\\"
        mapM_ exactPC ps
        printStringAt (pos b) "->"
        exactPC e
    Let l bs e      -> do
        let [a,b] = srcInfoPoints l
        printString "let"
        exactPC bs
        printStringAt (pos b) "in"
        exactPC e
    If l ec et ee   -> do
        let [a,b,c] = srcInfoPoints l
        printString "if"
        exactPC ec
        printStringAt (pos b) "then"
        exactPC et
        printStringAt (pos c) "else"
        exactPC ee
    Case l e alts   -> do
        let (a:b:pts) = srcInfoPoints l
        printString "case"
        exactPC e
        printStringAt (pos b) "of"
        layoutList pts alts
    Do l stmts      -> do
        let (a:pts) = srcInfoPoints l
        printString "do"
        layoutList pts stmts
    MDo l stmts     -> do
        let (a:pts) = srcInfoPoints l
        printString "mdo"
        layoutList pts stmts
    Tuple l es      -> parenList (srcInfoPoints l) es
    TupleSection l mexps    -> do
        let pts = srcInfoPoints l
        printSeq $ interleave (zip (map pos $ init pts) (map printString ("(": repeat ",")) ++ [(pos $ last pts, printString ")")])
                              (map (\me -> (maybe (0,0) (pos . ann) me, maybeEP exactPC me)) mexps)
    List l es               -> squareList (srcInfoPoints l) es
    Paren l p               -> parenList (srcInfoPoints l) [p]
    LeftSection l e qop     -> do
        let [a,b] = srcInfoPoints l
        printString "("
        exactPC e
        exactPC qop
        printStringAt (pos b) ")"
    RightSection l qop e    -> do
        let [a,b] = srcInfoPoints l
        printString "("
        exactPC qop
        exactPC e
        printStringAt (pos b) ")"
    RecConstr l qn fups     -> do
        let pts = srcInfoPoints l
        exactP qn
        curlyList pts fups
    RecUpdate l e fups      -> do
        let pts = srcInfoPoints l
        exactP e
        curlyList pts fups
    EnumFrom l e            -> do
        let [a,b,c] = srcInfoPoints l
        printString "["
        exactPC e
        printStringAt (pos b) ".."
        printStringAt (pos c) "]"
    EnumFromTo l e1 e2      -> do
        let [a,b,c] = srcInfoPoints l
        printString "["
        exactPC e1
        printStringAt (pos b) ".."
        exactPC e2
        printStringAt (pos c) "]"
    EnumFromThen l e1 e2    -> do
        let [a,b,c,d] = srcInfoPoints l
        printString "["
        exactPC e1
        printStringAt (pos b) ","
        exactPC e2
        printStringAt (pos c) ".."
    EnumFromThenTo l e1 e2 e3   -> do
        let [a,b,c,d] = srcInfoPoints l
        printString "["
        exactPC e1
        printStringAt (pos b) ","
        exactPC e2
        printStringAt (pos c) ".."
        exactPC e3
        printStringAt (pos d) "]"
    ListComp l e qss            -> do
        let (a:pts) = srcInfoPoints l
        printString "["
        exactPC e
        bracketList ("|",",","]") pts qss
    ParComp  l e qsss           -> do
        let (a:pts) = srcInfoPoints l
            (strs, qss) = unzip $ pairUp qsss
        printString "["
        exactPC e
        printInterleaved (zip pts (strs ++ ["]"])) qss
      where pairUp [] = []
            pairUp ((a:as):xs) = ("|", a) : zip (repeat ",") as ++ pairUp xs

    ExpTypeSig l e t    -> do
        let [a] = srcInfoPoints l
        exactP e
        printStringAt (pos a) "::"
        exactPC t
    VarQuote l qn   -> do
      printString "'"
      exactPC qn
    TypQuote l qn -> do
      printString "''"
      exactPC qn
    BracketExp l br -> exactP br
    SpliceExp l sp  -> exactP sp
    QuasiQuote l name qt    -> printString $ "[$" ++ name ++ "|" ++ qt ++ "]"
    XTag l xn attrs mat es  -> do
        let [a,b,c,d,e] = srcInfoPoints l
        printString "<"
        exactPC xn
        mapM_ exactPC attrs
        maybeEP exactPC mat
        printStringAt (pos b) ">"
        mapM_ exactPC es
        printStringAt (pos c) "</"
        printWhitespace (pos d)
        exactP xn
        printStringAt (pos e) ">"
    XETag l xn attrs mat    -> do
        let [a,b] = srcInfoPoints l
        printString "<"
        exactPC xn
        mapM_ exactPC attrs
        maybeEP exactPC mat
        printStringAt (pos b) "/>"
    XPcdata l str   -> printString str
    XExpTag l e     -> do
        let [a,b] = srcInfoPoints l
        printString "<%"
        exactPC e
        printString "%>"

    CorePragma l      str e         -> do
        let [a,b] = srcInfoPoints l
        printString $ "{-# CORE " ++ show str
        printStringAt (pos b) "#-}"
        exactPC e
    SCCPragma  l      str e         -> do
        let [a,b] = srcInfoPoints l
        printString $ "{-# SCC " ++ show str
        printStringAt (pos b) "#-}"
        exactPC e
    GenPragma  l      str (i1,i2) (i3,i4) e -> do
        printStrs $ zip (srcInfoPoints l) ["{-# GENERATED", show str, show i1, ":", show i2, "-", show i3, ":", show i4, "#-}"]
        exactPC e
    Proc            l p e   -> do
        let [a,b] = srcInfoPoints l
        printString "proc"
        exactPC p
        printStringAt (pos b) "->"
        exactPC e
    LeftArrApp      l e1 e2 -> do
        let [a] = srcInfoPoints l
        exactP e1
        printStringAt (pos a) "-<"
        exactPC e2
    RightArrApp     l e1 e2 -> do
        let [a] = srcInfoPoints l
        exactP e1
        printStringAt (pos a) ">-"
        exactPC e2
    LeftArrHighApp  l e1 e2 -> do
        let [a] = srcInfoPoints l
        exactP e1
        printStringAt (pos a) "-<<"
        exactPC e2
    RightArrHighApp l e1 e2 -> do
        let [a] = srcInfoPoints l
        exactP e1
        printStringAt (pos a) ">>-"
        exactPC e2

instance ExactP FieldUpdate where
  exactP fup = case fup of
    FieldUpdate l qn e  -> do
      let [a] = srcInfoPoints l
      exactP qn
      printStringAt (pos a) "="
      exactPC e
    FieldPun l n    -> exactP n
    FieldWildcard l -> printString ".."

instance ExactP Stmt where
  exactP stmt = case stmt of
    Generator l p e -> do
      let [a] = srcInfoPoints l
      exactP p
      printStringAt (pos a) "<-"
      exactPC e
    Qualifier l e -> exactP e
    LetStmt l bds   -> do
      printString "let"
      exactPC bds
    RecStmt l ss    -> do
      let (a:pts) = srcInfoPoints l
      printString "rec"
      layoutList pts ss

instance ExactP QualStmt where
  exactP qstmt = case qstmt of
    QualStmt     l stmt -> exactP stmt
    ThenTrans    l e    -> printString "then" >> exactPC e
    ThenBy       l e1 e2    -> do
        let [a,b] = srcInfoPoints l
        printString "then"
        exactPC e1
        printStringAt (pos b) "by"
        exactPC e2
    GroupBy      l e        -> do
        printStrs $ zip (srcInfoPoints l) ["then","group","by"]
        exactPC e
    GroupUsing   l e        -> do
        printStrs $ zip (srcInfoPoints l) ["then","group","using"]
        exactPC e
    GroupByUsing l e1 e2    -> do
        let pts = srcInfoPoints l
        printStrs $ zip (init pts) ["then","group","by"]
        exactPC e1
        printStringAt (pos (last pts)) "using"
        exactPC e2

instance ExactP Bracket where
  exactP br = case br of
    ExpBracket l e  -> do
        let [a,b] = srcInfoPoints l
        printString "[|"
        exactPC e
        printStringAt (pos b) "|]"
    PatBracket l p  -> do
        let [a,b] = srcInfoPoints l
        printString "[p|"
        exactPC p
        printStringAt (pos b) "|]"
    TypeBracket l t  -> do
        let [a,b] = srcInfoPoints l
        printString "[t|"
        exactPC t
        printStringAt (pos b) "|]"
    DeclBracket l ds -> do
        let (a:pts) = srcInfoPoints l
        printString "[d|"
        layoutList (init pts) (sepFunBinds ds)
        printStringAt (pos (last pts)) "|]"

instance ExactP XAttr where
  exactP (XAttr l xn e) = do
    let [a] = srcInfoPoints l
    exactP xn
    printStringAt (pos a) "="
    exactPC e

instance ExactP Alt where
  exactP (Alt l p galts mbs) = do
    exactP p
    exactPC galts
    maybeEP (\bs -> printStringAt (pos (head (srcInfoPoints l))) "where" >> exactPC bs) mbs

instance ExactP GuardedAlts where
  exactP (UnGuardedAlt l e) = printString "->" >> exactPC e
  exactP (GuardedAlts  l galts) = mapM_ exactPC galts

instance ExactP GuardedAlt where
  exactP (GuardedAlt l stmts e) = do
    bracketList ("|",",","->") (srcInfoPoints l) stmts
    exactPC e

instance ExactP Match where
  exactP (Match l n ps rhs mbinds) = do
    let pts = srcInfoPoints l
    exactP n
    mapM_ exactPC ps
    exactPC rhs
    maybeEP (\bds -> printStringAt (pos (head pts)) "where" >> exactPC bds) mbinds
  exactP (InfixMatch l a n b rhs mbinds) = do
    let pts = srcInfoPoints l
    exactP a
    epInfixName n
    exactPC b
    exactPC rhs
    maybeEP (\bds -> printStringAt (pos (head pts)) "where" >> exactPC bds) mbinds

instance ExactP Rhs where
  exactP (UnGuardedRhs l e) = printString "=" >> exactPC e
  exactP (GuardedRhss  l grhss) = mapM_ exactPC grhss

instance ExactP GuardedRhs where
  exactP (GuardedRhs l ss e) = do
    let (a:pts) = srcInfoPoints l
    printString "|"
    printInterleaved' (zip (init pts) (repeat ",") ++ [(last pts, "=")]) ss
    exactPC e

instance ExactP Pat where
  exactP pat = case pat of
    PVar l n    -> exactP n
    PLit l lit  -> exactP lit
    PNeg l p    -> printString "-" >> exactPC p
    PNPlusK l n k   -> do
        let [a,b] = srcInfoPoints l
        exactP n
        printStringAt (pos a) "+"
        printStringAt (pos b) (show k)
    PInfixApp l pa qn pb -> exactP pa >> epInfixQName qn >> exactPC pb
    PApp l qn ps    -> exactP qn >> mapM_ exactPC ps
    PTuple l ps -> parenList (srcInfoPoints l) ps
    PList l ps  -> squareList (srcInfoPoints l) ps
    PParen l p  -> parenList (srcInfoPoints l) [p]
    PRec l qn pfs   -> exactP qn >> curlyList (srcInfoPoints l) pfs
    PAsPat l n p    -> do
        let [a] = srcInfoPoints l
        exactP n
        printStringAt (pos a) "@"
        exactPC p
    PWildCard l -> printString "_"
    PIrrPat l p -> printString "~" >> exactPC p
    PatTypeSig l p t -> do
        let [a] = srcInfoPoints l
        exactP p
        printStringAt (pos a) "::"
        exactPC t
    PViewPat l e p -> do
        let [a] = srcInfoPoints l
        exactP e
        printStringAt (pos a) "->"
        exactPC p
    PRPat l rps -> squareList (srcInfoPoints l) rps
    PXTag l xn attrs mat ps -> do
        let [a,b,c,d,e] = srcInfoPoints l
        printString "<"
        exactPC xn
        mapM_ exactPC attrs
        maybeEP exactPC mat
        printStringAt (pos b) ">"
        mapM_ exactPC ps
        printStringAt (pos c) "</"
        printWhitespace (pos d)
        exactP xn
        printStringAt (pos e) ">"
    PXETag l xn attrs mat -> do
        let [a,b] = srcInfoPoints l
        printString "<"
        exactPC xn
        mapM_ exactPC attrs
        maybeEP exactPC mat
        printStringAt (pos b) "/>"
    PXPcdata l str -> printString str
    PXPatTag l p   -> do
        let [a,b] = srcInfoPoints l
        printString "<%"
        exactPC p
        printString "%>"
    PXRPats  l rps  -> bracketList ("<[",",","]>") (srcInfoPoints l) rps
    PExplTypeArg l qn t -> do
        let [a,b] = srcInfoPoints l
        exactP qn
        printStringAt (pos a) "{|"
        exactPC t
        printStringAt (pos b) "|}"
    PQuasiQuote l name qt   -> printString $ "[$" ++ name ++ "|" ++ qt ++ "]"
    PBangPat l p    -> printString "!" >> exactPC p

instance ExactP PatField where
  exactP pf = case pf of
    PFieldPat l qn p        -> do
        let [a] = srcInfoPoints l
        exactP qn
        printStringAt (pos a) "="
        exactPC p
    PFieldPun l n   -> exactP n
    PFieldWildcard l -> printString ".."

instance ExactP RPat where
  exactP rpat = case rpat of
    RPOp l rp op    -> exactP rp >> exactPC op
    RPEither l r1 r2 -> do
      let [a] = srcInfoPoints l
      exactP r1
      printStringAt (pos a) "|"
      exactPC r2
    RPSeq l rps -> bracketList ("(|",",","|)") (srcInfoPoints l) rps
    RPGuard l p stmts   -> do
      let (a:pts) = srcInfoPoints l
      printString "(|"
      exactPC p
      bracketList ("|",",","|)") pts stmts
    RPCAs l n rp    -> do
      let [a] = srcInfoPoints l
      exactP n
      printStringAt (pos a) "@:"
      exactPC rp
    RPAs l n rp     -> do
      let [a] = srcInfoPoints l
      exactP n
      printStringAt (pos a) "@"
      exactPC rp
    RPParen l rp    -> do
      parenList (srcInfoPoints l) [rp]
    RPPat l p   -> exactP p

instance ExactP RPatOp where
  exactP rop = printString $ case rop of
    RPStar  l  -> "*"
    RPStarG l  -> "*!"
    RPPlus  l  -> "+"
    RPPlusG l  -> "+!"
    RPOpt   l  -> "?"
    RPOptG  l  -> "?!"

instance ExactP PXAttr where
  exactP (PXAttr l xn p) = do
    let [a] = srcInfoPoints l
    exactP xn
    printStringAt (pos a) "="
    exactPC p

instance ExactP XName where
  exactP xn = case xn of
    XName l name -> printString name
    XDomName l dom name -> do
        let [a,b,c] = srcInfoPoints l
        printString dom
        printStringAt (pos b) ":"
        printStringAt (pos c) name

instance ExactP Binds where
  exactP (BDecls  l ds)  = layoutList (srcInfoPoints l) (sepFunBinds ds)
  exactP (IPBinds l ips) = layoutList (srcInfoPoints l) ips

instance ExactP CallConv where
  exactP (StdCall _) = printString "stdcall"
  exactP (CCall   _) = printString "ccall"

instance ExactP Safety where
  exactP (PlayRisky _) = printString "unsafe"
  exactP (PlaySafe _ b) = printString $ if b then "threadsafe" else "safe"

instance ExactP Rule where
  exactP (Rule l str mact mrvs e1 e2) = do
    let (a:pts) = srcInfoPoints l
    printString (show str)
    maybeEP exactP mact
    [x] <- case mrvs of
            Nothing -> return pts
            Just rvs -> do
                let (a:b:pts') = pts
                printStringAt (pos a) "forall"
                mapM_ exactPC rvs
                printStringAt (pos b) "."
                return pts'
    exactPC e1
    printStringAt (pos x) "="
    exactPC e2

instance ExactP RuleVar where
  exactP (TypedRuleVar l n t) = do
        let [a,b,c] = srcInfoPoints l
        printString "("
        exactPC n
        printStringAt (pos b) "::"
        exactPC t
        printStringAt (pos c) ")"
  exactP (RuleVar l n) = exactP n

instance ExactP Activation where
  exactP (ActiveFrom   l i) =
    printPoints l ["[", show i, "]"]
  exactP (ActiveUntil  l i) =
    printPoints l ["[", "~", show i, "]"]

instance ExactP FieldDecl where
  exactP (FieldDecl l ns bt) = do
    let pts = srcInfoPoints l
    printInterleaved' (zip (init pts) (repeat ",") ++ [(last pts, "::")]) ns
    exactPC bt

instance ExactP IPBind where
  exactP (IPBind l ipn e) = do
    let [a] = srcInfoPoints l
    exactP ipn
    printStringAt (pos a) "="
    exactPC e

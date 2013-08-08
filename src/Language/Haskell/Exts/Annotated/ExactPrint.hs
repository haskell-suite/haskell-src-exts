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
import Control.Arrow ((***), (&&&))
import Data.List (intersperse)

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

errorEP :: String -> EP a
errorEP = fail

------------------------------------------------------------------------------
-- Printing of source elements

-- | Print an AST exactly as specified by the annotations on the nodes in the tree.
exactPrint :: (ExactP ast) => ast SrcSpanInfo -> [Comment] -> String
exactPrint ast cs = runEP (exactPC ast) cs

exactPC :: (ExactP ast) => ast SrcSpanInfo -> EP ()
exactPC ast = let p = pos (ann ast) in mPrintComments p >> padUntil p >> exactP ast

printSeq :: [(Pos, EP ())] -> EP ()
printSeq [] = return ()
printSeq ((p,pr):xs) = printWhitespace p >> pr >> printSeq xs

printStrs :: SrcInfo loc => [(loc, String)] -> EP ()
printStrs = printSeq . map (pos *** printString)

printPoints :: SrcSpanInfo -> [String] -> EP ()
printPoints l = printStrs . zip (srcInfoPoints l)

printInterleaved, printInterleaved' :: (Annotated ast, ExactP ast, SrcInfo loc) => [(loc, String)] -> [ast SrcSpanInfo] -> EP ()
printInterleaved sistrs asts = printSeq $
    interleave (map (pos *** printString ) sistrs)
               (map (pos . ann &&& exactP) asts)

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

parenList, squareList, curlyList, parenHashList :: (Annotated ast, ExactP ast) => [SrcSpan] -> [ast SrcSpanInfo] -> EP ()
parenList = bracketList ("(",",",")")
squareList = bracketList ("[",",","]")
curlyList = bracketList ("{",",","}")
parenHashList = bracketList ("(#",",","#)")

layoutList :: (Functor ast, Show (ast ()), Annotated ast, ExactP ast) => [SrcSpan] -> [ast SrcSpanInfo] -> EP ()
layoutList poss asts = printStreams
        (map (pos *** printString) $ lList poss)
        (map (pos . ann &&& exactP) asts)

lList (p:ps) = (if isNullSpan p then (p,"") else (p,"{")) : lList' ps
lList' [] = []
lList' [p] = [if isNullSpan p then (p,"") else (p,"}")]
lList' (p:ps) = (if isNullSpan p then (p,"") else (p,";")) : lList' ps

printSemi :: SrcSpan -> EP ()
printSemi p = do
  printWhitespace (pos p)
  when (not $ isNullSpan p) $ printString ";"


--------------------------------------------------
-- Exact printing

class Annotated ast => ExactP ast where
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
         Unboxed -> "(#": replicate (n-1) "," ++ ["#)"]
         _       -> "(" : replicate (n-1) "," ++ [")"]
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
        case srcInfoPoints (ann qn) of
         [a,b,c] -> do
            printString "("
            printWhitespace (pos b)
            epQName qn
            printStringAt (pos c) ")"
         _ -> errorEP "ExactP: QName is given wrong number of srcInfoPoints"
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
        case srcInfoPoints (ann qn) of
         [a,b,c] -> do
            printStringAt (pos a) "`"
            printWhitespace (pos b)
            epQName qn
            printStringAt (pos c) "`"
         _ -> errorEP "ExactP: QName (epInfixName) is given wrong number of srcInfoPoints"

instance ExactP Name where
  exactP n = case n of
    Ident  l str    -> printString str
    Symbol l str    -> do
        case srcInfoPoints l of
         [a,b,c] -> do
            printString "("
            printWhitespace (pos b)
            printString str
            printStringAt (pos c) ")"
         _ -> errorEP "ExactP: Name is given wrong number of srcInfoPoints"

epName :: Name SrcSpanInfo -> EP ()
epName (Ident  _ str) = printString str
epName (Symbol _ str) = printString str

epInfixName :: Name SrcSpanInfo -> EP ()
epInfixName n
    | isSymbol n = printWhitespace (pos (ann n)) >> epName n
    | otherwise = do
        case srcInfoPoints (ann n) of
         [a,b,c] -> do
            printStringAt (pos a) "`"
            printWhitespace (pos b)
            epName n
            printStringAt (pos c) "`"
         _ -> errorEP "ExactP: Name (epInfixName) is given wrong number of srcInfoPoints"

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
    case srcInfoPoints l of
     (a:pts) -> do
        pts <- if src then
                 case pts of
                  x:y:pts' -> do
                     printStringAt (pos x) "{-# SOURCE"
                     printStringAt (pos y) "#-}"
                     return pts'
                  _ -> errorEP "ExactP: ImportDecl is given too few srcInfoPoints"
                else return pts
        pts <- if qf then
                 case pts of
                  x:pts' -> do
                     printStringAt (pos x) "qualified"
                     return pts'
                  _ -> errorEP "ExactP: ImportDecl is given too few srcInfoPoints"
                else return pts
        pts <- case mpkg of
                Just pkg ->
                  case pts of
                   x:pts' -> do
                      printStringAt (pos x) $ show pkg
                      return pts'
                   _ -> errorEP "ExactP: ImportDecl is given too few srcInfoPoints"
                _ -> return pts
        exactPC mn
        pts <- case mas of
                Just as ->
                 case pts of
                  x:pts' -> do
                     printStringAt (pos x) "as"
                     exactPC as
                     return pts'
                  _ -> errorEP "ExactP: ImportDecl is given too few srcInfoPoints"
                _ -> return pts
        case mispecs of
         Nothing -> return ()
         Just ispecs -> exactPC ispecs
     _ -> errorEP "ExactP: ImportDecl is given too few srcInfoPoints"

instance ExactP Module where
  exactP mdl = case mdl of
    Module l mmh oss ids decls -> do
        let (oPts, pts) = splitAt (max (length oss + 1) 2) (srcInfoPoints l)
        layoutList oPts oss
        maybeEP exactPC mmh
        printStreams (map (pos *** printString) $ lList pts)
                     (map (pos . ann &&& exactPC) ids ++ map (pos . ann &&& exactPC) (sepFunBinds decls))
    XmlPage l _mn oss xn attrs mat es  -> do
        let (oPts, pPts) = splitAt (max (length oss + 1) 2) $ srcInfoPoints l
        case pPts of
         [a,b,c,d,e] -> do
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
         _ -> errorEP "ExactP: Module: XmlPage is given wrong number of srcInfoPoints"
    XmlHybrid l mmh oss ids decls xn attrs mat es -> do
        let (oPts, pts) = splitAt (max (length oss + 1) 2) (srcInfoPoints l)
        layoutList oPts oss
        maybeEP exactPC mmh
        let (dPts, pPts) = splitAt (length pts - 5) pts
        case pPts of
         [a,b,c,d,e] -> do
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
         _ -> errorEP "ExactP: Module: XmlHybrid is given wrong number of srcInfoPoints"

instance ExactP ModuleHead where
  exactP (ModuleHead l mn mwt mess) = do
    case srcInfoPoints l of
     [a,b] -> do
        printStringAt (pos a) "module"
        exactPC mn
        maybeEP exactPC mwt
        maybeEP exactPC mess
        printStringAt (pos b) "where"
     _ -> errorEP "ExactP: ModuleHead is given wrong number of srcInfoPoints"

instance ExactP ModulePragma where
  exactP op = case op of
    LanguagePragma   l ns       ->
        let pts = srcInfoPoints l
            k = length ns - 1 -- number of commas
            m = length pts - k - 2 -- number of virtual semis, likely 0
         in printInterleaved (zip pts ("{-# LANGUAGE":replicate k "," ++ replicate m "" ++ ["#-}"])) ns
    OptionsPragma    l mt str   ->
        let k = length (srcInfoPoints l)
            opstr = "{-# OPTIONS" ++ case mt of { Just t -> "_" ++ show t ; _ -> "" } ++ " " ++ str
         in printPoints l $ opstr : replicate (k-2) "" ++ ["#-}"]
    AnnModulePragma  l ann       ->
        case srcInfoPoints l of
         [a,b] -> do
            printString $ "{-# ANN"
            exactPC ann
            printStringAt (pos b) "#-}"
         _ -> errorEP "ExactP: ModulePragma: AnnPragma is given wrong number of srcInfoPoints"

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
    TypeDecl     l dh t      ->
        case srcInfoPoints l of
         [a,b] -> do
            printStringAt (pos a) "type"
            exactPC dh
            printStringAt (pos b) "="
            exactPC t
         _ -> errorEP "ExactP: Decl: TypeDecl is given wrong number of srcInfoPoints"
    TypeFamDecl  l dh mk     ->
        case srcInfoPoints l of
         a:b:ps -> do
            printStringAt (pos a) "type"
            printStringAt (pos b) "family"
            exactPC dh
            maybeEP (\k -> printStringAt (pos (head ps)) "::" >> exactPC k) mk
         _ -> errorEP "ExactP: Decl: TypeFamDecl is given wrong number of srcInfoPoints"
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
        pts <- case mk of
                Nothing -> return pts
                Just kd -> case pts of
                            p:pts' -> do
                               printStringAt (pos p) "::"
                               exactPC kd
                               return pts'
                            _ -> errorEP "ExactP: Decl: GDataDecl is given too few srcInfoPoints"
        case pts of
         x:pts -> do
            printStringAt (pos x) "where"
            layoutList pts gds
            maybeEP exactPC mder
         _ -> errorEP "ExactP: Decl: GDataDecl is given too few srcInfoPoints"
    DataFamDecl  l mctxt dh mk -> do
        printString "data"
        maybeEP exactPC mctxt
        exactPC dh
        maybeEP (\kd -> printStringAt (pos (head (srcInfoPoints l))) "::" >> exactPC kd) mk
    TypeInsDecl  l t1 t2 ->
        case srcInfoPoints l of
         [a,b,c] -> do
            printString "type"
            printStringAt (pos b) "instance"
            exactPC t1
            printStringAt (pos c) "="
            exactPC t2
         _ -> errorEP "ExactP: Decl: TypeInsDecl is given wrong number of srcInfoPoints"
    DataInsDecl  l dn t constrs mder    ->
        case srcInfoPoints l of
         p:pts -> do
            exactP dn
            printStringAt (pos p) "instance"
            exactPC t
            printInterleaved (zip pts ("=": repeat "|")) constrs
            maybeEP exactPC mder
         _ -> errorEP "ExactP: Decl: DataInsDecl is given too few srcInfoPoints"
    GDataInsDecl l dn t mk gds mder     ->
        case srcInfoPoints l of
         p:pts -> do
            exactP dn
            printStringAt (pos p) "instance"
            exactPC t
            pts <- case mk of
                    Nothing -> return pts
                    Just kd -> case pts of
                                p:pts' -> do
                                    printStringAt (pos p) "::"
                                    exactPC kd
                                    return pts'
                                _ -> errorEP "ExactP: Decl: GDataInsDecl is given too few srcInfoPoints"
            case pts of
             x:pts -> do
                printStringAt (pos x) "where"
                layoutList pts gds
                maybeEP exactPC mder
             _ -> errorEP "ExactP: Decl: GDataInsDecl is given too few srcInfoPoints"
         _ -> errorEP "ExactP: Decl: GDataInsDecl is given too few srcInfoPoints"
    ClassDecl    l mctxt dh fds mcds    ->
        case srcInfoPoints l of
         a:pts -> do
            printString "class"
            maybeEP exactPC mctxt
            exactPC dh
            pts <- case fds of
                    [] -> return pts
                    _  -> do
                      let (pts1, pts2) = splitAt (length fds) pts
                      printInterleaved (zip pts1 ("|":repeat ",")) fds
                      return pts2
            maybeEP (\cds ->
                case pts of
                 p:pts' -> do
                    printStringAt (pos p) "where"
                    layoutList pts' $ sepClassFunBinds cds
                 _ -> errorEP "ExactP: Decl: ClassDecl is given too few srcInfoPoints"
                ) mcds
         _ -> errorEP "ExactP: Decl: ClassDecl is given too few srcInfoPoints"
    InstDecl     l mctxt ih mids        ->
        case srcInfoPoints l of
         a:pts -> do
            printString "instance"
            maybeEP exactPC mctxt
            exactPC ih
            maybeEP (\ids -> do
                let (p:pts') = pts
                printStringAt (pos p) "where"
                layoutList pts' $ sepInstFunBinds ids
                ) mids
         _ -> errorEP "ExactP: Decl: InstDecl is given too few srcInfoPoints"
    DerivDecl    l mctxt ih             ->
        case srcInfoPoints l of
         [a,b] -> do
            printString "deriving"
            printStringAt (pos b) "instance"
            maybeEP exactPC mctxt
            exactPC ih
         _ -> errorEP "ExactP: Decl: DerivDecl is given wrong number of srcInfoPoints"
    InfixDecl    l assoc mprec ops      -> do
        let pts = srcInfoPoints l
        exactP assoc
        pts <- case mprec of
                Nothing -> return pts
                Just prec ->
                    case pts of
                     p:pts' -> do
                        printStringAt (pos p) (show prec)
                        return pts'
                     _ -> errorEP "ExactP: Decl: InfixDecl is given too few srcInfoPoints"
        printInterleaved' (zip pts (repeat ",")) ops
    DefaultDecl  l ts   ->
        case srcInfoPoints l of
         a:pts -> do
            printString "default"
            printInterleaved (zip (init pts) ("(":repeat ",")) ts
            printStringAt (pos (last pts)) ")"
         _ -> errorEP "ExactP: Decl: DefaultDecl is given too few srcInfoPoints"
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
                Just t  -> case pts of
                            x:pts'-> do
                                printStringAt (pos x) "::"
                                exactPC t
                                return pts'
                            _ -> errorEP "ExactP: Decl: PatBind is given too few srcInfoPoints"
        exactPC rhs
        maybeEP (\bs -> printStringAt (pos (head pts)) "where" >> exactPC bs) mbs
    ForImp       l cc msf mstr n t   ->
        case srcInfoPoints l of
         a:b:pts -> do
            printString "foreign"
            printStringAt (pos b) "import"
            exactPC cc
            maybeEP exactPC msf
            pts <- case mstr of
                      Nothing -> return pts
                      Just str -> case pts of
                                   x:pts' -> do
                                      printStringAt (pos x) (show str)
                                      return pts'
                                   _ -> errorEP "ExactP: Decl: ForImp is given too few srcInfoPoints"
            case pts of
             y:_ -> do
                exactPC n
                printStringAt (pos y) "::"
                exactPC t
             _ -> errorEP "ExactP: Decl: ForImp is given too few srcInfoPoints"
         _ -> errorEP "ExactP: Decl: ForImp is given too few srcInfoPoints"
    ForExp       l cc mstr n t      ->
        case srcInfoPoints l of
         a:b:pts -> do
            printString "foreign"
            printStringAt (pos b) "export"
            exactPC cc
            pts <- case mstr of
                      Nothing -> return pts
                      Just str -> case pts of
                                   x:pts' -> do
                                      printStringAt (pos x) (show str)
                                      return pts'
                                   _ -> errorEP "ExactP: Decl: ForExp is given too few srcInfoPoints"
            case pts of
             y:_ -> do
                exactPC n
                printStringAt (pos y) "::"
                exactPC t
             _ -> errorEP "ExactP: Decl: ForExp is given too few srcInfoPoints"
         _ -> errorEP "ExactP: Decl: ForExp is given too few srcInfoPoints"
    RulePragmaDecl   l rs   ->
        case srcInfoPoints l of
         [a,b] -> do
            printString "{-# RULES"
            mapM_ exactPC rs
            printStringAt (pos b) "#-}"
         _ -> errorEP "ExactP: Decl: RulePragmaDecl is given too few srcInfoPoints"
    DeprPragmaDecl   l nstrs ->
        case srcInfoPoints l of
         a:pts -> do
            printString "{-# DEPRECATED"
            printWarndeprs (map pos (init pts)) nstrs
            printStringAt (pos (last pts)) "#-}"
         _ -> errorEP "ExactP: Decl: DeprPragmaDecl is given too few srcInfoPoints"
    WarnPragmaDecl   l nstrs ->
        case srcInfoPoints l of
         a:pts -> do
            printString "{-# WARNING"
            printWarndeprs (map pos (init pts)) nstrs
            printStringAt (pos (last pts)) "#-}"
         _ -> errorEP "ExactP: Decl: WarnPragmaDecl is given too few srcInfoPoints"
    InlineSig        l inl mact qn    ->
        case srcInfoPoints l of
         [a,b] -> do
            printString $ if inl then "{-# INLINE" else "{-# NOINLINE"
            maybeEP exactPC mact
            exactPC qn
            printStringAt (pos b) "#-}"
         _ -> errorEP "ExactP: Decl: InlineSig is given wrong number of srcInfoPoints"
    InlineConlikeSig l mact qn    ->
        case srcInfoPoints l of
         [a,b] -> do
            printString "{-# INLINE_CONLIKE"
            maybeEP exactPC mact
            exactPC qn
            printStringAt (pos b) "#-}"
         _ -> errorEP "ExactP: Decl: InlineConlikeSig is given wrong number of srcInfoPoints"
    SpecSig          l mact qn ts ->
        case srcInfoPoints l of
         a:pts -> do
            printString "{-# SPECIALISE"
            maybeEP exactPC mact
            exactPC qn
            printInterleaved (zip pts ("::" : replicate (length pts - 2) "," ++ ["#-}"])) ts
         _ -> errorEP "ExactP: Decl: SpecSig is given too few srcInfoPoints"
    SpecInlineSig    l b mact qn ts ->
        case srcInfoPoints l of
         a:pts -> do
            printString $ "{-# SPECIALISE " ++ if b then "INLINE" else "NOINLINE"
            maybeEP exactPC mact
            exactPC qn
            printInterleaved (zip pts ("::" : replicate (length pts - 2) "," ++ ["#-}"])) ts
         _ -> errorEP "ExactP: Decl: SpecInlineSig is given too few srcInfoPoints"
    InstSig          l mctxt ih     ->
        case srcInfoPoints l of
         [a,b,c] -> do
            printString $ "{-# SPECIALISE"
            printStringAt (pos b) "instance"
            maybeEP exactPC mctxt
            exactPC ih
            printStringAt (pos c) "#-}"
         _ -> errorEP "ExactP: Decl: InstSig is given wrong number of srcInfoPoints"
    AnnPragma       l ann       ->
        case srcInfoPoints l of
         [a,b] -> do
            printString $ "{-# ANN"
            exactPC ann
            printStringAt (pos b) "#-}"
         _ -> errorEP "ExactP: Decl: AnnPragma is given wrong number of srcInfoPoints"


instance ExactP Annotation where
    exactP ann = case ann of
        Ann     l n e   -> do
            exactP n
            exactPC e
        TypeAnn l n e   -> do
            printString "type"
            exactPC n
            exactPC e
        ModuleAnn l e   -> do
            printString "module"
            exactPC e

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
        case srcInfoPoints l of
         [_,b] -> printString "(" >> exactPC dh >> printStringAt (pos b) ")"
         _ -> errorEP "ExactP: DeclHead: DeclParen is given wrong number of srcInfoPoints"

instance ExactP InstHead where
  exactP ih = case ih of
    IHead l qn ts       -> exactP qn >> mapM_ exactPC ts
    IHInfix l ta qn tb  -> exactP ta >> epInfixQName qn >> exactPC tb
    IHParen l ih        ->
        case srcInfoPoints l of
         [_,b] -> printString "(" >> exactPC ih >> printStringAt (pos b) ")"
         _ -> errorEP "ExactP: InstHead: IHParen is given wrong number of srcInfoPoints"

instance ExactP TyVarBind where
  exactP (KindedVar   l n k) =
        case srcInfoPoints l of
         [a,b,c] -> do
            printString "("
            exactPC n
            printStringAt (pos b) "::"
            exactPC k
            printStringAt (pos c) ")"
         _ -> errorEP "ExactP: TyVarBind: KindedVar is given wrong number of srcInfoPoints"
  exactP (UnkindedVar l n) = exactP n

instance ExactP Kind where
  exactP kd = case kd of
    KindStar  l     -> printString "*"
    KindBang  l     -> printString "!"
    KindFn    l k1 k2 ->
        case srcInfoPoints l of
         [a] -> do
            exactP k1
            printStringAt (pos a) "->"
            exactPC k2
         _ -> errorEP "ExactP: Kind: KindFn is given wrong number of srcInfoPoints"
    KindParen l kd  -> do
        case srcInfoPoints l of
         [a,b] -> do
            printString "("
            exactPC kd
            printStringAt (pos b) ")"
         _ -> errorEP "ExactP: Kind: KindParen is given wrong number of srcInfoPoints"
    KindVar l n     -> exactP n

instance ExactP Type where
  exactP t = case t of
    TyForall l mtvs mctxt t -> do
        let pts = srcInfoPoints l
        pts <- case mtvs of
                Nothing -> return pts
                Just tvs ->
                    case pts of
                     a:b:pts' -> do
                        printString "forall"
                        mapM_ exactPC tvs
                        printStringAt (pos b) "."
                        return pts'
                     _ -> errorEP "ExactP: Type: TyForall is given too few srcInfoPoints"
        maybeEP exactPC mctxt
        exactPC t
    TyFun   l t1 t2 -> do
        case srcInfoPoints l of
         [a] -> do
            exactP t1
            printStringAt (pos a) "->"
            exactPC t2
         _ -> errorEP "ExactP: Type: TyFun is given wrong number of srcInfoPoints"
    TyTuple l bx ts -> do
        case bx of
          Boxed   -> parenList (srcInfoPoints l) ts
          Unboxed -> parenHashList (srcInfoPoints l) ts
    TyList  l t     -> do
        case srcInfoPoints l of
         [a,b] -> do
            printString "["
            exactPC t
            printStringAt (pos b) "]"
         _ -> errorEP "ExactP: Type: TyList is given wrong number of srcInfoPoints"
    TyApp   l t1 t2 -> exactP t1 >> exactPC t2
    TyVar   l n     -> exactP n
    TyCon   l qn    -> exactP qn
    TyParen l t     -> do
        case srcInfoPoints l of
         [a,b] -> do
            printString "("
            exactPC t
            printStringAt (pos b) ")"
         _ -> errorEP "ExactP: Type: TyParen is given wrong number of srcInfoPoints"
    TyInfix l t1 qn t2 -> exactP t1 >> epInfixQName qn >> exactPC t2
    TyKind  l t kd -> do
        case srcInfoPoints l of
         [a,b,c] -> do
            printString "("
            exactPC t
            printStringAt (pos b) "::"
            exactPC kd
            printStringAt (pos c) ")"
         _ -> errorEP "ExactP: Type: TyKind is given wrong number of srcInfoPoints"

instance ExactP Context where
  exactP ctxt = do
    printContext ctxt
    printStringAt (pos . last . srcInfoPoints . ann $ ctxt) "=>"

printContext ctxt = do
    let l = ann ctxt
        pts = init $ srcInfoPoints l
    case ctxt of
     CxParen l ctxt ->
        case pts of
         [a,b] -> do
            printStringAt (pos a) "("
            printContext ctxt
            printStringAt (pos b) ")"
         _ -> errorEP "ExactP: Context: CxParen is given wrong number of srcInfoPoints"
     CxSingle l asst -> exactP asst
     CxEmpty l ->
        case pts of
         [a,b] -> do
            printStringAt (pos a) "("
            printStringAt (pos b) ")"
         _ -> errorEP "ExactP: Context: CxEmpty is given wrong number of srcInfoPoints"
     CxTuple l assts -> parenList pts assts


instance ExactP Asst where
  exactP asst = case asst of
    ClassA l qn ts -> exactP qn >> mapM_ exactPC ts
    InfixA l ta qn tb -> exactP ta >> epInfixQName qn >> exactPC tb
    IParam l ipn t    ->
        case srcInfoPoints l of
         [a] -> do
            exactP ipn
            printStringAt (pos a) "::"
            exactPC t
         _ -> errorEP "ExactP: Asst: IParam is given wrong number of srcInfoPoints"
    EqualP l t1 t2  ->
        case srcInfoPoints l of
         [a] -> do
            exactP t1
            printStringAt (pos a) "~"
            exactPC t2

instance ExactP Deriving where
  exactP (Deriving l ihs) =
    case srcInfoPoints l of
     x:pts -> do
        printString "deriving"
        case pts of
         [] -> exactPC $ head ihs
         _  -> parenList pts ihs
     _ -> errorEP "ExactP: Deriving is given too few srcInfoPoints"

instance ExactP ClassDecl where
  exactP cdecl = case cdecl of
    ClsDecl    l d -> exactP d
    ClsDataFam l mctxt dh mk ->
        case srcInfoPoints l of
         x:pts -> do
            printString "data"
            maybeEP exactPC mctxt
            exactPC dh
            maybeEP (\kd -> printStringAt (pos (head pts)) "::" >> exactPC kd) mk
         _ -> errorEP "ExactP: ClassDecl: ClsDataFam is given too few srcInfoPoints"
    ClsTyFam   l dh mk  ->
        case srcInfoPoints l of
         x:pts -> do
            printString "type"
            exactPC dh
            maybeEP (\kd -> printStringAt (pos (head pts)) "::" >> exactPC kd) mk
         _ -> errorEP "ExactP: ClassDecl: ClsTyFam is given too few srcInfoPoints"
    ClsTyDef   l t1 t2  ->
        case srcInfoPoints l of
         a:b:pts -> do
            printString "type"
            exactPC t1
            printStringAt (pos b) "="
            exactPC t2
         _ -> errorEP "ExactP: ClassDecl: ClsTyDef is given too few srcInfoPoints"

instance ExactP InstDecl where
  exactP idecl = case idecl of
    InsDecl   l d -> exactP d
    InsType   l t1 t2 ->
        case srcInfoPoints l of
         [a,b] -> do
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
        pts <- case mk of
                Nothing -> return pts
                Just kd -> case pts of
                            p:pts' -> do
                                printStringAt (pos p) "::"
                                exactPC kd
                                return pts'
                            _ -> errorEP "ExactP: InstDecl: InsGData is given too few srcInfoPoints"
        case pts of
         x:_ -> do
            printStringAt (pos x) "where"
            mapM_ exactPC gds
            maybeEP exactPC mder
         _ -> errorEP "ExactP: InstDecl: InsGData is given too few srcInfoPoints"
--  InsInline l inl mact qn   -> do
--        case srcInfoPoints l of
--         [a,b] -> do
--            printString $ if inl then "{-# INLINE" else "{-# NOINLINE"
--            maybeEP exactPC mact
--            exactPC qn
--            printStringAt (pos b) "#-}"
--         _ -> errorEP "ExactP: InstDecl: InsInline is given wrong number of srcInfoPoints"

instance ExactP FunDep where
  exactP (FunDep l nxs nys) =
    case srcInfoPoints l of
     [a] -> do
        mapM_ exactPC nxs
        printStringAt (pos a) "->"
        mapM_ exactPC nys
     _ -> errorEP "ExactP: FunDep is given wrong number of srcInfoPoints"

instance ExactP QualConDecl where
  exactP (QualConDecl l mtvs mctxt cd) = do
        let pts = srcInfoPoints l
        pts <- case mtvs of
                Nothing -> return pts
                Just tvs ->
                    case pts of
                     a:b:pts' -> do
                        printString "forall"
                        mapM_ exactPC tvs
                        printStringAt (pos b) "."
                        return pts'
                     _ -> errorEP "ExactP: QualConDecl is given wrong number of srcInfoPoints"
        maybeEP exactPC mctxt
        exactPC cd

instance ExactP ConDecl where
  exactP cd = case cd of
    ConDecl l n bts -> exactP n >> mapM_ exactPC bts
    InfixConDecl l bta n btb -> exactP bta >> epInfixName n >> exactP btb
    RecDecl l n fds -> exactP n >> curlyList (srcInfoPoints l) fds

instance ExactP GadtDecl where
  exactP (GadtDecl l n t) =
    case srcInfoPoints l of
     [a] -> do
        exactP n
        printStringAt (pos a) "::"
        exactPC t
     _ -> errorEP "ExactP: GadtDecl is given wrong number of srcInfoPoints"

instance ExactP BangType where
  exactP bt = case bt of
    UnBangedTy l t  -> exactP t
    BangedTy   l t  -> printString "!" >> exactPC t
    UnpackedTy l t  ->
      case srcInfoPoints l of
       [a,b,c] -> do
          printString "{-# UNPACK"
          printStringAt (pos b) "#-}"
          printStringAt (pos c) "!"
          exactPC t
       _ -> errorEP "ExactP: BangType: UnpackedTy is given wrong number of srcInfoPoints"

instance ExactP Splice where
  exactP (IdSplice l str) = printString $ '$':str
  exactP (ParenSplice l e) =
    case srcInfoPoints l of
     [a,b] -> do
        printString "$("
        exactPC e
        printStringAt (pos b) ")"
     _ -> errorEP "ExactP: Splice: ParenSplice is given wrong number of srcInfoPoints"

instance ExactP Exp where
  exactP exp = case exp of
    Var l qn        -> exactP qn
    IPVar l ipn     -> exactP ipn
    Con l qn        -> exactP qn
    Lit l lit       -> exactP lit
    InfixApp l e1 op e2 -> exactP e1 >> exactPC op >> exactPC e2
    App l e1 e2     -> exactP e1 >> exactPC e2
    NegApp l e      -> printString "-" >> exactPC e
    Lambda l ps e   ->
        case srcInfoPoints l of
         [a,b] -> do
            printString "\\"
            mapM_ exactPC ps
            printStringAt (pos b) "->"
            exactPC e
         _ -> errorEP "ExactP: Exp: Lambda is given wrong number of srcInfoPoints"
    Let l bs e      ->
        case srcInfoPoints l of
         [a,b] -> do
            printString "let"
            exactPC bs
            printStringAt (pos b) "in"
            exactPC e
         _ -> errorEP "ExactP: Exp: Let is given wrong number of srcInfoPoints"
    If l ec et ee   -> -- traceShow (srcInfoPoints l) $ do
        -- First we need to sort out if there are any optional
        -- semicolons hiding among the srcInfoPoints.
        case srcInfoPoints l of
         (pIf:b:c:rest) -> do
            let (mpSemi1,pThen,rest2) =
                           if snd (spanSize b) == 4 -- this is "then", not a semi
                            then (Nothing, b, c:rest)
                            else (Just b, c, rest)
            case rest2 of
              (c:rest3) -> do
                let (mpSemi2,rest4) = if snd (spanSize c) == 4 -- this is "else", not a semi
                                       then (Nothing, rest2)
                                       else (Just c, rest3)
                case rest4 of
                  [pElse] -> do
                    -- real work starts here:
                    printString "if"
                    exactPC ec
                    maybeEP printSemi  mpSemi1
                    printStringAt (pos pThen) "then"
                    exactPC et
                    maybeEP printSemi mpSemi2
                    printStringAt (pos pElse) "else"
                    exactPC ee
                  [] -> errorEP "ExactP: Exp: If is given too few srcInfoPoints"
                  _  -> errorEP "ExactP: Exp: If is given too many srcInfoPoints"
              _ -> errorEP "ExactP: Exp: If is given too few srcInfoPoints"

         _ -> errorEP "ExactP: Exp: If is given too few srcInfoPoints"
    Case l e alts   ->
        case srcInfoPoints l of
         a:b:pts -> do
            printString "case"
            exactPC e
            printStringAt (pos b) "of"
            layoutList pts alts
         _ -> errorEP "ExactP: Exp: Case is given too few srcInfoPoints"
    Do l stmts      ->
        case srcInfoPoints l of
         a:pts -> do
            printString "do"
            layoutList pts stmts
         _ -> errorEP "ExactP: Exp: Do is given too few srcInfoPoints"
    MDo l stmts     ->
        case srcInfoPoints l of
         a:pts -> do
            printString "mdo"
            layoutList pts stmts
         _ -> errorEP "ExactP: Exp: Mdo is given wrong number of srcInfoPoints"
    Tuple l bx es   ->
        case bx of
          Boxed   -> parenList (srcInfoPoints l) es
          Unboxed -> parenHashList (srcInfoPoints l) es
    TupleSection l bx mexps -> do
        let pts = srcInfoPoints l
            (o, e) = case bx of Boxed -> ("(", ")"); Unboxed -> ("(#", "#)")
        printSeq $ interleave (zip (map pos $ init pts) (map printString (o: repeat ",")) ++ [(pos $ last pts, printString e)])
                              (map (maybe (0,0) (pos . ann) &&& maybeEP exactPC) mexps)
    List l es               -> squareList (srcInfoPoints l) es
    Paren l p               -> parenList (srcInfoPoints l) [p]
    LeftSection l e qop     ->
        case srcInfoPoints l of
         [a,b] -> do
            printString "("
            exactPC e
            exactPC qop
            printStringAt (pos b) ")"
         _ -> errorEP "ExactP: Exp: LeftSection is given wrong number of srcInfoPoints"
    RightSection l qop e    ->
        case srcInfoPoints l of
         [a,b] -> do
            printString "("
            exactPC qop
            exactPC e
            printStringAt (pos b) ")"
         _ -> errorEP "ExactP: Exp: RightSection is given wrong number of srcInfoPoints"
    RecConstr l qn fups     -> do
        let pts = srcInfoPoints l
        exactP qn
        curlyList pts fups
    RecUpdate l e fups      -> do
        let pts = srcInfoPoints l
        exactP e
        curlyList pts fups
    EnumFrom l e            ->
        case srcInfoPoints l of
         [a,b,c] -> do
            printString "["
            exactPC e
            printStringAt (pos b) ".."
            printStringAt (pos c) "]"
         _ -> errorEP "ExactP: Exp: EnumFrom is given wrong number of srcInfoPoints"
    EnumFromTo l e1 e2      ->
        case srcInfoPoints l of
         [a,b,c] -> do
            printString "["
            exactPC e1
            printStringAt (pos b) ".."
            exactPC e2
            printStringAt (pos c) "]"
         _ -> errorEP "ExactP: Exp: EnumFromTo is given wrong number of srcInfoPoints"
    EnumFromThen l e1 e2    ->
        case srcInfoPoints l of
         [a,b,c,d] -> do
            printString "["
            exactPC e1
            printStringAt (pos b) ","
            exactPC e2
            printStringAt (pos c) ".."
            printStringAt (pos d) "]"
         _ -> errorEP "ExactP: Exp: EnumFromThen is given wrong number of srcInfoPoints"
    EnumFromThenTo l e1 e2 e3   ->
        case srcInfoPoints l of
         [a,b,c,d] -> do
            printString "["
            exactPC e1
            printStringAt (pos b) ","
            exactPC e2
            printStringAt (pos c) ".."
            exactPC e3
            printStringAt (pos d) "]"
         _ -> errorEP "ExactP: Exp: EnumFromToThen is given wrong number of srcInfoPoints"
    ListComp l e qss            ->
        case srcInfoPoints l of
         a:pts -> do
            printString "["
            exactPC e
            bracketList ("|",",","]") pts qss
         _ -> errorEP "ExactP: Exp: ListComp is given too few srcInfoPoints"
    ParComp  l e qsss           ->
        case srcInfoPoints l of
         a:pts -> do
            let (strs, qss) = unzip $ pairUp qsss
            printString "["
            exactPC e
            printInterleaved (zip pts (strs ++ ["]"])) qss
         _ -> errorEP "ExactP: Exp: ParComp is given wrong number of srcInfoPoints"
      where pairUp [] = []
            pairUp ((a:as):xs) = ("|", a) : zip (repeat ",") as ++ pairUp xs

    ExpTypeSig l e t    ->
        case srcInfoPoints l of
         [a] -> do
            exactP e
            printStringAt (pos a) "::"
            exactPC t
         _ -> errorEP "ExactP: Exp: ExpTypeSig is given wrong number of srcInfoPoints"
    VarQuote l qn   -> do
      printString "'"
      exactPC qn
    TypQuote l qn -> do
      printString "''"
      exactPC qn
    BracketExp l br -> exactP br
    SpliceExp l sp  -> exactP sp
    QuasiQuote l name qt    -> do
        let qtLines = lines qt
        printString $ "[" ++ name ++ "|"
        sequence_ (intersperse newLine $ map printString qtLines)
        printString "|]"
    XTag l xn attrs mat es  -> do
        case srcInfoPoints l of
         [a,b,c,d,e] -> do
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
         -- TODO: Fugly hack/duplication, should be refactored
         -- For the case when there's an optional semicolon
         [a,b,semi,c,d,e] -> do
            printString "<"
            exactPC xn
            mapM_ exactPC attrs
            maybeEP exactPC mat
            printStringAt (pos b) ">"
            mapM_ exactPC es
            printSemi semi
            printStringAt (pos c) "</"
            printWhitespace (pos d)
            exactP xn
            printStringAt (pos e) ">"
         _ -> errorEP "ExactP: Exp: XTag is given wrong number of srcInfoPoints"
    XETag l xn attrs mat    ->
        case srcInfoPoints l of
         [a,b] -> do
            printString "<"
            exactPC xn
            mapM_ exactPC attrs
            maybeEP exactPC mat
            printStringAt (pos b) "/>"
         _ -> errorEP "ExactP: Exp: XETag is given wrong number of srcInfoPoints"
    XPcdata l str   -> do
        let strLines = lines str
        sequence_ (intersperse newLine $ map printString strLines)
    XExpTag l e     ->
        case srcInfoPoints l of
         [a,b] -> do
            printString "<%"
            exactPC e
            printStringAt (pos b) "%>"
         _ -> errorEP "ExactP: Exp: XExpTag is given wrong number of srcInfoPoints"
    XChildTag l es  ->
        case srcInfoPoints l of
         [a,b,c] -> do
            printString "<%>"
            mapM_ exactPC es
            printStringAt (pos b) "</"
            printStringAt (pos c) "%>"
         -- Ugly duplication for when there's an optional semi
         [a,semi,b,c] -> do
            printString "<%>"
            mapM_ exactPC es
            printSemi semi
            printStringAt (pos b) "</"
            printStringAt (pos c) "%>"
         _ -> errorEP "ExactP: Exp: XChildTag is given wrong number of srcInfoPoints"
    CorePragma l      str e         ->
        case srcInfoPoints l of
         [a,b] -> do
            printString $ "{-# CORE " ++ show str
            printStringAt (pos b) "#-}"
            exactPC e
         _ -> errorEP "ExactP: Exp: CorePragma is given wrong number of srcInfoPoints"
    SCCPragma  l      str e         ->
        case srcInfoPoints l of
         [a,b] -> do
            printString $ "{-# SCC " ++ show str
            printStringAt (pos b) "#-}"
            exactPC e
         _ -> errorEP "ExactP: Exp: SCCPragma is given wrong number of srcInfoPoints"
    GenPragma  l      str (i1,i2) (i3,i4) e -> do
        printStrs $ zip (srcInfoPoints l) ["{-# GENERATED", show str, show i1, ":", show i2, "-", show i3, ":", show i4, "#-}"]
        exactPC e
    Proc            l p e   ->
        case srcInfoPoints l of
         [a,b] -> do
            printString "proc"
            exactPC p
            printStringAt (pos b) "->"
            exactPC e
         _ -> errorEP "ExactP: Exp: Proc is given wrong number of srcInfoPoints"
    LeftArrApp      l e1 e2 ->
        case srcInfoPoints l of
         [a] -> do
            exactP e1
            printStringAt (pos a) "-<"
            exactPC e2
         _ -> errorEP "ExactP: Exp: LeftArrApp is given wrong number of srcInfoPoints"
    RightArrApp     l e1 e2 -> do
        case srcInfoPoints l of
         [a] -> do
            exactP e1
            printStringAt (pos a) ">-"
            exactPC e2
         _ -> errorEP "ExactP: Exp: RightArrApp is given wrong number of srcInfoPoints"
    LeftArrHighApp  l e1 e2 -> do
        case srcInfoPoints l of
         [a] -> do
            exactP e1
            printStringAt (pos a) "-<<"
            exactPC e2
         _ -> errorEP "ExactP: Exp: LeftArrHighApp is given wrong number of srcInfoPoints"
    RightArrHighApp l e1 e2 -> do
        case srcInfoPoints l of
         [a] -> do
            exactP e1
            printStringAt (pos a) ">>-"
            exactPC e2
         _ -> errorEP "ExactP: Exp: RightArrHighApp is given wrong number of srcInfoPoints"

instance ExactP FieldUpdate where
  exactP fup = case fup of
    FieldUpdate l qn e  -> do
      case srcInfoPoints l of
       [a] -> do
          exactP qn
          printStringAt (pos a) "="
          exactPC e
       _ -> errorEP "ExactP: FieldUpdate is given wrong number of srcInfoPoints"
    FieldPun l n    -> exactP n
    FieldWildcard l -> printString ".."

instance ExactP Stmt where
  exactP stmt = case stmt of
    Generator l p e ->
      case srcInfoPoints l of
       [a] -> do
          exactP p
          printStringAt (pos a) "<-"
          exactPC e
       _ -> errorEP "ExactP: Stmt: Generator is given wrong number of srcInfoPoints"
    Qualifier l e -> exactP e
    LetStmt l bds   -> do
      printString "let"
      exactPC bds
    RecStmt l ss    ->
      case srcInfoPoints l of
       a:pts -> do
          printString "rec"
          layoutList pts ss
       _ -> errorEP "ExactP: Stmt: RecStmt is given too few srcInfoPoints"

instance ExactP QualStmt where
  exactP qstmt = case qstmt of
    QualStmt     l stmt -> exactP stmt
    ThenTrans    l e    -> printString "then" >> exactPC e
    ThenBy       l e1 e2    -> do
        case srcInfoPoints l of
         [a,b] -> do
            printString "then"
            exactPC e1
            printStringAt (pos b) "by"
            exactPC e2
         _ -> errorEP "ExactP: QualStmt: ThenBy is given wrong number of srcInfoPoints"
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
    ExpBracket l e  ->
        case srcInfoPoints l of
         [a,b] -> do
            printString "[|"
            exactPC e
            printStringAt (pos b) "|]"
         _ -> errorEP "ExactP: Bracket: ExpBracket is given wrong number of srcInfoPoints"
    PatBracket l p  ->
        case srcInfoPoints l of
         [a,b] -> do
            printString "[p|"
            exactPC p
            printStringAt (pos b) "|]"
         _ -> errorEP "ExactP: Bracket: PatBracket is given wrong number of srcInfoPoints"
    TypeBracket l t  -> do
        case srcInfoPoints l of
         [a,b] -> do
            printString "[t|"
            exactPC t
            printStringAt (pos b) "|]"
         _ -> errorEP "ExactP: Bracket: TypeBracket is given wrong number of srcInfoPoints"
    DeclBracket l ds ->
        case srcInfoPoints l of
         a:pts -> do
            printString "[d|"
            layoutList (init pts) (sepFunBinds ds)
            printStringAt (pos (last pts)) "|]"
         _ -> errorEP "ExactP: Bracket: DeclBracket is given too few srcInfoPoints"

instance ExactP XAttr where
  exactP (XAttr l xn e) =
    case srcInfoPoints l of
     [a] -> do
        exactP xn
        printStringAt (pos a) "="
        exactPC e
     _ -> errorEP "ExactP: XAttr is given wrong number of srcInfoPoints"

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
        len = length pts
        pars = len `div` 2
        (oPars,cParsWh) = splitAt pars pts
        (cPars,whPt) = splitAt pars cParsWh    -- whPt is either singleton or empty
    printStrs (zip oPars (repeat "("))
    exactPC n
    printStreams (zip (map pos cPars) (repeat $ printString ")")) (map (pos . ann &&& exactPC) ps)
    exactPC rhs
    maybeEP (\bds -> printStringAt (pos (head pts)) "where" >> exactPC bds) mbinds
  exactP (InfixMatch l a n bs rhs mbinds) = do
    let pts = srcInfoPoints l
        len = length pts
        pars = len `div` 2
        (oPars,cParsWh) = splitAt pars pts
        (cPars,whPt) = splitAt pars cParsWh    -- whPt is either singleton or empty
    printStrs (zip oPars (repeat "("))
    exactPC a
    epInfixName n
    printInterleaved' (zip cPars (repeat ")")) bs
    exactPC rhs
    maybeEP (\bds -> printStringAt (pos (head whPt)) "where" >> exactPC bds) mbinds

instance ExactP Rhs where
  exactP (UnGuardedRhs l e) = printString "=" >> exactPC e
  exactP (GuardedRhss  l grhss) = mapM_ exactPC grhss

instance ExactP GuardedRhs where
  exactP (GuardedRhs l ss e) =
    case srcInfoPoints l of
     a:pts -> do
        printString "|"
        printInterleaved' (zip (init pts) (repeat ",") ++ [(last pts, "=")]) ss
        exactPC e
     _ -> errorEP "ExactP: GuardedRhs is given wrong number of srcInfoPoints"

instance ExactP Pat where
  exactP pat = case pat of
    PVar l n    -> exactP n
    PLit l lit  -> exactP lit
    PNeg l p    -> printString "-" >> exactPC p
    PNPlusK l n k   ->
        case srcInfoPoints l of
         [a,b] -> do
            exactP n
            printStringAt (pos a) "+"
            printStringAt (pos b) (show k)
         _ -> errorEP "ExactP: Pat: PNPlusK is given wrong number of srcInfoPoints"
    PInfixApp l pa qn pb -> exactP pa >> epInfixQName qn >> exactPC pb
    PApp l qn ps    -> exactP qn >> mapM_ exactPC ps
    PTuple l bx ps ->
        case bx of
          Boxed   -> parenList (srcInfoPoints l) ps
          Unboxed -> parenHashList (srcInfoPoints l) ps
    PList l ps  -> squareList (srcInfoPoints l) ps
    PParen l p  -> parenList (srcInfoPoints l) [p]
    PRec l qn pfs   -> exactP qn >> curlyList (srcInfoPoints l) pfs
    PAsPat l n p    ->
        case srcInfoPoints l of
         [a] -> do
            exactP n
            printStringAt (pos a) "@"
            exactPC p
         _ -> errorEP "ExactP: Pat: PAsPat is given wrong number of srcInfoPoints"
    PWildCard l -> printString "_"
    PIrrPat l p -> printString "~" >> exactPC p
    PatTypeSig l p t ->
        case srcInfoPoints l of
         [a] -> do
            exactP p
            printStringAt (pos a) "::"
            exactPC t
         _ -> errorEP "ExactP: Pat: PatTypeSig is given wrong number of srcInfoPoints"
    PViewPat l e p ->
        case srcInfoPoints l of
         [a] -> do
            exactP e
            printStringAt (pos a) "->"
            exactPC p
         _ -> errorEP "ExactP: Pat: PViewPat is given wrong number of srcInfoPoints"
    PRPat l rps -> squareList (srcInfoPoints l) rps
    PXTag l xn attrs mat ps ->
        case srcInfoPoints l of
         [a,b,c,d,e] -> do
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
         -- Optional semi
         [a,b,semi,c,d,e] -> do
            printString "<"
            exactPC xn
            mapM_ exactPC attrs
            maybeEP exactPC mat
            printStringAt (pos b) ">"
            mapM_ exactPC ps
            printSemi semi
            printStringAt (pos c) "</"
            printWhitespace (pos d)
            exactP xn
            printStringAt (pos e) ">"
         _ -> errorEP "ExactP: Pat: PXTag is given wrong number of srcInfoPoints"
    PXETag l xn attrs mat ->
        case srcInfoPoints l of
         [a,b] -> do
            printString "<"
            exactPC xn
            mapM_ exactPC attrs
            maybeEP exactPC mat
            printStringAt (pos b) "/>"
         _ -> errorEP "ExactP: Pat: PXETag is given wrong number of srcInfoPoints"
    PXPcdata l str -> printString str
    PXPatTag l p   ->
        case srcInfoPoints l of
         [a,b] -> do
            printString "<%"
            exactPC p
            printString "%>"
         _ -> errorEP "ExactP: Pat: PXPatTag is given wrong number of srcInfoPoints"
    PXRPats  l rps  -> bracketList ("<[",",","]>") (srcInfoPoints l) rps
    PExplTypeArg l qn t ->
        case srcInfoPoints l of
         [a,b] -> do
            exactP qn
            printStringAt (pos a) "{|"
            exactPC t
            printStringAt (pos b) "|}"
         _ -> errorEP "ExactP: Pat: PExplTypeArg is given wrong number of srcInfoPoints"
    PQuasiQuote l name qt   -> printString $ "[$" ++ name ++ "|" ++ qt ++ "]"
    PBangPat l p    -> printString "!" >> exactPC p

instance ExactP PatField where
  exactP pf = case pf of
    PFieldPat l qn p        ->
        case srcInfoPoints l of
         [a] -> do
            exactP qn
            printStringAt (pos a) "="
            exactPC p
         _ -> errorEP "ExactP: PatField: PFieldPat is given wrong number of srcInfoPoints"
    PFieldPun l n   -> exactP n
    PFieldWildcard l -> printString ".."

instance ExactP RPat where
  exactP rpat = case rpat of
    RPOp l rp op    -> exactP rp >> exactPC op
    RPEither l r1 r2 ->
      case srcInfoPoints l of
       [a] -> do
          exactP r1
          printStringAt (pos a) "|"
          exactPC r2
       _ -> errorEP "ExactP: RPat: RPEither is given wrong number of srcInfoPoints"
    RPSeq l rps -> bracketList ("(|",",","|)") (srcInfoPoints l) rps
    RPGuard l p stmts   ->
      case srcInfoPoints l of
       a:pts -> do
          printString "(|"
          exactPC p
          bracketList ("|",",","|)") pts stmts
       _ -> errorEP "ExactP: RPat: RPGuard is given wrong number of srcInfoPoints"
    RPCAs l n rp    ->
      case srcInfoPoints l of
       [a] -> do
          exactP n
          printStringAt (pos a) "@:"
          exactPC rp
       _ -> errorEP "ExactP: RPat: RPCAs is given wrong number of srcInfoPoints"
    RPAs l n rp     ->
      case srcInfoPoints l of
       [a] -> do
          exactP n
          printStringAt (pos a) "@"
          exactPC rp
       _ -> errorEP "ExactP: RPat: RPAs is given wrong number of srcInfoPoints"
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
  exactP (PXAttr l xn p) =
    case srcInfoPoints l of
     [a] -> do
        exactP xn
        printStringAt (pos a) "="
        exactPC p
     _ -> errorEP "ExactP: PXAttr is given wrong number of srcInfoPoints"

instance ExactP XName where
  exactP xn = case xn of
    XName l name -> printString name
    XDomName l dom name ->
        case srcInfoPoints l of
         [a,b,c] -> do
            printString dom
            printStringAt (pos b) ":"
            printStringAt (pos c) name
         _ -> errorEP "ExactP: XName: XDomName is given wrong number of srcInfoPoints"

instance ExactP Binds where
  exactP (BDecls  l ds)  = layoutList (srcInfoPoints l) (sepFunBinds ds)
  exactP (IPBinds l ips) = layoutList (srcInfoPoints l) ips

instance ExactP CallConv where
  exactP (StdCall   _) = printString "stdcall"
  exactP (CCall     _) = printString "ccall"
  exactP (CPlusPlus _) = printString "cplusplus"
  exactP (DotNet    _) = printString "dotnet"
  exactP (Jvm       _) = printString "jvm"
  exactP (Js        _) = printString "js"
  exactP (CApi      _) = printString "capi"

instance ExactP Safety where
  exactP (PlayRisky _) = printString "unsafe"
  exactP (PlaySafe _ b) = printString $ if b then "threadsafe" else "safe"
  exactP (PlayInterruptible _) = printString "interruptible"

instance ExactP Rule where
  exactP (Rule l str mact mrvs e1 e2) =
    case srcInfoPoints l of
     a:pts -> do
        printString (show str)
        maybeEP exactP mact
        pts <- case mrvs of
                Nothing -> return pts
                Just rvs ->
                    case pts of
                     a:b:pts' -> do
                        printStringAt (pos a) "forall"
                        mapM_ exactPC rvs
                        printStringAt (pos b) "."
                        return pts'
                     _ -> errorEP "ExactP: Rule is given too few srcInfoPoints"
        case pts of
         [x] -> do
            exactPC e1
            printStringAt (pos x) "="
            exactPC e2
         _ -> errorEP "ExactP: Rule is given wrong number of srcInfoPoints"
     _ -> errorEP "ExactP: Rule is given too few srcInfoPoints"

instance ExactP RuleVar where
  exactP (TypedRuleVar l n t) = do
        case srcInfoPoints l of
         [a,b,c] -> do
            printString "("
            exactPC n
            printStringAt (pos b) "::"
            exactPC t
            printStringAt (pos c) ")"
         _ -> errorEP "ExactP: RuleVar: TypedRuleVar is given wrong number of srcInfoPoints"
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
    case srcInfoPoints l of
     [a] -> do
        exactP ipn
        printStringAt (pos a) "="
        exactPC e
     _ -> errorEP "ExactP: IPBind is given wrong number of srcInfoPoints"

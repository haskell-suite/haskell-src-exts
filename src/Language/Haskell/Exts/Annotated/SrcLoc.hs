{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Language.Haskell.Exts.Annotated.SrcLoc where

#ifdef __GLASGOW_HASKELL__
#ifdef BASE4
import Data.Data
#else
import Data.Generics (Data(..),Typeable(..))
#endif
#endif

-- | A single position in the source.
data SrcLoc = SrcLoc
    { srcFilename :: String
    , srcLine :: Int
    , srcColumn :: Int
    }
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif


-- | A portion of the source, spanning one or more lines and zero or more columns.
data SrcSpan = SrcSpan
    { srcSpanFilename    :: String
    , srcSpanStartLine   :: Int
    , srcSpanStartColumn :: Int
    , srcSpanEndLine     :: Int
    , srcSpanEndColumn   :: Int
    }
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Combine two locations in the source to denote a span.
mkSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan (SrcLoc fn sl sc) (SrcLoc _ el ec) = SrcSpan fn sl sc el ec

-- | Merge two source spans into a single span from the start of the first
--   to the end of the second. Assumes that the two spans are given in the
--   order they appear in the source.
mergeSrcSpan :: SrcSpan -> SrcSpan -> SrcSpan
mergeSrcSpan (SrcSpan fn sl sc _ _) (SrcSpan _ _ _ el ec) = SrcSpan fn sl sc el ec

isNullSpan ss = srcSpanStartLine ss == srcSpanEndLine ss &&
                    srcSpanStartColumn ss >= srcSpanEndColumn ss

-- | An entity located in the source.
data Loc a = Loc
    { loc :: SrcSpan
    , unLoc :: a
    }
  deriving (Eq,Ord,Show)


-- | A portion of the source, extended with information on the position of entities within the span.
data SrcSpanInfo = SrcSpanInfo
    { srcInfoSpan    :: SrcSpan
--    , explLayout     :: Bool
    , srcInfoPoints  :: [SrcSpan]    -- Marks the location of specific entities inside the span
    }
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

nIS, noInfoSpan :: SrcSpan -> SrcSpanInfo
noInfoSpan ss = SrcSpanInfo ss []

nIS = noInfoSpan

pIS, pointInfoSpan :: SrcSpan -> SrcSpanInfo
pointInfoSpan ss = SrcSpanInfo ss [ss]

pIS = pointInfoSpan

iS, infoSpan :: SrcSpan -> [SrcSpan] -> SrcSpanInfo
infoSpan x y = SrcSpanInfo x y

iS = infoSpan

(<++>), combSpanInfo :: SrcSpanInfo -> SrcSpanInfo -> SrcSpanInfo
combSpanInfo s1 s2 = SrcSpanInfo
    (mergeSrcSpan (srcInfoSpan s1) (srcInfoSpan s2))
    []

(<++>) = combSpanInfo

(<+?>) :: SrcSpanInfo -> Maybe SrcSpanInfo -> SrcSpanInfo
a <+?> b = case b of {Nothing -> a; Just b -> a <++> b}

(<?+>) :: Maybe SrcSpanInfo -> SrcSpanInfo -> SrcSpanInfo
a <?+> b = case a of {Nothing -> b; Just a -> a <++> b}

(<**) :: SrcSpanInfo -> [SrcSpan] -> SrcSpanInfo
ss@(SrcSpanInfo {srcInfoPoints = ps}) <** xs = ss {srcInfoPoints = ps ++ xs}

--(<??) :: SrcSpanInfo -> Bool -> SrcSpanInfo
--ss <?? b = ss { explLayout = b }

(<^^>) :: SrcSpan -> SrcSpan -> SrcSpanInfo
a <^^> b = nIS (mergeSrcSpan a b)

infixl 6 <^^>
infixl 5 <++>
infixl 4 <**, <+?>, <?+>

class SrcInfo si where
  toSrcInfo   :: SrcLoc -> [SrcSpan] -> SrcLoc -> si
  fromSrcInfo :: SrcSpanInfo -> si
  getPointLoc :: si -> SrcLoc
  fileName    :: si -> String
  startLine   :: si -> Int
  startColumn :: si -> Int

  getPointLoc si = SrcLoc (fileName si) (startLine si) (startColumn si)

instance SrcInfo SrcLoc where
  toSrcInfo s _ _ = s
  fromSrcInfo si = SrcLoc (fileName si) (startLine si) (startColumn si)
  fileName = srcFilename
  startLine = srcLine
  startColumn = srcColumn

instance SrcInfo SrcSpan where
  toSrcInfo st _ end = mkSrcSpan st end
  fromSrcInfo = srcInfoSpan
  fileName = srcSpanFilename
  startLine = srcSpanStartLine
  startColumn = srcSpanStartColumn

instance SrcInfo SrcSpanInfo where
  toSrcInfo st pts end = SrcSpanInfo (mkSrcSpan st end) pts
  fromSrcInfo = id
  fileName = fileName . srcInfoSpan
  startLine = startLine . srcInfoSpan
  startColumn = startColumn . srcInfoSpan

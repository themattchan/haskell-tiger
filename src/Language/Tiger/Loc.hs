-- | Source locations
module Language.Tiger.Loc where

import Data.Semigroup

-- This is AlexPosn but in my own type because i can't figure out how to import it
data SrcPosn = SrcPosn !Int  -- absolute character offset
                       !Int  -- line number
                       !Int  -- column number
  deriving (Show, Eq)

data SrcSpan = SrcSpan
  { srcSpanStartLine  :: !Int
  , srcSpanStartCol   :: !Int
  , srcSpanEndLine    :: !Int
  , srcSpanEndCol     :: !Int
  } deriving (Show, Eq, Ord)

-- mkSrcSpan :: SrcPosn -> SrcPosn -> SrcSpan
-- mkSrcSpan (SrcPosn _ sl sc) (SrcPosn _ el ec) = SrcSpan sl sc el ec

posnToSpan :: SrcPosn -> SrcSpan
posnToSpan (SrcPosn _ sl sc) = SrcSpan sl sc sl sc

instance Semigroup SrcSpan where
  x@(SrcSpan sl1 sc1 el1 ec1) <> y@(SrcSpan sl2 sc2 el2 ec2)
    | x == mempty = y
    | y == mempty = x
    | otherwise
    = SrcSpan (min sl1 sl2) (min sc1 sc2) (max el1 el2) (max ec1 ec2)

instance Monoid SrcSpan where
  mempty = SrcSpan 0 0 0 0
  mappend = (<>)

spanSize :: SrcSpan -> (Int, Int)
spanSize ss = (srcSpanEndLine ss - srcSpanStartLine ss, max 0 (srcSpanEndCol ss - srcSpanStartCol ss))

data Loc a = Loc { locPosn :: SrcPosn, locData :: a }
  deriving Show

data Span a = Span { spanSpan :: SrcSpan, spanData :: a }

instance Show a => Show (Span a) where
  show (Span ss a) = show a <> " at location " <> show ss

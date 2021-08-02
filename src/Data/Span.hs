module Data.Span where

data SourcePos = SourcePos
  { line :: !Int,
    col :: !Int
  }
  deriving (Show, Eq, Ord)

data Span = Span
  { col1 :: !Int,
    line1 :: !Int,
    col2 :: !Int,
    line2 :: !Int
  }
  deriving (Show, Eq, Ord)

class Spanned a where
  spanOf :: a -> Span

instance Spanned Span where
  spanOf = id

mkSpan :: SourcePos -> SourcePos -> Span
mkSpan pos1 pos2 = Span {col1 = col pos1, line1 = line pos1, col2 = col pos2, line2 = line pos2}
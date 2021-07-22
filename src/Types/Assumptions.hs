module Types.Assumptions
  ( Assumptions (..),
    empty,
    remove,
    lookup,
    merge,
    singleton,
    keys,
    add,
  )
where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Types.Type (Type)
import Prelude hiding (lookup)

newtype Assumptions = Assumptions (Seq (Text, Type))
  deriving (Eq)

empty :: Assumptions
empty = Assumptions Seq.empty

remove :: Assumptions -> Text -> Assumptions
remove (Assumptions as) var = Assumptions (Seq.filter (\(n, _) -> n /= var) as)

lookup :: Text -> Assumptions -> Seq Type
lookup key (Assumptions as) = snd <$> Seq.filter (\(n, _) -> n == key) as

add :: Assumptions -> (Text, Type) -> Assumptions
add (Assumptions as) a = Assumptions $ as Seq.|> a

merge :: Assumptions -> Assumptions -> Assumptions
merge (Assumptions as) (Assumptions as') = Assumptions $ as <> as'

singleton :: Text -> Type -> Assumptions
singleton x y = Assumptions $ Seq.singleton (x, y)

keys :: Assumptions -> Seq Text
keys (Assumptions as) = fst <$> as

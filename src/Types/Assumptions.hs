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

import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Data.HashSet (HashSet)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Types.Type (Type)
import Prelude hiding (lookup)

newtype Assumptions = Assumptions (HashMap Text (Seq Type))
  deriving (Show)

empty :: Assumptions
empty = Assumptions HMap.empty

remove :: Assumptions -> Text -> Assumptions
remove (Assumptions m) var = Assumptions $ HMap.delete var m

lookup :: Text -> Assumptions -> Seq Type
lookup key (Assumptions m) = HMap.lookup key m & fromMaybe Seq.empty

add :: Assumptions -> (Text, Type) -> Assumptions
add (Assumptions m) (n, t) = HMap.insert n (ts Seq.|> t) m & Assumptions
  where
    ts = HMap.lookupDefault Seq.empty n m

merge :: Assumptions -> Assumptions -> Assumptions
merge (Assumptions as) (Assumptions as') = Assumptions $ as <> as'

singleton :: Text -> Type -> Assumptions
singleton x y = Assumptions $ HMap.singleton x (Seq.singleton y)

keys :: Assumptions -> HashSet Text
keys (Assumptions m) = HMap.keysSet m

module Protolude
  ( (|>),
    Text,
    module Data.Maybe,
    module Data.List,
    module Data.Either,
  )
where

import Data.Function ((&))
import Data.Text (Text)
import Data.Maybe
import Data.Either
import Data.List hiding (lookup)

(|>) :: a -> (a -> b) -> b
(|>) = (&)

infixl 1 |>

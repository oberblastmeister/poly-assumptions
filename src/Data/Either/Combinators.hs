module Data.Either.Combinators
  ( fromRight',
    mapLeft,
  )
where

import Control.Monad.Except

fromRight' :: Show a => Either a b -> b
fromRight' (Right b) = b
fromRight' (Left a) = error $ "Either was not right: " ++ show a

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b
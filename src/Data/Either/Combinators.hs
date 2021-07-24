module Data.Either.Combinators
  ( fromRight',
  )
where

fromRight' :: Show a => Either a b -> b
fromRight' (Right b) = b
fromRight' (Left a) = error $ "Either was not right: " ++ show a

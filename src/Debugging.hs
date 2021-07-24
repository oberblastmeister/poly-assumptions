module Debugging
  ( dbg,
    dbg',
  )
where

import Debug.Trace

dbg :: Show a => a -> a
dbg x = trace ("DBG: " ++ show x) x

dbg' :: Show a => String -> a -> a
dbg' tag x = trace ("DBG " ++ tag ++ ": " ++ show x) x

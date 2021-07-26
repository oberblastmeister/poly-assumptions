module Debugging
  ( dbg,
    dbg',
    dbgP,
    dbgP',
  )
where

import qualified Data.Text as T
import Debug.Trace
import Prettyprinter (Pretty, pretty)
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as P.Render.Text

dbg :: Show a => a -> a
dbg x = trace ("DBG: " ++ show x) x

dbgP :: Pretty a => a -> a
dbgP x =
  trace
    ( T.unpack $
        "DBG: "
          <> P.Render.Text.renderStrict
            (P.layoutPretty P.defaultLayoutOptions $ pretty x)
    )
    x

dbgP' :: (a -> P.Doc ann) -> a -> a
dbgP' pp x =
  trace
    ( T.unpack $
        "DBG: "
          <> P.Render.Text.renderStrict
            (P.layoutPretty P.defaultLayoutOptions $ pp x)
    )
    x

dbg' :: Show a => String -> a -> a
dbg' tag x = trace ("DBG " ++ tag ++ ": " ++ show x) x

module Pretty where

import Prettyprinter

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

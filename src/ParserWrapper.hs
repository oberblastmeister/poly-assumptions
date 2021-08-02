module ParserWrapper where

import qualified LexerWrapper

data ParseError
  = LexError [LexerWrapper.LexError]
  | ParseError String
  deriving (Show, Eq)
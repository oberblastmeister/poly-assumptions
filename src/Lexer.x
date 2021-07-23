{
module Lexer (
  scanTokens
) where

import Protolude
import qualified Syntax.Token as Tok
import Syntax.Token (Token)
import Data.Maybe (fromJust)
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol = [\n]

tokens :-
  $eol;
  $white+;
  "--".*;

  "let" { tok Tok.Let }
  "in" { tok Tok.In }
  "True" { tok Tok.True }
  "False" { tok Tok.False }
  $digit+ { string $ Tok.Num . read }
  "->" { tok Tok.Arrow }
  '=' { tok Tok.Eq }
  "\\" { tok Tok.Lambda }
  '+' { tok Tok.Add }
  '-' { tok Tok.Sub }
  '*' { tok Tok.Mul }
  '/' { tok Tok.Div }
  "(" { tok Tok.LParen }
  ")" { tok Tok.RParen }

{
tok :: Token -> String -> Token
tok t s = t

string :: (String -> Token) -> String -> Token
string f s = f s

scanTokens :: String -> Either String [Token]
scanTokens str = go ('\n',[],str) where 
  go inp@(_,_bs,str) =
    case alexScan inp 0 of
     AlexEOF -> return []
     AlexError _ -> Left "Invalid lexeme."
     AlexSkip  inp' len -> go inp'
     AlexToken inp' len act -> do
      res <- go inp'
      let rest = act (take len str)
      return (rest : res)
}

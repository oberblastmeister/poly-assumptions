{
module Lexer (
  scanTokens
) where

import Protolude
import qualified Syntax.Token as Tok
import qualified Data.Text as T
import Syntax.Token (Token)
import Data.Maybe (fromJust)
}

%wrapper "basic"

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]
$ident = [$digit $upper $lower '_' '\'']
$eol = [\n]
-- we put this separate because alex doesn't do string escapes like haskell
-- inside literal strings which is confusing but it does in regex
$lam = [\\]

tokens :-
  $eol;
  $white+;
  "--".*;

  "forall" { tok Tok.Forall }
  "let" { tok Tok.Let }
  "rec" { tok Tok.Rec }
  "in" { tok Tok.In }
  "True" { tok Tok.True }
  "False" { tok Tok.False }
  $digit+ { string $ Tok.Num . read }
  "->" { tok Tok.Arrow }
  "=" { tok Tok.Eq }
  $lam { tok Tok.Lambda }
  "+" { tok Tok.Add }
  "-" { tok Tok.Sub }
  "*" { tok Tok.Mul }
  "/" { tok Tok.Div }
  "(" { tok Tok.LParen }
  ")" { tok Tok.RParen }
  "." { tok Tok.Dot }

  $lower $ident* { string $ Tok.Ident . T.pack }
  $upper $ident* { string $ Tok.ConIdent . T.pack }

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
     AlexError t -> Left $ "Invalid lexeme: " ++ show t
     AlexSkip  inp' len -> go inp'
     AlexToken inp' len act -> do
      res <- go inp'
      let rest = act (take len str)
      return (rest : res)
}

{
module Parser (
  parseExpr,
  parseTokens,
) where

import Lexer
import Syntax.Expr (Expr)
import qualified Syntax.Expr as Expr
import qualified Types.Type as T
import Types.Type (Type)
import Syntax.Token (Token)
import qualified Syntax.Token as Tok
}

%name expr Expr
%name ty Ty
%name scheme Scheme

%tokentype { Token }
%monad { Either String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    forall { Tok.Forall }
    let { Tok.Let }
    true { Tok.True }
    false { Tok.False }
    in { Tok.In }
    num { Tok.Num $$ }
    ident { Tok.Ident $$ }
    '\\' { Tok.Lambda }
    '->' { Tok.Arrow }
    '=' { Tok.Eq }
    '+' { Tok.Add }
    '-' { Tok.Sub }
    '*' { Tok.Mul }
    '/' { Tok.Div }
    '(' { Tok.LParen }
    ')' { Tok.RParen }
    '.' { Tok.Dot }

%%

Expr
  : AddExpr { $1 }
  | let ident '=' Expr in Expr { Expr.App (Expr.Lam $2 $6) $4 }
  | '\\' ident '->' Expr { Expr.Lam $2 $4 }

AddExpr
  : MulExpr { $1 }
  | AddExpr '+' MulExpr { Expr.Bin $1 Expr.Add $3 }
  | AddExpr '-' MulExpr { Expr.Bin $1 Expr.Sub $3 }

MulExpr
  : AtomExpr { $1 }
  | MulExpr '*' AtomExpr { Expr.Bin $1 Expr.Mul $3 }
  | MulExpr '/' AtomExpr { Expr.Bin $1 Expr.Div $3 }

AtomExpr
  : '(' Expr ')' { $2 }
  | num { Expr.Lit (Expr.LInt $1) }
  | ident { Expr.Var $1 }
  | true { Expr.Lit (Expr.LBool True) }
  | false { Expr.Lit (Expr.LBool False) }

Scheme
  : forall ListE1(ident) '.' Ty { T.Forall [] $4 }

Ty
  : ArrowType { $1 }

ArrowType
  : AtomType { $1 }
  | AtomType '->' ArrowType { $1 T.:-> $3 }

AtomType
  : '(' Ty ')' { $2 }
  | ident { T.Con $1 }

-- | A non-empty list with no separator
ListE1(p)
     : p                { [$1] }
     | p ListE1(p)      { $1 : $2 }
{
parseError :: [Token] -> Either String a
parseError (l:ls) = Left (show l)
parseError [] = Left "Unexpected end of Input"

type Lexer = String -> Either String [Token]

type Parser a = [Token] -> Either String a

parse :: Parser a -> String -> Either String a
parse parser input = do
  tokenStream <- scanTokens input
  parser tokenStream

parseExpr :: String -> Either String Expr
parseExpr = parse expr

parseType :: String -> Either String Type
parseType = parse ty

parseScheme :: String -> Either String T.Scheme
parseScheme = parse scheme

parseTokens :: String -> Either String [Token]
parseTokens = scanTokens
}

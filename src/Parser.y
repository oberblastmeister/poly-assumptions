{
module Parser (
  parseExpr,
  parseTokens,
) where

import Lexer
import Syntax.Expr (Expr)
import qualified Syntax.Expr as Expr
import Token (Token)
import qualified Token as Tok
}

-- -- Entry point
%name expr

%tokentype { Token }
%monad { Either String } { (>>=) } { return }
-- %lexer { alexScanTokens }
%error { parseError }

-- Token Names
%token
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
  : Atom { $1 }
  | MulExpr '*' Atom { Expr.Bin $1 Expr.Mul $3 }
  | MulExpr '/' Atom { Expr.Bin $1 Expr.Div $3 }

Atom
  : '(' Expr ')' { $2 }
  | num { Expr.Lit (Expr.LInt $1) }
  | ident { Expr.Var $1 }
  | true { Expr.Lit (Expr.LBool True) }
  | false { Expr.Lit (Expr.LBool False) }

{

parseError :: [Token] -> Either String a
parseError (l:ls) = Left (show l)
parseError [] = Left "Unexpected end of Input"

parseExpr :: String -> Either String Expr
parseExpr input = do
  tokenStream <- scanTokens input
  expr tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = scanTokens
    
}

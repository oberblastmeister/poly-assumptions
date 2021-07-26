{
module Parser (
  parseExpr,
  parseType,
  parseScheme,
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
    rec { Tok.Rec }
    true { Tok.True }
    false { Tok.False }
    in { Tok.In }
    if { Tok.If }
    else { Tok.Else }
    then { Tok.Then }
    num { Tok.Num $$ }
    ident { Tok.Ident $$ }
    con_ident { Tok.ConIdent $$ }
    '\\' { Tok.Lambda }
    '->' { Tok.Arrow }
    '=' { Tok.Assign }
    '==' { Tok.Eq }
    '!=' { Tok.NEq }
    '+' { Tok.Add }
    '-' { Tok.Sub }
    '*' { Tok.Mul }
    '/' { Tok.Div }
    '(' { Tok.LParen }
    ')' { Tok.RParen }
    '.' { Tok.Dot }

%%

Expr
  : EqExpr { $1 }
  | let ident '=' Expr in Expr { Expr.Let Expr.NonRec $2 $4 $6 }
  | let rec ident '=' Expr in Expr { Expr.Let Expr.Rec $3 $5 $7 }
  | if Expr then Expr else Expr { Expr.If $2 $4 $6 }
  | '\\' ListE1(ident) '->' Expr { foldr Expr.Lam $4 $2 }

EqExpr
  : AddExpr { $1 }
  | AddExpr '==' AddExpr { Expr.Bin $1 Expr.Eq $3 }
  | AddExpr '!=' AddExpr { Expr.Bin $1 Expr.NEq $3 }

AddExpr
  : MulExpr { $1 }
  | AddExpr '+' MulExpr { Expr.Bin $1 Expr.Add $3 }
  | AddExpr '-' MulExpr { Expr.Bin $1 Expr.Sub $3 }

MulExpr
  : AppExpr { $1 }
  | MulExpr '*' AppExpr { Expr.Bin $1 Expr.Mul $3 }
  | MulExpr '/' AppExpr { Expr.Bin $1 Expr.Div $3 }

AppExpr
  : AtomExpr { $1 }
  | AppExpr AtomExpr { Expr.App $1 $2 }

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
  | con_ident { T.Con $1 }

-- | A non-empty list with no separator
ListE1(p)
     : p                { [$1] }
     | p ListE1(p)      { $1 : $2 }
{
parseError :: [Token] -> Either String a
parseError (l:ls) = Left $ "Unexpected token: " ++ show l
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

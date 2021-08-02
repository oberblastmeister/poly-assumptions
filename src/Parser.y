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
import Syntax.Token (Token(..))
import qualified Syntax.Token.Kind as TK
import LexerWrapper (runAlex)
import Data.Function ((&))
import Data.Text (Text)
import ParserWrapper
import Data.Either.Combinators (mapLeft)
import Control.Monad.Except
import qualified LexerWrapper
}

%name expr Expr
%name ty Ty
%name scheme Scheme

%tokentype { Token }
%monad { Either ParseError } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    forall { Token TK.Forall _ }
    let { Token TK.Let _ }
    rec { Token TK.Rec _ }
    true { Token TK.True _ }
    false { Token TK.False _ }
    in { Token TK.In _ }
    if { Token TK.If _ }
    else { Token TK.Else _ }
    then { Token TK.Then _ }
    num { Token (TK.Num _) _ }
    ident { Token (TK.Ident _) _ }
    con_ident { Token (TK.ConIdent _) _ }
    '\\' { Token TK.Lambda _ }
    '->' { Token TK.Arrow _ }
    '=' { Token TK.Assign _ }
    '==' { Token TK.Eq _ }
    '!=' { Token TK.NEq _ }
    '+' { Token TK.Add _ }
    '-' { Token TK.Sub _ }
    '*' { Token TK.Mul _ }
    '/' { Token TK.Div _ }
    '(' { Token TK.LParen _ }
    ')' { Token TK.RParen _ }
    '.' { Token TK.Dot _ }

%%

Expr
  : EqExpr { $1 }
  | let ident '=' Expr in Expr { Expr.Let Expr.NonRec (getIdent $2) $4 $6 }
  | let rec ident '=' Expr in Expr { Expr.Let Expr.Rec (getIdent $3) $5 $7 }
  | if Expr then Expr else Expr { Expr.If $2 $4 $6 }
  | '\\' ListE1(ident) '->' Expr { foldr Expr.Lam $4 (fmap getIdent $2) }

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
  | num { Expr.Lit $ Expr.LInt $ getNum $1 }
  | ident { Expr.Var $ getIdent $1 }
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
  | con_ident { T.Con (getIdent $1) }

-- | A non-empty list with no separator
ListE1(p)
     : p                { [$1] }
     | p ListE1(p)      { $1 : $2 }
{
getNum :: Token -> Int
getNum tok = case kind tok of
  TK.Num x -> x
  _ -> error "cannot get number from this token"

getIdent :: Token -> Text
getIdent tok = case kind tok of
  TK.Ident x -> x
  TK.ConIdent x -> x
  _ -> error "cannot get ident from this token"
  
getIdents :: [Token] -> [Text]
getIdents = fmap getIdent
  
parseError :: [Token] -> Either ParseError a
parseError (l:ls) = Left $ ParseError $ "Unexpected token: " ++ show l
parseError [] = Left $ ParseError "Unexpected end of Input"

-- type Lexer = String -> Either String [Token]

type Parser a = [Token] -> Either ParseError a

parse :: MonadError ParseError m => Parser a -> Text -> m a
parse parser input = liftEither $ do
  tokenStream <- runAlex input lexTokens & mapLeft LexError
  parser tokenStream

parseExpr :: MonadError ParseError m => Text -> m Expr
parseExpr = parse expr

parseType :: MonadError ParseError m => Text -> m Type
parseType = parse ty

parseScheme :: MonadError ParseError m => Text -> m T.Scheme
parseScheme = parse scheme

parseTokens :: Text -> Either [LexerWrapper.LexError] [Token]
parseTokens input = runAlex input lexTokens
}

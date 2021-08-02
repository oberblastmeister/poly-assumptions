{
module Lexer where

import LexerWrapper
import Syntax.Token (Token(..))
import Syntax.Token.Kind (TokenKind)
import qualified Syntax.Token.Kind as TK
import Data.Text (Text)
import qualified Data.Text as T
import Data.Span (Span)
import qualified Data.Span as Span
import GHC.Records
import Control.Monad.Reader
import Control.Monad.Writer 
import Data.Function ((&))
import qualified Data.DList as DL
}

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]
$ident = [$digit $upper $lower '_' '\'']
$eol = [\n]
-- we put this separate because alex doesn't do string escapes like haskell
-- inside literal strings which is confusing but it does in regex
$lam = [\\]

tokens :-
  <0> $eol { skip }
  <0> $white+ { skip }
  <0> "--".* { skip }

  <0> "forall" { tk TK.Forall }
  <0> "let" { tk TK.Let }
  <0> "rec" { tk TK.Rec }
  <0> "in" { tk TK.In }
  <0> "if" { tk TK.If }
  <0> "then" { tk TK.Then }
  <0> "else" { tk TK.Else }
  <0> "True" { tk TK.True }
  <0> "False" { tk TK.False }
  <0> $digit+ { string $ TK.Num . read . T.unpack }
  <0> "->" { tk TK.Arrow }
  <0> "=" { tk TK.Assign }
  <0> "==" { tk TK.Eq }
  <0> "!=" { tk TK.NEq }
  <0> $lam { tk TK.Lambda }
  <0> "+" { tk TK.Add }
  <0> "-" { tk TK.Sub }
  <0> "*" { tk TK.Mul }
  <0> "/" { tk TK.Div }
  <0> "(" { tk TK.LParen }
  <0> ")" { tk TK.RParen }
  <0> "." { tk TK.Dot }

  <0> $lower $ident* { string $ TK.Ident }
  <0> $upper $ident* { string $ TK.ConIdent }
  
  <0> .
    { do
        tokText <- asks tokText
        span <- asks $ getField @"span"
        tell $ DL.singleton $ LexError (UnknownToken tokText) span
        return $ Token TK.Error span
    }
  
{
skip :: AlexAction Token
skip = lift $ alexMonadScan

begin :: Int -> AlexAction Token
begin sc = lift $ do
  alexSetStartCode sc
  alexMonadScan

alexMonadScan :: Alex Token
alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError inp ->
      error $ "BUG: should not happen, use the MonadWriter instance to report errors\n\nThe input was: " ++ show inp
    AlexSkip inp' _len -> do
      alexSetInput inp'
      alexMonadScan
    AlexToken inp' len action -> do
      alexSetInput inp'
      let tokText = T.take len $ getField @"text" inp
      let pos1 = getField @"pos" inp
          pos2 = getField @"pos" inp'
          span = Span.mkSpan pos1 pos2
      let env = AlexEnv { tokText, span }
      action
        & (`runReaderT` env)

lexUntilEOF :: Alex Token -> Alex [Token]
lexUntilEOF lexer = do
  tok <- lexer
  case kind $ tok of
    TK.EOF -> pure []
    _ -> (tok :) <$> lexUntilEOF lexer
    
lexTokens :: Alex [Token]
lexTokens = lexUntilEOF alexMonadScan
}

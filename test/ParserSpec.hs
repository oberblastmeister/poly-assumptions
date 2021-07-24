module ParserSpec (spec) where

import qualified Parser
import Syntax.Expr (Expr)
import qualified Syntax.Expr as Expr
import Syntax.Token (Token)
import qualified Syntax.Token as Tok
import Test.Hspec
import qualified Types.Type as Type
import Types.Type (Type)

check :: HasCallStack => String -> Either String Expr -> Expectation
check s expected = do
  let res = Parser.parseExpr s
  res `shouldBe` expected

checkTy :: HasCallStack => String -> Either String Type -> Expectation
checkTy s expected = do
  let res = Parser.parseType s
  res `shouldBe` expected

checkLex :: HasCallStack => String -> Either String [Token] -> Expectation
checkLex s expected = do
  let res = Parser.parseTokens s
  res `shouldBe` expected

spec :: Spec
spec = parallel $ do
  it "should work" $ do
    check "234124" (Right $ Expr.Lit $ Expr.LInt 234124)

  it "should do comment" $ do
    checkLex "-- adsfasdf asdfasd" (Right [])

  it "should lex lambda" $ do
    checkLex "\\" (Right [Tok.Lambda])

  it "should lex" $ do
    -- checkLex "(*) True +->" (Right [Tok.LParen, Tok.Mul, Tok.RParen, Tok.True, Tok.Add, Tok.Arrow])
    checkLex "x" (Right [Tok.Ident "x"])
    -- checkLex "Int" (Right [Tok.ConIdent "Int"])

  it "should parse lambda" $ do
    check "\\x -> x" (Right (Expr.Lam "x" (Expr.Var "x")))

  it "should parse literals" $ do
    check "1234" (Right (Expr.Lit $ Expr.LInt 1234))
    check "True" (Right (Expr.Lit $ Expr.LBool True))

  it "should parse tycon" $ do
    checkTy "Int" (Right (Type.Con "Int"))

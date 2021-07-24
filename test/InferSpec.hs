module InferSpec (spec) where

import Control.Monad.Except
import Control.Monad.Supply
import Data.Either (fromLeft)
import Data.Either.Combinators
import Data.Function ((&))
import Data.Text (Text)
import qualified Parser
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as P.Render.Text
import Syntax.Expr (Expr)
import qualified Syntax.Expr as Expr
import Test.Hspec
import qualified Types.Infer as Infer
import Types.Infer.Monad (TypeError, varSupply)
import Types.Type (Type)
import qualified Types.Type as T

testInferExpr :: Expr -> Either TypeError T.Scheme
testInferExpr e = runExcept $ evalSupplyT (Infer.inferExpr e) varSupply

check :: HasCallStack => String -> Text -> Expectation
check s expected = do
  let ex = fromRight' $ Parser.parseExpr s
  let !ty = fromRight' $ runSupplyT (Infer.inferExpr ex) varSupply
  let !actual = P.pretty ty & P.layoutPretty P.defaultLayoutOptions & P.Render.Text.renderStrict
  "" `shouldBe` expected

test :: HasCallStack => (String, String, Text) -> SpecWith (Arg Expectation)
test (name, s, expected) = it name (check s expected)

tests :: HasCallStack => [(String, String, Text)] -> SpecWith (Arg Expectation)
tests = mapM_ test

spec :: Spec
spec = parallel $ do
  it "" $ do
    1 `shouldBe` 1
  
  -- it "should work" $ do
  --   check "1234" "Int"
  -- test ("", "1234", "a -> a")

module PrettySpec (spec) where

import Data.Either.Combinators
import Data.Text (Text)
import Data.Function ((&))
import qualified Parser
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as P.Render.Text
import Test.Hspec

checkSame :: Text -> Expectation
checkSame s = do
  let expr = fromRight' $ Parser.parseExpr s
  let actual = P.pretty expr & P.layoutCompact & P.Render.Text.renderStrict
  s `shouldBe` actual

checkSameTy :: Text -> Expectation
checkSameTy s = do
  let ty = fromRight' $ Parser.parseType s
  let actual = P.pretty ty & P.layoutCompact & P.Render.Text.renderStrict
  s `shouldBe` actual

spec :: Spec
spec = parallel $ do
  it "should work for expr" $ do
    checkSame "\\x -> x"
    -- checkSame "let x = 1234 in x"
  -- it "should work for ty" $ do
  --   checkSame "Int"

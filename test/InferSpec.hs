module InferSpec (spec) where

import Control.Monad.Supply
import Data.Either.Combinators
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Parser
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as P.Render.Text
import Test.Hspec
import qualified Types.Infer as Infer
import Types.Infer.Monad (TypeError, varSupply)
import qualified Types.Type as T

check :: HasCallStack => String -> Either TypeError Text -> Expectation
check s expected = do
  let ex = fromRight' $ Parser.parseExpr s
  let ty_res :: Either TypeError T.Scheme
      ty_res = Infer.inferExpr ex & (`evalSupplyT` varSupply)
  let actual = P.pretty <$> ty_res <&> P.layoutPretty P.defaultLayoutOptions <&> P.Render.Text.renderStrict
  actual `shouldBe` expected

test :: HasCallStack => (String, String, Either TypeError Text) -> SpecWith (Arg Expectation)
test (name, s, expected) = it name (check s expected)

pattern Ok :: b -> Either a b
pattern Ok x = Right x

pattern Err :: a -> Either a b
pattern Err x = Left x

spec :: Spec
spec = parallel $ do
  let
    t :: HasCallStack => _
    t = test
  t ("id", "let id = \\x -> x in id", Ok idTy)
  t ("id'", "let x = \\y -> y in x", Ok idTy)
  t ("id'", "let id' = \\x -> let y = x in y in id'", Ok idTy)
  t ("apply", "let apply = \\f x -> f x in apply", Ok "forall a b. (a -> b) -> a -> b")
  t ("apply twice", "\\f x -> f x x", Ok "forall a b. (a -> a -> b) -> a -> b")
  t ("apply twice other", "\\x -> \\y -> let z = x y in z y", Ok "forall a b. (a -> a -> b) -> a -> b")
  t ("compose", "\\f g x -> f (g x)", Ok "forall a b c. (b -> c) -> (a -> b) -> a -> c")
  where
    idTy = "forall a. a -> a"

-- it "should work" $ do
--   check "1234" "Int"
-- test ("", "1234", "a -> a")

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

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
import Types.Infer.Monad (TypeError (InfiniteType), varSupply)
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

-- TODO: need to figure out why must alpha rename
spec :: Spec
spec = parallel $ do
  let t :: HasCallStack => _
      t = test
  t ("id", "let id = \\x -> x in id", Ok idTy)
  t ("id'", "let x = \\y -> y in x", Ok idTy)
  t ("id'", "let id' = \\x -> let y = x in y in id'", Ok idTy)
  t ("apply", "let apply = \\f x -> f x in apply", Ok "forall a b. (a -> b) -> a -> b")
  t ("apply twice", "\\f x -> f x x", Ok "forall a b. (a -> a -> b) -> a -> b")
  t ("apply twice other", "\\x -> \\y -> let z = x y in z y", Ok "forall a b. (a -> a -> b) -> a -> b")
  t ("apply twice other not alpha renamed", "\\x -> \\y -> let x = x y in x y", Ok "forall a b. (a -> a -> b) -> a -> b")
  t ("compose", "\\f g x -> f (g x)", Ok "forall a b c. (b -> c) -> (a -> b) -> a -> c")
  t ("apply with let inside", "\\x -> let y = \\z -> x(z) in y", Ok "forall a b. (a -> b) -> a -> b")
  t ("const with let inside", "\\x -> let y = \\z -> x in y", Ok "forall a b. a -> b -> a")
  t ("complicated without alpha renaming", "\\x -> \\y -> let x = x(y) in \\x -> y(x)", Ok "forall a b c. ((a -> b) -> c) -> (a -> b) -> a -> b")
  -- TODO: need to fix the vars not correct
  t ("complicated with alpha renaming", "\\x -> \\y -> let z = x(y) in \\w -> y(z)", Ok "forall a b c. ((a -> b) -> c) -> (a -> b) -> a -> b")
  t ("recursive types", "\\x -> let y = x in y(y)", Err $ InfiniteType (T.VarId 4) (T.Var (T.VarId 4) T.:-> T.Var (T.VarId 5)))
  t ("simple if", "\\z x y -> let u = if z x then x else y in u", Ok "forall a. (a -> Bool) -> a -> a -> a")
  t ("recursive", "let y = x in let x = y in x", Ok "")
  t ("recursive to itself", "let x = x in x", Ok "forall a. a")
  t
    ( "fibonacci",
      "let rec fib = \\x -> if x == 0 then 0 else if x == 1 then 1 else fib (x - 1) + fib (x - 2) in fib",
      Ok "Int -> Int"
    )
  describe "ski combinators" $ do
    t ("s", "\\x y z -> (x z)(y z)", Ok "forall a b c. (a -> b -> c) -> (a -> b) -> a -> c")
    t ("k", "\\x y -> x", Ok "forall a b. a -> b -> a")
    t ("i", "\\x -> x", Ok idTy)
  -- t ("y combinator", "let rec y = \\f -> (\\x -> y f (x x)) (\\x -> y f (x x)) in y", Ok "")
  -- t ("y combinator no let", "\\f -> (\\x -> f (x x)) (\\x -> f (x x))", Ok "")
  where
    idTy = "forall a. a -> a"

-- it "should work" $ do
--   check "1234" "Int"
-- test ("", "1234", "a -> a")

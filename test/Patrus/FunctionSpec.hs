module Patrus.FunctionSpec (spec) where

import Patrus.Types
import Patrus

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "Function handling" $ do
        it "successfully computes the fibonnaci sequence" $ do
            r <- runProgram (parseProgram "fun fib(n){ if(n<=1) return n;  else  return fib(n-2) + fib(n-1); } return fib(20);")
            fst r `shouldBe` Lit (NumberLit 6765.0)

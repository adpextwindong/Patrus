{-# LANGUAGE QuasiQuotes #-}
module Patrus.BindingSpec (spec) where

import Patrus.Types
import Patrus

import Test.Hspec
import Test.QuickCheck
import Data.String.QQ
import Data.Either
import System.IO.Silently (capture)

spec :: Spec
spec = do
    describe "Binding" $ do
        it "GlobalLocal" $ do
            let prog = parseProgram [s|
            var a = "global";

            {
              fun scopeA(){
                print a;
                return;
              }

              scopeA();
              var a = "local";
              scopeA();
            }|]

            let expected = "global\nglobal\n"
            (captured, result) <- capture $ runProgram prog

            captured `shouldBe` expected

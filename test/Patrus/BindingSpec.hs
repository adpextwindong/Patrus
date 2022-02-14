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
              }

              scopeA();
              var a = "local";
              scopeA();
            }|]

            let expected = "global\nglobal\n"
            (captured, result) <- capture $ runProgram prog

            captured `shouldBe` expected
        it "recursion" $ do
            let prog = parseProgram [s|

            //not globals
            {
              //function parameters can locally overwrite function name bindings
              fun foo(foo){
                print foo;
              }

              foo(5);
              foo(foo);
            }

            |]

            let expected = "5\n<fn foo>\n"
            (captured, result) <- capture $ runProgram prog

            captured `shouldBe` expected

{-# LANGUAGE QuasiQuotes #-}
module Patrus.BindingSpec (spec) where

import Patrus.Types
import Patrus.EvalK
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
        it "globalMutation" $ do
            let prog = parseProgram [s|
              var x = 0;
              print x;

              {
                {
                  //local x to 1
                  var x = 1;
                  print x;
                }

                //mutate global x to 2
                x = 2;
                print x;

              }
              print x;

            |]

            let expected = "0\n1\n2\n2\n"
            (captured, result) <- capture $ runProgram prog
            captured `shouldBe` expected
        it "globalMutationK" $ do
            let prog = parseProgram [s|
              var x = 0;
              print x;

              {
                {
                  //local x to 1
                  var x = 1;
                  print x;
                }

                //mutate global x to 2
                x = 2;
                print x;

              }
              print x;

            |]

            let expected = "0\n1\n2\n2\n"
            (captured, result) <- capture $ interpretK prog return emptyEnvironment
            captured `shouldBe` expected

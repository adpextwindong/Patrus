module Patrus.ParserSpec (spec) where

import Patrus.Types
import Patrus.Parser

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "Parsing functions" $ do
        it "successfully parses empty function call" $ do
            parseProgram "foo();" `shouldBe` [ExprStatement (Call (Var "foo") [])]
        it "successfully parses function call with only one argument" $ do
            parseProgram "foo(3);" `shouldBe` [ExprStatement (Call (Var "foo") [Lit (NumberLit 3.0)])]
        it "successfully parses function call with only one argument" $ do
            parseProgram "foo(3,4);" `shouldBe` [ExprStatement (Call (Var "foo") [Lit (NumberLit 3.0), Lit (NumberLit 4.0)])]
        it "successfully parses curried functions" $ do
            parseProgram "foo(1,2,3)(true)(nil);" `shouldBe` [ExprStatement (Call
                                                                                (Call
                                                                                    (Call (Var "foo")
                                                                                        [Lit (NumberLit 1.0),Lit (NumberLit 2.0),Lit (NumberLit 3.0)])

                                                                                    [Lit (BoolLit True)])

                                                                                [Lit Nil])]
        it "successfully parses function declaration" $ do
            parseProgram "fun add(x,y) { x + y; }" `shouldBe` [FunStatement "add" ["x","y"] (BlockStatement [ExprStatement (BOp Plus (Var "x") (Var "y"))])] --TODO switch to return statement after 10.5

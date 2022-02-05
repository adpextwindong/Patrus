module Patrus.EvaluationSpec where

import Patrus.Types
import Patrus.Parser

import Test.Hspec
import Test.QuickCheck

import Patrus.EvalK
import Data.IORef
import Control.Applicative (Alternative(empty))
import qualified Data.Map as M

totalEmptyEnvironment = Environment EmptyEnv EmptyEnv Nothing

spec :: Spec
spec = do
  describe "Testing the EvalK Evaluation" $ do
    it "Unary negation converts number to bool" $ do
     let prog = parseProgram "var r = !(5.0);"
     (Environment _ resultGlobalEnv _) <- interpretK prog return totalEmptyEnvironment
     resultGlobalEnv `envShouldBe` (Lit (BoolLit False))

--Checks the "r" variable for the result.
--This is a kludge around evalK only returning an env
--Expects the test result to be a flat env with no globals and no continuation
resulte `envShouldBe` expectede = resulte `shouldBe` Env (M.fromList [("r",expectede)]) EmptyEnv

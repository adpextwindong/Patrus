module Patrus.EvaluationSpec where

import Patrus.Types
import Patrus.Parser

import Test.Hspec
import Test.QuickCheck

import Patrus.EvalK
import Data.IORef
import Control.Applicative (Alternative(empty))

-- clown shit, does not work. Just test directly against the environment
-- spec :: Spec
-- spec = do
--     describe "Testing the continuation" $ do
--         it "successfully sets the IORef so we can test evalK" $ do
--             tref <- newIORef $ Lit Nil
--             _ <- pure $! evalK (Lit (BoolLit False)) (\e env -> do 
--                 writeIORef tref e >> return emptyEnvironment) emptyEnvironment 
--             result <- readIORef tref
--             result `shouldBe` Lit (BoolLit False)
module Patrus
    ( module Patrus.Types
     ,module Patrus.Lexer
     ,module Patrus.Parser
     ,module Patrus.Env
     ,module Patrus.Interpret
     ,module Patrus.Eval
     ,ip
     ,runPF
     ) where

import Patrus.Types
import Patrus.Lexer
import Patrus.Parser
import Patrus.Env
import Patrus.Interpret
import Patrus.Eval

ip = interpretProgram . parseProgram

runPF fp = ip =<< readFile fp

module Patrus
    ( module Patrus.Types
     ,module Patrus.Lexer
     ,module Patrus.Parser
     ,module Patrus.Environment
     ,module Patrus.Interpret
     ,module Patrus.Eval
     ,ip
     ) where

import Patrus.Types
import Patrus.Lexer
import Patrus.Parser
import Patrus.Environment
import Patrus.Interpret
import Patrus.Eval

ip = interpretProgram . parseProgram

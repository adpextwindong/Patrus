{

module Patrus.Lexer where

}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+ ;

{

data Token = Token

}
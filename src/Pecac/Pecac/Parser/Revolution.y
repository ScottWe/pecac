-- Monad template from: https://github.com/dagit/happy-plus-alex/
{
module Pecac.Parser.Revolution (parseRevolution) where

import Pecac.Parser.Lexer
import Pecac.Parser.Syntax
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { happyError }

%token
    nat             { Token _ (TokenNat $$) }
    pi              { Token _ (TokenPi _) }
    '-'             { Token _ TokenMinus }
    '*'             { Token _ TokenStar }
    '/'             { Token _ TokenSlash }
    '%'             { Token _ TokenPercent }

%%

Revolution : Ratio                                { $1 }
           | Ratio '*' pi                         { rescaleRev $1 }

Ratio : AbsRatio                                  { $1 }
      | '-' AbsRatio                              { negateRev $2 }

AbsRatio : Nat Div Nat                            { ($1, $3) }
         | Nat                                    { ($1, 1) }

Div : '/'                                         {}
    | '%'                                         {}

Nat : nat                                         { (read $1 :: Integer) }

{
rescaleRev :: (Integer, Integer) -> (Integer, Integer)
rescaleRev (x, y) = (x, y * 2)

negateRev :: (Integer, Integer) -> (Integer, Integer)
negateRev (x, y) = (-x, y)

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexQasmMonadScan >>=)

happyError :: Token -> Alex a
happyError (Token p t) = alexQasmError p "error"

parseRevolution :: String -> Maybe (Integer, Integer)
parseRevolution input =
    case runAlexQasm parse "" input of
        Left  _   -> Nothing
        Right rev -> Just rev
}

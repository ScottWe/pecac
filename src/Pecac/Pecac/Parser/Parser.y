-- Monad template from: https://github.com/dagit/happy-plus-alex/
{
module Pecac.Parser.Parser (parseQasm) where

import Pecac.Parser.Lexer
import Pecac.Parser.Syntax
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { happyError }

%token
    openqasm        { Token _ TokenOpenQasm }
    version         { Token _ (TokenVer $$) }
    include         { Token _ TokenInclude }
    path            { Token _ (TokenPath $$) }
    ctrl            { Token _ TokenCtrl }
    negctrl         { Token _ TokenNegCtrl }
    inv             { Token _ TokenInv }
    qreg            { Token _ TokenQReg }
    qubit           { Token _ TokenQubit }
    angle           { Token _ TokenAngle }
    input           { Token _ TokenInput }
    nat             { Token _ (TokenNat $$) }
    id              { Token _ (TokenID $$) }
    '@'             { Token _ TokenAt }
    '+'             { Token _ TokenPlus }
    '-'             { Token _ TokenMinus }
    '*'             { Token _ TokenStar }
    '('             { Token _ TokenLParen }
    ')'             { Token _ TokenRParen }
    '['             { Token _ TokenLBrack }
    ']'             { Token _ TokenRBrack }
    ','             { Token _ TokenComma }
    ';'             { Token _ TokenSemicolon }

%left '+' '-'
%left '*' '/'
%left NEG
%%

------------------------
-- | General File Format

Program : Version Includes StmtList               { QASMFile $1 $2 $3 }

Version : {- empty -}                             { "3" }
        | openqasm version ';'                    { $2 }

Includes : {- empty -}                            { [] }
         | include Path ';' Includes              { $2 : $4 }

Path : path                                       { filter (/='\"')  $1 }

StmtList : {- empty -}                            { [] }
         | Stmt StmtList                          { $1 : $2 }

Stmt : Gate ';'                                   { GateStmt $1 }
     | ParamDecl ';'                              { ParamDeclStmt $1 }
     | QubitDecl ';'                              { QubitDeclStmt $1 }

-----------------------
-- | Declaration Format

Designator : '[' nat ']'                          { (read $2 :: Int) }

ParamDecl : input angle id                        { ParamVarDecl $3 }
          | input angle Designator id             { ParamArrDecl $4 $3 }

QubitDecl : qubit id                              { QubitVarDecl $2 }
          | qubit Designator id                   { QubitArrDecl $3 $2}
          | qreg id                               { QubitVarDecl $2 }
          | qreg id Designator                    { QubitArrDecl $2 $3 }

----------------
-- | Gate Format

Gate : id GateOperands                            { Gate (PlainGate $1 $2) }
     | id '(' Expr ')' GateOperands               { Gate (RotGate $1 $3 $5) }
     | ctrl '@' Gate                              { CtrlMod $3 }
     | negctrl '@' Gate                           { NegCtrlMod $3 }
     | inv '@' Gate                               { InvMod $3 }

GateOperands : GateOperand                        { [$1] }
             | GateOperand ',' GateOperands       { $1 : $3 }

GateOperand : id                                  { QVar $1 }
            | id Designator                       { QReg $1 $2 }

Expr : Expr '+' Expr                              { Plus $1 $3 }
     | Expr '-' Expr                              { Minus $1 $3 }
     | Expr '*' Expr                              { Times $1 $3 }
     | '(' Expr ')'                               { Brack $2 }
     | '-' Expr %prec NEG                         { Negate $2 }
     | id                                         { VarId $1 }
     | id Designator                              { CellId $1 $2 }
     | Nat                                        { ConstNat $1 }

Nat : nat                                         { (read $1 :: Int) }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexQasmMonadScan >>=)

happyError :: Token -> Alex a
happyError (Token p t) = alexQasmError p msg
    where msg = "parse error at token '" ++ unlex t ++ "' of type " ++ classify t

parseQasm :: FilePath -> String -> Either String QASMFile
parseQasm fp input = runAlexQasm parse fp input
}

-- | Functions to print statements.

module Pecac.Printer.Stmt
  ( printParamDecl
  , printQubitDecl
  , printStmt
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Pecac.Parser.Syntax
  ( ParamDecl (..)
  , QubitDecl (..)
  , Stmt (..)
  )
import Pecac.Printer.Gate (printGate)
import Pecac.Printer.General (printCell)

-----------------------------------------------------------------------------------------
-- * Declaration Printing.

-- | Pretty prints scalar variables, given the type and identifier.
printScalar :: String -> String -> String
printScalar ty id = ty ++ " " ++ id

-- | Converts a qubit declaration to an OpenQASM 3 prettified string representation.
printQubitDecl :: QubitDecl -> String
printQubitDecl (QubitVarDecl id)    = printScalar "qubit" id
printQubitDecl (QubitArrDecl id sz) = printCell "qubit" sz ++ " " ++ id

-- | Converts an angle declaration to a prettified string representation.
printParamDecl :: ParamDecl -> String
printParamDecl (ParamVarDecl id)    = "input " ++ printScalar "angle" id
printParamDecl (ParamArrDecl id sz) = "input array[angle, " ++ show sz ++ "] " ++ id

-----------------------------------------------------------------------------------------
-- * Statement Printing.

-- | Helper function to pretty print a statement, given a stringified representation of
-- the statement's body.
formatStmt :: String -> String
formatStmt body = body ++ ";"

-- | Converts an OpenQASM statement to a prettified string representation.
printStmt :: Stmt -> String
printStmt (GateStmt gate)      = formatStmt $ printGate gate
printStmt (ParamDeclStmt decl) = formatStmt $ printParamDecl decl
printStmt (QubitDeclStmt decl) = formatStmt $ printQubitDecl decl

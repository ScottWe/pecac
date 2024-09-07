-- | Utility functions to log errors to the command-line.

module PecacExe.ErrorLogging
  ( logCircErr
  , logExprErr
  , logGateErr
  , logOperandErr
  , logStmtErr
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.List (intercalate)
import Pecac.List (prettyNonEmpty)
import Pecac.Parser.Gate
  ( ExprErr (..)
  , OperandErr (..)
  , GateErr (..)
  )
import Pecac.Parser.Problem
  ( CircErr (..)
  , StmtErr (..)
  )
import Pecac.Printer.Expr (printExpr)
import Pecac.Printer.Gate (printGate)
import Pecac.Printer.General (printCell)
import Pecac.Printer.Stmt
  ( printParamDecl
  , printQubitDecl
  )

import qualified Data.List.NonEmpty as NonEmpty

-----------------------------------------------------------------------------------------
-- * Logging Functions.

-- | Function to report an out-of-bounds access error, given the requested index (idx)
-- and the size of the array (sz).
logOOBErr :: Int -> Int -> String
logOOBErr idx sz = msg
    where msg = "OOB index " ++ show idx ++ " in array of length " ++ show sz ++ "."

-- | Function to index lines when resolving nested errors.
indent :: [String] -> [String]
indent = map ("  " ++)

-- | Function to convert an expression error to a display message. Each string in the
-- error corresponds to one line of output.
logExprErr :: ExprErr -> [String]
logExprErr (IntVarUse id) = [msg]
    where msg = "Integer variables are not supported: " ++ id ++ "."
logExprErr (IntArrUse id idx) = [msg]
    where msg = "Integer arrays are not supported: " ++ printCell id idx ++ "."
logExprErr (UnknownParam name) = [msg]
    where msg = "Unknown parameter variable: " ++ name ++ "."
logExprErr (ParamOOB idx sz) = [msg]
    where msg = logOOBErr idx sz
logExprErr (UnexpectedNat n) = [msg]
    where msg = "Unexpected integer literal " ++ show n ++ "."
logExprErr (UnknownTimesLHS expr) = [msg]
    where msg = "Unable to determine type of left-hand side: " ++ printExpr expr ++ "."
logExprErr (AngleAsInt str) = [msg]
    where msg = "Unexpected use of " ++ str ++ " as a scalar."

-- | Function to convert an operand error to a display message. Each string in the error
-- corresponds to one line of output.
logOperandErr :: OperandErr -> [String]
logOperandErr (TooManyOperands n) = [msg]
    where msg = "Operand list has " ++ show n ++ " more elemenets than expected."
logOperandErr (TooFewOperands n) = [msg]
    where msg = "Operand list has " ++ show n ++ " fewer elemenets than expected."
logOperandErr NonArrOperand = [msg]
    where msg = "Operand must be an array access."
logOperandErr (QubitOOB idx sz) = [msg]
    where msg = logOOBErr idx sz
logOperandErr (UnknownQubitReg name) = [msg]
    where msg = "Unknown qubit register: " ++ name ++ "."
logOperandErr (NoCloningViolation idx) = [msg]
    where msg = "Multiple uses of index " ++ show idx ++ " violates no-cloning."

-- | Function to convert an gate error to a display message. Each string in the error
-- corresponds to one line of output.
logGateErr :: GateErr -> [String]
logGateErr (GateOperandErr err) = msg : res
    where msg = "Invalid gate operand. Reason:"
          res = indent $ logOperandErr err
logGateErr (GateAngleErr err) = msg :res
    where msg = "Invalid gate angle. Reason:"
          res = indent $ logExprErr err
logGateErr (UnknownPlainName name) = [msg]
    where msg = "Unknown parameter-free gate name: " ++ name ++ "."
logGateErr (UnknownRotName name) = [msg]
    where msg = "Unknown parameterized gate name: " ++ name ++ "."

-- | Function to convert a statement error to a display message. Each string in the error
-- corresponds to one line of output.
logStmtErr :: StmtErr -> [String]
logStmtErr (DuplicateName name) = [msg]
    where msg = "Parameter and qubit arrays have the same variable name: " ++ name ++ "."
logStmtErr MissingParams = [msg]
    where msg = "Parameter array is not declared at start of file."
logStmtErr MissingQubits = [msg]
    where msg = "Qubit register is not declared at start of file, after parameter array."
logStmtErr (UnexpectParamDecl decl) = [msg]
    where msg = "Expected a single parameter declaration, found: " ++ printParamDecl decl
logStmtErr (UnexpectQubitDecl decl) = [msg]
    where msg = "Expected a singlq qubit declaration, found: " ++ printQubitDecl decl
logStmtErr (InvalidGate gate err) = msg : res
    where msg = "Invalid gate: " ++ printGate gate ++ ". Reason:"
          res = indent $ logGateErr err
logStmtErr NonArrParamDecl = [msg]
    where msg = "Parameter declarations must be of type array."
logStmtErr NonArrQubitDecl = [msg]
    where msg = "Qubit declarations must be of type array."

-- | Function to convert a circuit error to a display message. Each string in the error
-- corresponds to one line of output.
logCircErr :: CircErr -> [String]
logCircErr (UnsupportedVersion ver) = [msg]
    where msg = "OpenQASM version " ++ ver ++ " is not supported."
logCircErr (MissingIncludes incls) = [msg]
    where msg = "Missing required include files: " ++ prettyNonEmpty show incls ++ "."
logCircErr (UnsupportedIncludes incls) = [msg]
    where msg = "Unsupported include files: " ++ prettyNonEmpty show incls ++ "."
logCircErr (InvalidStmt err) = logStmtErr err

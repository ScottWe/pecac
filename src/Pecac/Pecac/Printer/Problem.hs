-- | Functions to convert problem summaries to syntactic objects.

module Pecac.Printer.Problem
  ( encodeProblem
  , printFile
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Pecac.Analyzer.Gate (GateConfigs)
import Pecac.Analyzer.Problem
  ( ParamArr (..)
  , ParamCirc (..)
  , QubitReg (..)
  )
import Pecac.List (prettyIntercalate)
import Pecac.Parser.Syntax
  ( ParamDecl (ParamArrDecl)
  , QubitDecl (QubitArrDecl)
  , QASMFile (..)
  , Stmt (..)
  )
import Pecac.Printer.GateSummary (summaryToGate)
import Pecac.Printer.Stmt (printStmt)

-----------------------------------------------------------------------------------------
-- * Problem Conversion.

-- | Converts a parameter array to its corresponding syntactic statement.
declareParams :: ParamArr -> Stmt
declareParams (ParamArr name sz) = ParamDeclStmt $ ParamArrDecl name sz

-- | Converts a qubit register to its corresponding syntactic statement.
declareQubits :: QubitReg -> Stmt
declareQubits (QubitReg name sz) = QubitDeclStmt $ QubitArrDecl name sz

-- | Converts a parameterized circuit to its corresponding syntactic statement. The
-- version defaults to 3.0, and the library "stdgates.inc" is included.
encodeProblem :: ParamCirc -> QASMFile
encodeProblem (ParamCirc parr qreg gates) = QASMFile "3.0" ["stdgates.inc"] stmts
    where pstmt = declareParams parr
          qstmt = declareQubits qreg
          gstmt = map (GateStmt . summaryToGate parr qreg) gates
          stmts = pstmt : qstmt : gstmt

-----------------------------------------------------------------------------------------
-- * QASMFile Printing.

-- | Helper function to convert a library name to an include statement.
formatInclude :: String -> String
formatInclude incl = "include \"" ++ incl ++ "\";"

-- | Converts a QASMFile to a textual representation.
printFile :: QASMFile -> String
printFile (QASMFile vers incls stmts) =
    if itext == ""
    then vtext ++ "\n\n" ++ stext
    else vtext ++ "\n" ++ itext ++ "\n\n" ++ stext
    where vtext = "OPENQASM " ++ vers ++ ";"
          itext = prettyIntercalate "\n" formatInclude incls
          stext = prettyIntercalate "\n" printStmt stmts

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
  ( FDecl
  , ParamDecl (ParamArrDecl)
  , QubitDecl (QubitArrDecl)
  , QASMFile (..)
  , Stmt (..)
  )
import Pecac.Printer.FDecl (printFDecl)
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
encodeProblem (ParamCirc parr qreg gates) = QASMFile "3.0" ["stdgates.inc"] [] stmts
    where pstmt = declareParams parr
          qstmt = declareQubits qreg
          gstmt = map (GateStmt . summaryToGate parr qreg) gates
          stmts = pstmt : qstmt : gstmt

-----------------------------------------------------------------------------------------
-- * QASMFile Printing.

-- | Helper function to convert a library name to an include statement.
formatInclude :: String -> String
formatInclude incl = "include \"" ++ incl ++ "\";"

-- | Helper method to print each include statement, with proper spacing.
formatIncludeLine :: [String] -> String
formatIncludeLine []    = ""
formatIncludeLine incls = "\n" ++ itext
    where itext = prettyIntercalate "\n" formatInclude incls

-- | Helper method to print each include function declaration, with proper spacing. This
-- tring will include newlines separating the function definitions from the previous
-- block in the file.
formatDeclLines :: [FDecl] -> String
formatDeclLines []    = ""
formatDeclLines decls = "\n\n" ++ ftext
    where ftext = prettyIntercalate "\n\n" printFDecl decls

-- | Helper method to print the statements in the main procedure of the program, with
-- proper spacing. This string will include newlines separating the function definitions
-- from the previous block in the file.
formatMain :: [Stmt] -> String
formatMain []   = ""
formatMain main = "\n\n" ++ mtext
    where mtext = prettyIntercalate "\n" printStmt main

-- | Converts a QASMFile to a textual representation.
printFile :: QASMFile -> String
printFile (QASMFile vers incls decls main) = vtext ++ itext ++ ftext ++ mtext
    where vtext = "OPENQASM " ++ vers ++ ";"
          itext = formatIncludeLine incls
          ftext = formatDeclLines decls
          mtext = formatMain main

-- | Command-line interface to interact with the PECEC parser.

module Main where

-----------------------------------------------------------------------------------------
-- * Import Section.

import CircViewer.CmdLn
  ( ViewTool(..)
  , getToolArgs
  )
import Control.Monad (foldM)
import PecacExe.ErrorLogging (logCircErr)
import PecacExe.IOUtils (readSrc)
import Pecac.List (prettyList)
import Pecac.Parser.Parser (parseQasm)
import Pecac.Parser.Problem (qasmToParamCirc)
import Pecac.Parser.Syntax (QASMFile)
import Pecac.Analyzer.Gate
  ( GateConfigs (..)
  , GateSummary (..)
  , Polarity (..)
  , plainNameToString
  , rotNameToString
  )
import Pecac.Analyzer.Problem
  ( ParamArr (..)
  , ParamCirc (..)
  , QubitReg (..)
  )

-----------------------------------------------------------------------------------------
-- * Entry Point.

-- | Adds a C(-) to the base name, for each control.
formatName :: String -> [Polarity] -> String
formatName name []        = name
formatName name (_:ctrls) = "C(" ++ formatName name ctrls ++ ")"

-- | Converts a control qubit to its string representation, and then adds either a "+" or
-- "-" prefix to indicate the polarity of the control.
formatQubit :: Int -> Polarity -> String
formatQubit x Pos = "+" ++ show x
formatQubit x Neg = "-" ++ show x

-- | Converts a list of qubits to their string representations, such that each control
-- qubit also has a "+" or "-" prefix to indicate control polarity.
formatQubits :: [Int] -> [Polarity] -> [String]
formatQubits []     []     = []
formatQubits (x:xs) []     = show x          : formatQubits xs []
formatQubits (x:xs) (c:cs) = formatQubit x c : formatQubits xs cs

-- | Unifies PlainSummary and RotSummary gate formatting, by taking the type of the gate
-- as a string, and by taking the list of parameter coefficients as an optional list.
formatGateImpl :: String -> Maybe [Int] -> GateConfigs -> String
formatGateImpl name Nothing (GateConfigs inv ctrls ops) = full
    where cname = formatName name ctrls
          opstr = prettyList id $ formatQubits ops ctrls
          full  = cname ++ " at " ++ opstr
formatGateImpl name (Just coeffs) confs = full
    where base = formatGateImpl name Nothing confs
          cstr = prettyList show coeffs
          full = base ++ " with parameters " ++ cstr

-- | Helper function to convert a gate summary to a concise string representation.
formatGate :: GateSummary -> String
formatGate (PlainSummary ty conf) = formatGateImpl name Nothing conf
    where name = plainNameToString ty
formatGate (RotSummary ty coeffs conf) = formatGateImpl name (Just coeffs) conf
    where name = rotNameToString ty

-- | Helper function to display a gate summary to stdout according to formatGate.
printGate :: GateSummary -> IO ()
printGate = putStrLn . formatGate

-- | Helper function to display a list of gates to stdout according to formatGate, such
-- that each gate appears on its own line.
printGates :: [GateSummary] -> IO ()
printGates = foldM (\() gate -> putStrLn $ formatGate gate) ()

-- | Helper function to display a declaration summary to stdout. Excepts the type of the
-- declaration (e.g., "Parameter" or "Qubit"), along with the declaration name and size.
printDecl :: String -> String -> Int -> IO ()
printDecl ty var sz = putStrLn msg
    where msg = ty ++ " Variable: " ++ var ++ " (length " ++ show sz ++ ")"

-- | Helper function to print a parameterized circuit to stdout. In particular, the qubit
-- and parameter declarations are printed according to printDecl, whereas the gates are
-- printed according to printGates.
printCirc :: ParamCirc -> IO ()
printCirc (ParamCirc (ParamArr pvar psz) (QubitReg qvar qsz) gates) = do
    putStrLn $ "[CIRCUIT SUMMARY]"
    printDecl "Parameter" pvar psz
    printDecl "Qubit"     qvar qsz
    putStrLn ""
    putStrLn "[GATE SUMMARY LIST]"
    printGates gates

-- | Helper function to convert an OpenQASM file to a parameterized circuit, and then
-- log the circuit to stdout. If the program is invalid, then an error is logged to
-- stdout instead.
processProg :: QASMFile -> IO ()
processProg prog =
    case qasmToParamCirc prog of
        Left  err  -> putStrLn $ unlines $ logCircErr err
        Right circ -> printCirc circ

processArgs :: ViewTool -> IO ()
processArgs (CircMode src) = do
    contents <- readSrc src
    case parseQasm src contents of
        Left err   -> putStrLn err
        Right prog -> processProg prog

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args

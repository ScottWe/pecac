-- | Command-line interface to interact with the PECEC parser.

module Main where

-----------------------------------------------------------------------------------------
-- * Import Section.

import CutoffViewer.CmdLn
  ( ViewTool (..)
  , getToolArgs
  )
import Pecac.Analyzer.Cutoffs
  ( CutoffResult (..)
  , circToKappa
  , circToLambda
  , forallElimSize
  , randomSampleSize
  )
import Pecac.Analyzer.Problem (ParamCirc)
import PecacExe.IOUtils
  ( logShowableVect
  , readCirc
  )

-----------------------------------------------------------------------------------------
-- * Summarization Routines.

-- | Helper function to print the cutoff bounds (e.g, forallElimSize value) for a pair of
-- circuits to stdout. If the circuits have different numbers of arguments, then an error
-- is logged instead.
computeCutoffs :: ParamCirc -> ParamCirc -> IO ()
computeCutoffs circ1 circ2 = do
    putStrLn "[CIRCUIT COMPARISON]"
    case randomSampleSize circ1 circ2 of
        ParamMismatch -> putStrLn "Circuits have different numbers of parameters."
        RationalCoeff -> putStrLn "Rational coefficients encountered."
        Result prob   -> case forallElimSize circ1 circ2 of
            Result elim -> do
                putStrLn $ "d-value: " ++ show prob
                logShowableVect "elim" elim
            _ -> putStrLn "Unexpected failure."

-- | Helper function to print the parameter summaries for a circuit (e.g., the lambda
-- vector and kappa value). To differentiate between pairs of circuits, a string argument
-- is also taken, and used to formal the header: [<name> SUMMARY]
computeSummary :: String -> ParamCirc -> IO ()
computeSummary name circ =
    case circToKappa circ of
        Just kappa -> case circToLambda circ of
            Just lambda -> do
                putStrLn $ "[" ++ name ++ " SUMMARY]"
                putStrLn $ "Kappa Value: " ++ show kappa
                logShowableVect "lambda" lambda
            Nothing -> putStrLn "Failed to compute lambda vector."
        Nothing -> putStrLn "Failed to compute kappa value."

-----------------------------------------------------------------------------------------
-- * Entry Point.

-- | Unwraps the src files, and parses their contents as circuits. The circuits are then
-- passed along to the relevant summarization routines.
processArgs :: ViewTool -> IO ()
processArgs (Inspect src) =
    readCirc src $ \circ -> computeSummary "CIRCUIT" circ
processArgs (Compare src1 src2) =
    readCirc src1 $ \circ1 ->
        readCirc src2 $ \circ2 -> do
            computeSummary "CIRCUIT 1" circ1
            putStrLn ""
            computeSummary "CIRCUIT 2" circ2
            putStrLn ""
            computeCutoffs circ1 circ2

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args

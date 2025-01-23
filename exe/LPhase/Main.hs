-- | Command-line interface to eliminate global linear phase.

module Main where

-----------------------------------------------------------------------------------------
-- * Import Section.

import LPhase.CmdLn
  ( LPhaseTool (..)
  , getToolArgs
  )
import Pecac.Affine (linear)
import Pecac.Analyzer.Problem
  ( ParamCirc
  , addGlobalPhase
  )
import Pecac.Printer.Problem
  ( encodeProblem
  , printFile
  )
import Pecac.Verifier.CycloCircuit
  ( GPhaseResult (..)
  , findLinearPhase
  )
import PecacExe.IOUtils
  ( logShowableVect
  , readCirc
  )

-----------------------------------------------------------------------------------------
-- * Error Handling and Callbacks.

-- | Attmepts to find the coefficients for a candiate global affine linear phase. If the
-- search is successful, then a callback function is invoked which takes the coefficients
-- as parameters. Otherwise, an error is logged to the console.
tryFindLinearPhase :: ([Rational] -> IO ()) -> ParamCirc -> ParamCirc -> IO ()
tryFindLinearPhase f src otr =
    case findLinearPhase src otr of
        CutoffFailure       -> putStrLn "Could not compute cutoff bounds."
        InferenceFailure    -> putStrLn "Could not find a candidate affine global phase."
        LinearCoeffs coeffs -> f coeffs

-----------------------------------------------------------------------------------------
-- * Phase Encoding.

-- | The callback function for doEncode. Recall that tryFindLinearPhase consumes a
-- callback function which takes the rational coefficients as arguments. This callback
-- first takes the src circuit as an argument, so that it can be curried before passing
-- the callback to tryFindLinearPhase.
doEncodeCb :: ParamCirc -> [Rational] -> IO ()
doEncodeCb circ coeffs = putStrLn $ printFile $ encodeProblem pcirc
    where pcirc = addGlobalPhase (linear coeffs) circ

-- | Attempts to find the coefficients for a candiate global affine linear phase. If a
-- candidate is found, then a variant of src is logged to the console in which the global
-- affine linear phase is accounted for by a global phase gate. Otherwise, an error is
-- reported to the console.
doEncode :: ParamCirc -> ParamCirc -> IO ()
doEncode src = tryFindLinearPhase (doEncodeCb src) src

-----------------------------------------------------------------------------------------
-- * Phase Inference.

-- | Attempts to find the coefficients for a candiate global affine linear phase. If a
-- candidate is found, then the results are logged to the console. Otherwise, an error is
-- reported to the console.
doInfer :: ParamCirc -> ParamCirc -> IO ()
doInfer = do
    putStrLn "Found candidate global phase alpha."
    tryFindLinearPhase $ logShowableVect "alpha"

-----------------------------------------------------------------------------------------
-- * Entry Point.

-- | Unwraps the src files, and parses their contents as circuits. The circuits are then
-- passed along to the phase estimation procedure.
processArgs :: LPhaseTool -> IO ()
processArgs (Infer src otr) =
    readCirc src $ \srcCirc ->
        readCirc otr $ doInfer srcCirc
processArgs (Encode src otr) =
    readCirc src $ \srcCirc ->
        readCirc otr $ doEncode srcCirc

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args

-- | Command-line interface to interact with the pecac cyclotomic instantiator.

module Main where

-----------------------------------------------------------------------------------------
-- * Import Section.

import CycloInst.CmdLn
  ( CycloInstTool(..)
  , getToolArgs
  )
import PecacExe.IOUtils
  ( readCirc
  , readRevs
  )
import Pecac.Analyzer.Problem (ParamCirc (..))
import Pecac.Analyzer.Revolution (Revolution)
import Pecac.Verifier.CycloCircuit (circToMat)

import Pecac.Verifier.Matrix as Matrix

-----------------------------------------------------------------------------------------
-- * Circuit Logging.

-- | Interprets a parameterized circuit as a cyclotomic matrix, when each rotation is
-- instantiated according to the list of angles.
evalCirc :: [Revolution] -> ParamCirc -> IO ()
evalCirc revs circ =
    case circToMat revs circ of
        Nothing  -> putStrLn err
        Just mat -> putStrLn $ Matrix.prettyShow mat
    where err = "Number of angles does not equal number of parameters in the circuit."

-- | Determines if two parameterized circuits are equal, when each rotation is
-- instantiated according to the list of angles.
compCircs :: [Revolution] -> ParamCirc -> ParamCirc -> IO ()
compCircs revs circ1 circ2 =
    case circToMat revs circ1 of
        Nothing   -> putStrLn $ err "left-hand side"
        Just mat1 -> case circToMat revs circ2 of
            Nothing   -> putStrLn $ err "right-hand side"
            Just mat2 -> putStrLn $ show $ mat1 == mat2
    where err x = "Number of angles does not equal number of parameters on " ++ x ++ "."

-----------------------------------------------------------------------------------------
-- * Entry Point.

-- | Unwraps the src file name, and parses it as a circuit. The circuit is then passed
-- along to a circuit logging routine.
processArgs :: CycloInstTool -> IO ()
processArgs (Evaluate src args) =
    readCirc src $ \circ ->
        readRevs args $ \revs ->
            evalCirc revs circ
processArgs (Compare lhs rhs args) =
    readCirc lhs $ \circ1 ->
        readCirc rhs $ \circ2 -> 
            readRevs args $ \revs ->
                compCircs revs circ1 circ2

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args

-- | Command-line interface to interact with the pecac cyclotomic instantiator.

module Main where

-----------------------------------------------------------------------------------------
-- * Import Section.

import CycloInst.CmdLn
  ( CycloInstTool(..)
  , getToolArgs
  )
import PecacExe.IOUtils (readCirc)
import Pecac.Analyzer.Problem (ParamCirc (..))
import Pecac.Verifier.CycloCircuit (circToMat)

import Pecac.Verifier.Matrix as Matrix

-----------------------------------------------------------------------------------------
-- * Circuit Logging.

-- | Interprets a parameterized circuit as a cyclotomic matrix, when each rotation is
-- instantiated according to the list of angles.
evalCirc :: [Rational] -> ParamCirc -> IO ()
evalCirc degs circ =
    case circToMat degs circ of
        Nothing  -> putStrLn err
        Just mat -> putStrLn $ Matrix.prettyShow mat
    where err = "Number of angles does not equal number of parameters in the circuit."

-- | Determines if two parameterized circuits are equal, when each rotation is
-- instantiated according to the list of angles.
compCircs :: [Rational] -> ParamCirc -> ParamCirc -> IO ()
compCircs degs circ1 circ2 =
    case circToMat degs circ1 of
        Nothing   -> putStrLn $ err "left-hand side"
        Just mat1 -> case circToMat degs circ2 of
            Nothing   -> putStrLn $ err "right-hand side"
            Just mat2 -> putStrLn $ show $ mat1 == mat2
    where err x = "Number of angles does not equal number of parameters on " ++ x ++ "."

-----------------------------------------------------------------------------------------
-- * Entry Point.

-- | Converts a rational multiple of pi (given as a string) from radians to degrees.
toDeg :: String -> Rational
toDeg str = rad * 180
    where adj = map (\c -> if c == '/' then '%' else c) str
          rad = read adj :: Rational

-- | Unwraps the src file name, and parses it as a circuit. The circuit is then passed
-- along to a circuit logging routine.
processArgs :: CycloInstTool -> IO ()
processArgs (Evaluate src rads) =
    readCirc src $ \circ -> evalCirc (map toDeg rads) circ
processArgs (Compare lhs rhs rads) =
    readCirc lhs $ \circ1 ->
        readCirc rhs $ \circ2 -> compCircs (map toDeg rads) circ1 circ2

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args

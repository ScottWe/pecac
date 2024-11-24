-- | Command-line interface to interact with the PECEC parser.

module Main where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.Ratio ((%))
import PPec.CmdLn
  ( PPecTool (..)
  , getToolArgs
  )
import PecacExe.CmdLnParser (whenLoud)
import Pecac.Analyzer.Problem (ParamCirc)
import Pecac.Analyzer.Revolution (Revolution)
import Pecac.Verifier.CycloGate
  ( Cyclotomic
  , CycMat
  )
import Pecac.Verifier.CycloCircuit
  ( circToMat
  , findGlobalPhase
  , phaseEquiv
  )
import Pecac.Verifier.PEC
  ( EquivFun
  , PECRes (..)
  , Side
  , ppec
  )
import PecacExe.ErrorLogging (sideToString)
import PecacExe.IOUtils
  ( logShowableVect
  , readCirc
  )
import System.Random.TF (TFGen)
import System.Random.TF.Init
  ( initTFGen
  , mkTFGen
  )

import Pecac.Verifier.Matrix as Matrix

-----------------------------------------------------------------------------------------
-- * Exact Parameterized Equivalence Checking.

-- | Logs an EvalFaiil result to standard out.
printEvalFailure :: Side -> [Revolution] -> IO ()
printEvalFailure side theta = do
    putStrLn $ "Failed to evaluate " ++ sideToString side ++ " side at theta."
    logShowableVect "theta" theta

-- | Logs an EqFail result to standard out.
printEqFailure :: [Revolution] -> IO ()
printEqFailure theta = do
    putStrLn "False."
    whenLoud $ do
        putStrLn "Circuits disagree on angle theta."
        logShowableVect "theta" theta

-- | Logs an EqSuccess result to standard out.
printEqSuccess :: [[Revolution]] -> IO ()
printEqSuccess pset = do
    putStrLn "True."
    whenLoud $ do
            putStrLn "Circuits agree on angle theta."
            logShowableVect "theta" $ map (\x -> x !! 0) pset

-- | Takes an input an equality function and two circuits. Performs parameterized
-- equivalence checking using the provided equality function. The results are printed to
-- standard out.
runExactPPec :: TFGen -> Rational -> EquivFun CycMat -> ParamCirc -> ParamCirc -> IO ()
runExactPPec rgen prob eq lhs rhs =
    case ppec rgen prob lhs rhs evalFn eq of
        (_, BadCutoff)           -> putStrLn "Failed to compute cutoff."
        (_, EvalFail side theta) -> printEvalFailure side theta
        (_, EqFail theta)        -> printEqFailure theta
        (_, EqSuccess pset)      -> printEqSuccess pset
    where evalFn x y = circToMat y x

-----------------------------------------------------------------------------------------
-- * Parameterized Equivalence Checking Upto Global Phase.

-- | Harness setup for equivalence checking upto global phase. This function takes as
-- input the two circuits, and then calls runExactPec with equivalence-upto-global-phase
-- as the equality function.
runPhasePPec :: TFGen -> Rational -> ParamCirc -> ParamCirc -> IO ()
runPhasePPec rgen prob lhs rhs =
    case findGlobalPhase lhs rhs of
        Just s  -> runExactPPec rgen prob (phaseEquiv s) lhs rhs
        Nothing -> do
            putStrLn "False."
            whenLoud $ putStrLn "Failed to find candidate phase using all zero angles."

-----------------------------------------------------------------------------------------
-- * Entry Point.

-- | Helper method to initialize random number generator.
seedGen :: Maybe Int -> IO TFGen
seedGen Nothing  = initTFGen
seedGen (Just n) = return $ mkTFGen n

-- | Unwraps the src files, and parses their contents as circuits. The circuits are then
-- passed along to the relevant verification.
processArgs :: PPecTool -> IO ()
processArgs (PPecTool prob src1 src2 gphase seed) = do
    rgen <- seedGen seed
    readCirc src1 $ \circ1 ->
        readCirc src2 $ \circ2 ->
            if gphase
            then runPhasePPec rgen prob circ1 circ2
            else runExactPPec rgen prob (==) circ1 circ2

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args

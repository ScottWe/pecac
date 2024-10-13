-- | Command-line interface to interact with the PECEC parser.

module Main where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Pec.CmdLn
  ( PecTool (..)
  , getToolArgs
  )
import PecacExe.CmdLnParser
  ( whenLoud
  , whenNormal
  )
import Pecac.List (getCombinations)
import Pecac.Analyzer.Problem (ParamCirc)
import Pecac.Analyzer.Revolution (Revolution)
import Pecac.Verifier.CycloGate
  ( Cyclotomic
  , CycMat
  )
import Pecac.Verifier.CycloCircuit
  ( circToMat
  , findGlobalPhase
  )
import Pecac.Verifier.PEC
  ( PECRes (..)
  , Side (..)
  , pec
  )
import PecacExe.IOUtils
  ( logShowableVect
  , readCirc
  )

import Pecac.Verifier.Matrix as Matrix

-----------------------------------------------------------------------------------------
-- * Formatting Utilities.

-- | Formats a specific angle obtained by expanding out a parameter set. The integer
-- input is used to distinguish different parameters (i.e., thetaj rather than theta).
printParamSetElem :: Int -> [Revolution] -> IO ()
printParamSetElem j = logShowableVect $ "theta" ++ show j

-- | Implementation details for printParamSet. This function takes as input a list of all
-- possible parameters. For each parameter, the component values are listed, with proper
-- formatting (i.e., a newline between each parameter). The integer parameter to this
-- function is used to assign a unique index to parameter (i.e., theta0, theta1, ...).
printParamSetImpl :: Int -> [[Revolution]] -> IO ()
printParamSetImpl _ []             = return ()
printParamSetImpl j [theta]        = printParamSetElem j theta
printParamSetImpl j (theta:thetas) = do
    printParamSetElem j theta
    putStrLn ""
    printParamSetImpl (j + 1) thetas

-- | A helper method to print a list of parameter combinations to standard out. In
-- particular, the input to this function should be set of sets obtained by applying
-- combinations to a pset.
printParamSet :: [[Revolution]] -> IO ()
printParamSet = printParamSetImpl 0

-----------------------------------------------------------------------------------------
-- * Exact Parameterized Equivalence Checking.

-- | Returns a textual representation of Side values.
sideToString :: Side -> String
sideToString LHS = "left-hand"
sideToString RHS = "right-hand"

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
    whenNormal $ putStrLn $ "Checked " ++ show len ++ " parameters."
    whenLoud $ do
            putStrLn "The following theta were checked during verification."
            putStrLn ""
            printParamSet combs
    where combs = getCombinations pset
          len   = length combs

-- | Takes an input an equality function and two circuits. Performs parameterized
-- equivalence checking using the provided equality function. The results are printed to
-- standard out.
runExactPec :: (CycMat -> CycMat -> Bool) -> ParamCirc -> ParamCirc -> IO ()
runExactPec eq lhs rhs =
    case pec lhs rhs circToMat eq of
        BadCutoff           -> putStrLn "Failed to compute cutoff."
        EvalFail side theta -> printEvalFailure side theta
        EqFail theta        -> printEqFailure theta
        EqSuccess pset      -> printEqSuccess pset

-----------------------------------------------------------------------------------------
-- * Parameterized Equivalence Checking Upto Global Phase.

-- | For each cyclotomic number s, returns a equivalence relation which states that x is
-- related to y whenever x and y differ by a global phase of s.
--
-- Note that this function actually relates x to y when s*x = y, which is not equivalent
-- to s*y = x. This means that (phaseEquiv s) is not truly an equivalence relations.
-- However, if x and y are equivalent upto global phase, then there exists a unique s
-- such that (phaseEquiv s) relates x to y. Moreever, if x and y are not equivalent upto
-- global phase, then no such s exists.
phaseEquiv :: Cyclotomic -> CycMat -> CycMat -> Bool
phaseEquiv s x y = Matrix.scale s x == y

-- | Harness setup for equivalence checking upto global phase. This function takes as
-- input the two circuits, and then calls runExactPec with equivalence-upto-global-phase
-- as the equality function.
runPhasePec :: ParamCirc -> ParamCirc -> IO ()
runPhasePec lhs rhs =
    case findGlobalPhase lhs rhs of
        Just s  -> runExactPec (phaseEquiv s) lhs rhs
        Nothing -> do
            putStrLn "False."
            whenLoud $ putStrLn "Failed to find candidate phase using all zero angles."

-----------------------------------------------------------------------------------------
-- * Entry Point.

-- | Unwraps the src files, and parses their contents as circuits. The circuits are then
-- passed along to the relevant verification.
processArgs :: PecTool -> IO ()
processArgs (PecTool src1 src2 gphase) =
    readCirc src1 $ \circ1 ->
        readCirc src2 $ \circ2 ->
            if gphase
            then runPhasePec circ1 circ2
            else runExactPec (==) circ1 circ2

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args

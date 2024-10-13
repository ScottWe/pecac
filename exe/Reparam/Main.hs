-- | Command-line interface to reparameterize a pair of circuits.

module Main where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.Ratio ((%))
import Reparam.CmdLn
  ( ReparamTool (..)
  , getToolArgs
  )
import Pecac.Maybe (maybeApply)
import Pecac.Analyzer.Integrality
  ( circuitToLcd
  , reparameterize
  )
import Pecac.Analyzer.Problem (ParamCirc)
import Pecac.Printer.Problem
  ( encodeProblem
  , printFile
  )
import PecacExe.IOUtils
  ( logShowableVect
  , readCirc
  )

-----------------------------------------------------------------------------------------
-- * FindLCD Mode.

-- | Prints the least common denominators (lcd's) for each parameter in the circuit.
findLcd :: ParamCirc -> IO ()
findLcd circ = logShowableVect "lcd" $ circuitToLcd circ

-----------------------------------------------------------------------------------------
-- * JoinLCD Mode.

-- | Implementation details for getJointLcd. Takes as input the lcd seqeuence for each of
-- the two circuits.
getJointLcdImpl :: [Integer] -> [Integer] -> Maybe [Integer]
getJointLcdImpl [] []         = Just []
getJointLcdImpl [] _          = Nothing
getJointLcdImpl _  []         = Nothing
getJointLcdImpl (x:xs) (y:ys) = maybeApply (getJointLcdImpl xs ys) (lcm x y :)

-- | Takes as input a pair of parameterized circuits. If the two circuits have the same
-- number of parameters, then a pairwise list of least common denominators (lcd's) is
-- returned. Otherwise, nothing is returned.
getJointLcd :: ParamCirc -> ParamCirc -> Maybe [Integer]
getJointLcd circ1 circ2 = getJointLcdImpl lcd1 lcd2
    where lcd1 = circuitToLcd circ1
          lcd2 = circuitToLcd circ2

-- | Attempts to print the joint least common denominators (lcd's) of the two circuits.
joinLcd :: ParamCirc -> ParamCirc -> IO ()
joinLcd circ1 circ2 =
    case getJointLcd circ1 circ2 of
        Just lcds -> logShowableVect "lcd" lcds
        Nothing   -> putStrLn "Circuits have differing numbers of parameters."

-----------------------------------------------------------------------------------------
-- * Rescale Mode.

-- | Reparameterizes the first circuit, based on the provided list of least common
-- denominators, such that the resulting circuit is integral. The results are printed to
-- standard out.
rescaleImpl :: ParamCirc -> [Integer] -> IO ()
rescaleImpl circ lcds =
    case reparameterize factors circ of
        Just reparam -> putStrLn $ printFile $ encodeProblem reparam
        Nothing      -> putStrLn "Unexpected failure."
    where factors = map (\x -> x % 1) lcds

-- | Attempts to reparameterize the first circuit based on the joint least common
-- denominators (lcd's) of the two circuits.
rescale :: ParamCirc -> ParamCirc -> IO ()
rescale src otr =
    case getJointLcd src otr of
        Just lcds -> rescaleImpl src lcds
        Nothing   -> putStrLn "Circuits have differing numbers of parameters."

-----------------------------------------------------------------------------------------
-- * Entry Point.

-- | Unwraps the src files, and parses their contents as circuits. The circuits are then
-- passed along to the reparameterization procedure.
processArgs :: ReparamTool -> IO ()
processArgs (FindLCD src)     = readCirc src findLcd
processArgs (JoinLCD src otr) = 
    readCirc src $ \srcCirc ->
        readCirc otr $ joinLcd srcCirc
processArgs (Rescale src otr) = 
    readCirc src $ \srcCirc ->
        readCirc otr $ rescale srcCirc

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args

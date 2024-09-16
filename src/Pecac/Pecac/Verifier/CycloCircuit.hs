-- | A library for constructing quantum circuits over the cyclotomic numbers.

module Pecac.Verifier.CycloCircuit
  ( circToMat
  , findGlobalPhase
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Pecac.Maybe (branchJust)
import Pecac.List (repeatn)
import Pecac.Analyzer.Revolution
  ( Revolution
  , rationalToRev
  )
import Pecac.Analyzer.Problem
  ( ParamArr (..)
  , ParamCirc (..)
  , QubitReg (..)
  , toParamCount
  )
import Pecac.Analyzer.Gate (GateSummary (..))
import Pecac.Verifier.CycloGate
  ( Cyclotomic
  , CycMat
  , gateToMat
  , idGate
  )

import qualified Pecac.Verifier.Matrix as Matrix

-----------------------------------------------------------------------------------------
-- * Circuit to Matrix Conversion.

-- | Takes as input the number of qubits (n), an instantiation for each angle in the gate
-- list (as a rational degree), and a list of gates. Returns a matrix which corresponds
-- to applying each gate of the list, in order, to an n-qubit system, with all rotations
-- instantiated according to the angles.
gatesToMat :: Int -> [Revolution] -> [GateSummary] -> CycMat
gatesToMat n thetas []           = idGate n
gatesToMat n thetas (gate:gates) = Matrix.compose cmat gmat
    where cmat = gatesToMat n thetas gates
          gmat = gateToMat n thetas gate

-- | Takes as input a list of rational degrees and a parameterized circuit. If the length
-- of the list of angles is equal to the number of parameters in the circuit, then a
-- matrix is returned which corresponds to executing the circuit, with the given angles.
-- Otherwise, nothing is returned.
circToMat :: [Revolution] -> ParamCirc -> Maybe CycMat
circToMat thetas (ParamCirc (ParamArr _ psz) (QubitReg _ qsz) gates) =
    if len == psz
    then Just $ gatesToMat qsz thetas gates
    else Nothing
    where len = length thetas

-----------------------------------------------------------------------------------------
-- * Global Phase Extraction.

-- | Takes as input a pair of parameterized circuit (circ1 and circ2). The two circuits
-- are compared with respect to the zero parameter. If the two instantiations differ by a
-- global phase s, then this global phase is returned (specifically, the instantiation of
-- circ2 is obtained by multiplying circ1 by s). Otherwise, nothing is returned.
findGlobalPhase :: ParamCirc -> ParamCirc -> Maybe Cyclotomic
findGlobalPhase circ1 circ2 =
    branchJust (circToMat theta0 circ1) $ \op1 ->
        branchJust (circToMat theta0 circ2) (Matrix.findScalar op1)
    where angle0 = rationalToRev 0
          theta0 = repeatn angle0 $ toParamCount circ1

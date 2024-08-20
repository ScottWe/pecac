-- | A library for constructing quantum circuits over the cyclotomic numbers.

module Pecac.Verifier.CycloCircuit (circToMat) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Pecac.Analyzer.Problem
  ( ParamArr (..)
  , ParamCirc (..)
  , QubitReg (..)
  )
import Pecac.Analyzer.Gate (GateSummary (..))
import Pecac.Verifier.CycloGate
  ( CycMat
  , gateToMat
  , idGate
  )

import qualified Pecac.Verifier.Matrix as Matrix

-----------------------------------------------------------------------------------------
-- * Circuit to Matrix Conversion

-- | Takes as input the number of qubits (n), an instantiation for each angle in the gate
-- list (as a rational degree), and a list of gates. Returns a matrix which corresponds
-- to applying each gate of the list, in order, to an n-qubit system, with all rotations
-- instantiated according to the angles.
gatesToMat :: Int -> [Rational] -> [GateSummary] -> CycMat
gatesToMat n thetas []           = idGate n
gatesToMat n thetas (gate:gates) = Matrix.compose cmat gmat
    where cmat = gatesToMat n thetas gates
          gmat = gateToMat n thetas gate

-- | Takes as input a list of rational degrees and a parameterized circuit. If the length
-- of the list of angles is equal to the number of parameters in the circuit, then a
-- matrix is returned which corresponds to executing the circuit, with the given angles.
-- Otherwise, nothing is returned.
circToMat :: [Rational] -> ParamCirc -> Maybe CycMat
circToMat thetas (ParamCirc (ParamArr _ psz) (QubitReg _ qsz) gates) =
    if len == psz
    then Just $ gatesToMat qsz thetas gates
    else Nothing
    where len = length thetas

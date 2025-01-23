-- | Problem-specific abstraction for OpenQASM 3 files.

module Pecac.Analyzer.Problem
  ( ParamArr (..)
  , ParamCirc (..)
  , QubitReg (..)
  , addGlobalPhase
  , toGates
  , toParamCount
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Pecac.Affine (Affine)
import Pecac.Analyzer.Gate
  ( GateSummary (..)
  , createGlobalPhase
  )
import Pecac.Analyzer.Revolution (Revolution)

-----------------------------------------------------------------------------------------
-- * Datatypes for Circuit Parameters.

-- | The name and size of a parameter array.
data ParamArr = ParamArr String Int deriving (Show, Eq)

-- | The name and size fo a qubit register.
data QubitReg = QubitReg String Int deriving (Show, Eq)

-----------------------------------------------------------------------------------------
-- * Parameterized Circuit Description.

-- | Abstract description of a parameterized OpenQASM circuit.
data ParamCirc = ParamCirc ParamArr QubitReg [GateSummary] deriving (Show, Eq)

-- | Modifies the circuit to include a global phase.
addGlobalPhase :: Affine Rational Revolution -> ParamCirc -> ParamCirc
addGlobalPhase params (ParamCirc parr qreg gates) = ParamCirc parr qreg $ gphase : gates
    where gphase = createGlobalPhase params

-- | Returns the number of parameters in a circuit.
toParamCount :: ParamCirc -> Int
toParamCount (ParamCirc (ParamArr _ n) _ _) = n

-- | Returns the list of gates in the circuit.
toGates :: ParamCirc -> [GateSummary]
toGates (ParamCirc _ _ gates) = gates

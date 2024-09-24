-- | Problem-specific abstraction for OpenQASM 3 files.

module Pecac.Analyzer.Problem
  ( ParamArr (..)
  , ParamCirc (..)
  , QubitReg (..)
  , toGates
  , toParamCount
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Pecac.Analyzer.Gate (GateSummary (..))

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

-- | Returns the number of parameters in a circuit.
toParamCount :: ParamCirc -> Int
toParamCount (ParamCirc (ParamArr _ n) _ _) = n

-- | Returns the list of gates in the circuit.
toGates :: ParamCirc -> [GateSummary]
toGates (ParamCirc _ _ gates) = gates

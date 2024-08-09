-- | Problem-specific abstraction for OpenQASM 3 files.

module Pecac.Analyzer.Problem
  ( ParamArr (..)
  , ParamCirc (..)
  , QubitReg (..)
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

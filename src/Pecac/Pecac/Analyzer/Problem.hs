-- | Problem-specific abstraction for OpenQASM 3 files.

module Pecac.Analyzer.Problem
  ( ParamArr (..)
  , QubitReg (..)
  ) where

-----------------------------------------------------------------------------------------
-- * Enumeration of Supported Gate Types.

-- | The name and size of a parameter array.
data ParamArr = ParamArr String Int

-- | The name and size fo a qubit register.
data QubitReg = QubitReg String Int

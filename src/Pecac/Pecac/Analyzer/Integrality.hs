-- |

module Pecac.Analyzer.Integrality
  ( circuitToLcd
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.Ratio (denominator)
import Pecac.Affine (cmap)
import Pecac.List
  ( mergeWith
  , repeatn
  )
import Pecac.Analyzer.Gate (GateSummary (..))
import Pecac.Analyzer.Problem
  ( ParamCirc
  , toGates
  , toParamCount
  )

-----------------------------------------------------------------------------------------
-- * Lowest Common Denominator.

-- | Helper function to implement the fold described by circuitToLcd.
lcdFold :: [Integer] -> GateSummary -> [Integer]
lcdFold lcds (PlainSummary _ _)   = lcds
lcdFold lcds (RotSummary _ aff _) = mergeWith lcm 1 lcds denoms
    where denoms = cmap denominator aff

-- | For each parameter theta_j in a circuit, this function computes the lowest common
-- denominator for all coefficients of theta_j in the circuit. This information is
-- returned as an array of integers, such that element j is the lowest common denominator
-- for parameter theta_j.
circuitToLcd :: ParamCirc -> [Integer]
circuitToLcd circ = foldl lcdFold init $ toGates circ
    where init = repeatn 1 $ toParamCount circ

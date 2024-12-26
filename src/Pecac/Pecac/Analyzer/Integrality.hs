-- | Tools for circuit reparameterization, specifically for integral coefficients. 

module Pecac.Analyzer.Integrality
  ( circuitToLcd
  , reparameterize
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.List (foldl')
import Data.Ratio (denominator)
import Pecac.Affine
  ( Affine
  , affine
  , cmap
  , skew
  )
import Pecac.List
  ( mergeWith
  , repeatn
  )
import Pecac.Analyzer.Gate (GateSummary (..))
import Pecac.Analyzer.Problem
  ( ParamCirc (..)
  , toGates
  , toParamCount
  )
import Pecac.Analyzer.Revolution (Revolution)

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
circuitToLcd circ = foldl' lcdFold init $ toGates circ
    where init = repeatn 1 $ toParamCount circ

-----------------------------------------------------------------------------------------
-- * Circuit Reparameterization.

-- | Helper function to implement the fold described by reparameterize.
repFold :: [Rational] -> GateSummary -> GateSummary
repFold factors (RotSummary name aff confs) = RotSummary name naff confs
    where naff = skew factors aff
repFold _ g = g

-- | This function taks as input a sequence of coefficients, and a parameterized circuit.
-- If the sequence of coefficients has as many elements as the circuit has parameters,
-- then returns a new circuit obtained by scaling each parameter theta_j by the provided
-- coefficient factor_j. Otherwise, if number of coefficients differs from the number of
-- parameters, then nothing is returned.
reparameterize :: [Rational] -> ParamCirc -> Maybe ParamCirc
reparameterize factors circ@(ParamCirc ps vs gates) =
    if length factors == toParamCount circ
    then Just $ ParamCirc ps vs ngates
    else Nothing
    where ngates = map (repFold factors) gates

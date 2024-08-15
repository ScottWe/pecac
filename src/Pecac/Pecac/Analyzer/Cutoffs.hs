-- | Functions to compute cutoff bounds for qunatum circuits.

module Pecac.Analyzer.Cutoffs
  ( circToKappa
  , circToLambda
  , forallElimSize
  , gatesToAlphas
  , gatesToKappa
  , gatesToLambda
  , randomSampleSize
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.Maybe (catMaybes)
import Pecac.Analyzer.Gate (GateSummary (..))
import Pecac.Analyzer.Problem
  ( ParamArr (..)
  , ParamCirc (..)
  )
import Pecac.List (repeatn)

-----------------------------------------------------------------------------------------
-- * Helper methods.

-- |
getSize :: ParamCirc -> Int
getSize (ParamCirc (ParamArr _ sz) _ _) = sz

-- |
isSameSize :: ParamCirc -> ParamCirc -> Bool
isSameSize circ1 circ2 = getSize circ1 == getSize circ2

-- |
getGates :: ParamCirc -> [GateSummary]
getGates (ParamCirc _ _ gates) = gates

-----------------------------------------------------------------------------------------
-- * Alpha Extraction.

-- | Returns the alpha vector associated with a gate. If the gate is a plain gate, then
-- nothing is returned. Otherwise, the coefficient vector is returned.
gateToAlpha :: GateSummary -> Maybe [Integer]
gateToAlpha (PlainSummary _ _)     = Nothing
gateToAlpha (RotSummary _ alpha _) = Just alpha

-- | Returns the list of alpha vectors associated with a list of gates. For each rotation
-- gate, there is a element in the list of alpha vectors. This element corresponds to the
-- coefficient vector of the rotation (i.e., the coefficients of the angle parameters in
-- the integer linear sum of parameters).
gatesToAlphas :: [GateSummary] -> [[Integer]]
gatesToAlphas = catMaybes . map gateToAlpha

-----------------------------------------------------------------------------------------
-- * Lambda-Value Calcuation.

-- | This zip function takes the sum of the left-hand side with the absolute value of the
-- right-hand side. This coincides with how lambda-values are computed.
lambdaZip :: Integer -> Integer -> Integer
lambdaZip lambda_j alpha_j = lambda_j + abs alpha_j

-- | Computes the lambda vector associated with a list of gate summaries. This is the sum
-- of the absolute value of each gate's alpha vector.
gatesToLambda :: Int -> [GateSummary] -> [Integer]
gatesToLambda len gates = foldl (zipWith lambdaZip) init avec
    where init = repeatn 0 len
          avec = gatesToAlphas gates

-- | Computes the lambda vector associated with the gates in a circuit (see gatesToLambda
-- for more details).
circToLambda :: ParamCirc -> [Integer]
circToLambda circ = gatesToLambda (getSize circ) $ getGates circ

-----------------------------------------------------------------------------------------
-- * Kappa-Value Calcuation.

-- | Implementation of kappaTerm, using counters for the positive and negative subsums.
kappaTermImpl :: Integer -> Integer -> [Integer] -> Integer
kappaTermImpl pos neg []     = max pos neg
kappaTermImpl pos neg (x:xs) =
    if x > 0
    then kappaTermImpl (pos + x) neg xs
    else kappaTermImpl pos (neg - x) xs

-- | Computes the sum of the strictly positive terms in a list, and the sum of the
-- strictly negative terms in a list, and returns the greatest of their asbolute values.
kappaTerm :: [Integer] -> Integer
kappaTerm = kappaTermImpl 0 0

-- | Fold function for gatesToKappa. Adds the partial kappa value to the kappa term of
-- the current alpha vector.
foldKappa :: Integer -> [Integer] -> Integer
foldKappa kappa alpha = kappa + kappaTerm alpha

-- | Computes the kappa value associated with a list of gate summaries. This is the sum
-- of the kappa terms associated with each gate's alpha vector. For more detail, see the
-- functions kappaTerm and gatesToAlphas.
gatesToKappa :: [GateSummary] -> Integer
gatesToKappa gates = foldl foldKappa 0 avec
    where avec = gatesToAlphas gates

-- | Computes the kappa vector associated with the gates in a circuit (see gatesToKappa
-- for more details).
circToKappa :: ParamCirc -> Integer
circToKappa circ = gatesToKappa $ getGates circ

-----------------------------------------------------------------------------------------
-- * Cutoff Calcuations.

-- | Returns the component-wise maximum for the lambda values from a pair of circuits.
getMaxLambda :: ParamCirc -> ParamCirc -> [Integer]
getMaxLambda circ1 circ2 = zipWith max (circToLambda circ1) (circToLambda circ2)

-- | Computes the number of instantiations needed for each parameter, such that PEC
-- reduces to the parameter-free case.
forallElimSize :: ParamCirc -> ParamCirc -> Maybe [Integer]
forallElimSize circ1 circ2 =
    if isSameSize circ1 circ2
    then let lambdaToElimSize lambda_j = 2 * lambda_j + 1
         in Just $ map lambdaToElimSize $ getMaxLambda circ1 circ2
    else Nothing

-- | Computes the numerator in the probability bound for random sampling.
randomSampleSize :: ParamCirc -> ParamCirc -> Maybe Integer
randomSampleSize circ1 circ2 = 
    if isSameSize circ1 circ2
    then let lambdaSum = sum $ getMaxLambda circ1 circ2
             kappaTerm = max (circToKappa circ1) (circToKappa circ2)
         in Just $ lambdaSum + kappaTerm
    else Nothing

-- | Functions to compute cutoff bounds for qunatum circuits.

module Pecac.Analyzer.Cutoffs
  ( CutoffResult (..)
  , circToKappa
  , circToLambda
  , forallElimSize
  , gatesToAlphas
  , gatesToKappa
  , gatesToLambda
  , getLambda
  , randomSampleSize
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.Ratio
  ( denominator
  , numerator
  )
import Pecac.Affine
  ( Affine
  , cfold
  )
import Pecac.List
  ( mergeWith
  , repeatn
  )
import Pecac.Maybe
  ( branchJust
  , maybeApply
  )
import Pecac.Analyzer.Gate
  ( GateSummary (..)
  , RotName (..)
  )
import Pecac.Analyzer.Problem
  ( ParamArr (..)
  , ParamCirc (..)
  , toGates
  , toParamCount
  )
import Pecac.Analyzer.Revolution (Revolution)

-----------------------------------------------------------------------------------------
-- * Helper methods.

-- | Determines if two parameterized circuits have the same parameter space.
isSameSize :: ParamCirc -> ParamCirc -> Bool
isSameSize circ1 circ2 = toParamCount circ1 == toParamCount circ2

-----------------------------------------------------------------------------------------
-- * Alpha Extraction.

-- | Takes as input a boolean flag, a rational number, and maybe a list of integers. If
-- the rational number corresponds to an integer and a list of integers is provided, and
-- the integer is prepended to the list. If the flag is set, then this integer is the
-- coefficient of a half-cycle gate, and should be multiplied by two. Otherwise, nothing
-- is returned.
extractIntegral :: Bool -> Rational -> Maybe [Integer] -> Maybe [Integer]
extractIntegral isHalf _  Nothing    = Nothing
extractIntegral isHalf aj (Just seq) =
    if denominator aj == 1
    then Just $ mult * base : seq
    else Nothing
    where mult = if isHalf then 2 else 1
          base = numerator aj

-- | Returns true if a gate has a period of 2pi (as opposed to 4pi).
isHalfCycle :: RotName -> Bool
isHalfCycle GPhase = True
isHalfCycle RotP   = True
isHalfCycle RotCP  = True
isHalfCycle _      = False

-- | Returns the list of alpha vectors associated with a list of gates. For each rotation
-- gate, there is a element in the list of alpha vectors. This element corresponds to the
-- coefficient vector of the rotation (i.e., the coefficients of the angle parameters in
-- the integer linear sum of parameters).
gatesToAlphas :: [GateSummary] -> Maybe [[Integer]]
gatesToAlphas []                            = Just []
gatesToAlphas (PlainSummary _ _:gates)      = gatesToAlphas gates
gatesToAlphas (RotSummary name aff _:gates) =
    branchJust (cfold f (Just []) aff) $
        \alpha -> maybeApply (gatesToAlphas gates) (alpha:)
    where f = extractIntegral (isHalfCycle name)

-----------------------------------------------------------------------------------------
-- * Lambda-Value Calcuation.

-- | This zip function takes the sum of the left-hand side with the absolute value of the
-- right-hand side. This coincides with how lambda-values are computed.
lambdaZip :: Integer -> Integer -> Integer
lambdaZip lambda_j alpha_j = lambda_j + abs alpha_j

-- | Computes the lambda vector associated with a list of gate summaries. This is the sum
-- of the absolute value of each gate's alpha vector.
gatesToLambda :: Int -> [GateSummary] -> Maybe [Integer]
gatesToLambda n gates = maybeApply (gatesToAlphas gates) apply
    where init  = repeatn 0 n
          apply = foldl (mergeWith lambdaZip 0) init

-- | Computes the lambda vector associated with the gates in a circuit (see gatesToLambda
-- for more details).
circToLambda :: ParamCirc -> Maybe [Integer]
circToLambda circ = gatesToLambda (toParamCount circ) $ toGates circ

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
gatesToKappa :: [GateSummary] -> Maybe Integer
gatesToKappa gates = maybeApply (gatesToAlphas gates) (foldl foldKappa 0)

-- | Computes the kappa vector associated with the gates in a circuit (see gatesToKappa
-- for more details).
circToKappa :: ParamCirc -> Maybe Integer
circToKappa circ = gatesToKappa $ toGates circ

-----------------------------------------------------------------------------------------
-- * Cutoff Calcuations.

-- | Either indicates the reason cutoff analysis fails, or returns the results of the
-- cutoff analysis.
data CutoffResult a = ParamMismatch
                    | RationalCoeff
                    | Result a
                    deriving (Eq, Show)

-- | Returns the component-wise maximum for the lambda values from a pair of circuits.
getMaxLambda :: ParamCirc -> ParamCirc -> Maybe [Integer]
getMaxLambda circ1 circ2 =
    branchJust (circToLambda circ1) $
        \lambda1 -> maybeApply (circToLambda circ2) (zipWith max lambda1)

-- | Computes the lambda value for a pair of circuits.
getLambda :: ParamCirc -> ParamCirc -> CutoffResult [Integer]
getLambda circ1 circ2 =
    if isSameSize circ1 circ2
    then case getMaxLambda circ1 circ2 of
        Just maxl -> Result maxl
        _         -> RationalCoeff
    else ParamMismatch

-- | Computes the number of instantiations needed for each parameter, such that PEC
-- reduces to the parameter-free case.
forallElimSize :: ParamCirc -> ParamCirc -> CutoffResult [Integer]
forallElimSize circ1 circ2 =
    case getLambda circ1 circ2 of
        Result lambda -> Result $ map lambdaToElimSize lambda
        err           -> err
    where lambdaToElimSize lambda_j = 2 * lambda_j + 1

-- | Computes the kappa term in the random sample cutoff size.
getKappaTerm :: ParamCirc -> ParamCirc -> Maybe Integer
getKappaTerm circ1 circ2 =
    branchJust (circToKappa circ1) $
        \kappa1 -> maybeApply (circToKappa circ2) (max kappa1)

-- | Equivalent to randomSampleSize, except that all parameter size checks are omitted.
-- Then the return value Nothing corresponds to the return value RationalCoeff from the
-- safe version of this function.
randomSampleSizeUnsafe :: ParamCirc -> ParamCirc -> Maybe Integer
randomSampleSizeUnsafe circ1 circ2 =
    branchJust (getMaxLambda circ1 circ2) $
        \maxl -> maybeApply (getKappaTerm circ1 circ2) (sum maxl +)

-- | Computes the numerator in the probability bound for random sampling.
randomSampleSize :: ParamCirc -> ParamCirc -> CutoffResult Integer
randomSampleSize circ1 circ2 = 
    if isSameSize circ1 circ2
    then case randomSampleSizeUnsafe circ1 circ2 of
        Just n -> Result n
        _      -> RationalCoeff
    else ParamMismatch

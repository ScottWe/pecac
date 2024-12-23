-- | A library for parameterized equivalence checking.

module Pecac.Verifier.PEC
  ( EquivFun
  , PECRes (..)
  , Side (..)
  , pec
  , ppec
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.Maybe (isNothing)
import Data.Ratio ((%))
import Pecac.Rational (qceil)
import Pecac.Analyzer.Cutoffs
  ( CutoffResult (Result)
  , forallElimSize
  , randomSampleSize
  )
import Pecac.Analyzer.Problem
  ( ParamCirc (..)
  , toParamCount
  )
import Pecac.Analyzer.Revolution
  ( Revolution
  , rationalToRev
  )
import System.Random.TF.Gen (RandomGen)
import System.Random.TF.Instances (randomR)

-----------------------------------------------------------------------------------------
-- * Data Types to Communicate Results

-- | Differentiates the LHS and the RHS of a circuit (in)equality.
data Side = LHS | RHS deriving (Show, Eq)

-- | Summarizes the result of a parameterized equivalence checking instance, or a reason
-- as to why no answers can be obtained.
data PECRes = BadCutoff
            | EvalFail Side [Revolution]
            | EqFail [Revolution]
            | EqSuccess [[Revolution]]
            deriving (Show, Eq)

-- | Takes as input the denominator of a revolution, together with an index into a sample
-- space. Returns the j-th entry with said denominator.
indexToRev :: Integer -> Integer -> Revolution
indexToRev n j = rationalToRev $ 2 * toInteger j % n

-----------------------------------------------------------------------------------------
-- * Parameterized Equivalence Checking.

-- | A function which takes a list of angles and a circuit, and if the angles are valid,
-- returns a circuit representation of type a.
type EvalFun a = ParamCirc -> [Revolution] -> Maybe a

-- | A function which takes a pair of circuit evaluations, and returns whether they are
-- equivalent or not.
type EquivFun a = a -> a -> Bool

-- | Instantiation of EvalFun for a specific circuit.
type CircFun a = [Revolution] -> Maybe a

-- | Takes as input a list of rational degrees and the evaluation functions for two
-- patameterized curcits. If the circuits cannot be evaluated with respect to these
-- angles, then an instantiation error is returned. Otherwise, the two circuits are
-- compared. If the two circuit representations are not equal, then the angle is
-- returned as a counterexample. Otherwise, nothing is returned (to indicate success).
pecCase :: [Revolution] -> CircFun a -> CircFun a -> EquivFun a -> Maybe PECRes
pecCase thetas lhsFn rhsFn eq =
    case lhsFn thetas of
        Nothing -> Just $ EvalFail LHS thetas
        Just v1 -> case rhsFn thetas of
            Nothing -> Just $ EvalFail RHS thetas
            Just v2 -> if eq v1 v2 then Nothing else Just $ EqFail thetas

-----------------------------------------------------------------------------------------
-- * Deterministic Equivalence Checking.

-- | Takes as input the cutoff value for a specific parameter, and returns a sufficient
-- number of distinct rational multiples of pi from [0, 4), such that parameterized
-- equivalence can be determined.
cutoffToParamSet :: Integer -> [Revolution]
cutoffToParamSet n =
    if n <= 2 * denom
    then map (indexToRev denom) [0 .. (n - 1)]
    else error $ "Internal error: cutoffToParamSet exceeded two periods."
    where denom = qceil $ n % 2

-- | Takes as input a list of parameter sets, a list of rational angles, and the
-- evaluation functions for two parameterized circuits. The purpose of this function is
-- to evaluate the two parameterized circuits on all possible choices of angles, and to
-- return an error if and only if the circuits yield different representations on at
-- least one angle (if the circuits fail to evaluate, then this also counts as an error).
-- Otherwise, nothing is returned.
--
-- Note: This function builds the parameter list is reverse order. For example, the
-- function should be initialized with ([D_k, ..., D_1] [] fn1 fn2) where D_j is the
-- parameter set for the j-th parameter. After j steps of the algorithm, the inputs to
-- the function will be ([D_{k-j}, ..., D_1] [D_{k-j+1}, ..., D_k] fn1 fn2).
pecRun :: [[Revolution]] -> [Revolution] -> CircFun a -> CircFun a -> EquivFun a
                         -> Maybe PECRes
pecRun []           thetas lhsFn rhsFn eq = pecCase thetas lhsFn rhsFn eq
pecRun (pset:psets) thetas lhsFn rhsFn eq = foldl f Nothing pset
    where f res x = if isNothing res then pecRun psets (x:thetas) lhsFn rhsFn eq else res

-- | Takes as input a list of cutoff bounds for the parameters, a pair of parameterized
-- circuits, and a function which faithfully evaluates each circuit, given a choice of
-- angles. Using these cutoff results, the circuits will be evaluated on sufficiently
-- many instances (as described in pec), with results produced accordingly.
pecSetup :: [Integer] -> ParamCirc -> ParamCirc -> EvalFun a -> EquivFun a -> PECRes
pecSetup cutoffs circ1 circ2 eval eq =
    case pecRun (reverse paramSet) [] (eval circ1) (eval circ2) eq of
        Nothing  -> EqSuccess paramSet
        Just res -> res
    where paramSet = map cutoffToParamSet cutoffs

-- | Takes as input a pair of parameterized circuits, and a function which faithfully
-- evaluates each circuit, given a choice of angles (i.e., the representation of two
-- circuits is the same, if and only if they are equivalent as parameterized circuits).
-- Given these circuits, cutoff analysis will be performed, and then the circuits will
-- be evaluated on sufficiently many instances, to determine if they are equivalent as
-- circuits. If the evaluation is successful, then either the circuits are equivalent and
-- the set of evaluated parameters is returned, or the circuits are inequivalent and an
-- instantiation of the parameters witnessing this inequivalence is returned. If the
-- evaluation fails at any point, then an error result which summarizes the failure is
-- returned.
pec :: ParamCirc -> ParamCirc -> EvalFun a -> EquivFun a -> PECRes
pec circ1 circ2 eval eq =
    case forallElimSize circ1 circ2 of
        Result cutoffs -> pecSetup cutoffs circ1 circ2 eval eq
        _              -> BadCutoff

-----------------------------------------------------------------------------------------
-- * Probabilistic Equivalence Checking.

-- | Takes as input a random generator, the number of parameters to select, and the size
-- of the sample space (i.e., the denominator of the angles as revolutions). Returns the
-- desired number of parameters, selected according to the random generator, along with
-- the newest state of the random generator.
sampleParams :: RandomGen g => g -> Int -> Integer -> (g, [Revolution])
sampleParams rgen  0 _  = (rgen, [])
sampleParams rgen0 k sz = (rgen2, param : params)
    where (j, rgen1)      = randomR (0, sz - 1) rgen0
          param           = indexToRev sz j
          (rgen2, params) = sampleParams rgen1 (k - 1) sz

-- | Takes as input a random generator, a desired probability, the d-value from the
-- DeMillo-Lipton-Schwartz-Zippel lemma, a pair of parameterized circuits, and a function
-- which faithfully evaluates each circuit, given a choice of angles. Using the d-value
-- and the desired probability, the circuits will be evaluated on a randomly selected
-- parameter from a sufficiently large sample space (as described in ppec), with results
-- produced accordingly.
ppecSetup :: RandomGen g => g -> Rational -> Integer -> ParamCirc -> ParamCirc
                              -> EvalFun a -> EquivFun a -> (g, PECRes)
ppecSetup rgen0 prob cutoff circ1 circ2 eval eq =
    case pecCase params (eval circ1) (eval circ2) eq of
        Nothing  -> (rgen1, EqSuccess $ map (: []) params)
        Just res -> (rgen1, res)
    where k               = toParamCount circ1
          sz              = qceil $ (cutoff % 1) / prob
          (rgen1, params) = sampleParams rgen0 k sz

-- | Takes as input a random generator, a desired probability, a pair of parameterized
-- circuits, and a function which faithfully evaluates each circuit, given a choice of
-- angles (i.e., the representation of two circuits is the same, if and only if they are
-- equivalent as parameterized circuits). Given these circuits, cutoff analysis will be
-- performed, and then the circuits will be evaluated on a randomly selected parameter,
-- to determine if they are equivalent of circuits with probability of an incorrect answer
-- bounded above by the desired probability. If the evaluation is successful, then either
-- the circuits are likely equivalent and the evaluated parameter is returned, or the
-- circuits are inequivalent and an instantiation of the parameters witnessing this
-- inequivalence is returned. If the evaluation fails at any point, then an error result
-- which summarizes the failure is returned. In either case, the newest state of the
-- random generator is returned.
--
-- Remark: This function has a false positive rate (claiming circuits are equivalent when
-- they are not) bounded by the provided probability, and a false negative rate of zero.
ppec :: RandomGen g => g -> Rational -> ParamCirc -> ParamCirc -> EvalFun a -> EquivFun a
                         -> (g, PECRes)
ppec rgen prob circ1 circ2 eval eq =
    case randomSampleSize circ1 circ2 of
        Result cutoff -> ppecSetup rgen prob cutoff circ1 circ2 eval eq
        _             -> (rgen, BadCutoff)

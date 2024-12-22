-- | A library for constructing quantum circuits over the cyclotomic numbers.

module Pecac.Verifier.CycloCircuit
  ( GPhaseResult (..)
  , circToMat
  , findGlobalPhase
  , findLinearPhase
  , precomputeMat
  , phaseEquiv
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.Ratio
  ( (%)
  , denominator
  , numerator
  )
import Pecac.Cyclotomic (einv)
import Pecac.Maybe
  ( branchJust
  , maybeApply
  )
import Pecac.List (repeatn)
import Pecac.Analyzer.Cutoffs
  ( CutoffResult (..)
  , getLambda
  )
import Pecac.Analyzer.Revolution
  ( Revolution
  , rationalToRev
  )
import Pecac.Analyzer.Problem
  ( ParamArr (..)
  , ParamCirc (..)
  , QubitReg (..)
  , toParamCount
  )
import Pecac.Analyzer.Gate
  ( GateSummary (..)
  , isParameterized
  )
import Pecac.Verifier.CycloGate
  ( Cyclotomic
  , CycMat
  , gateToMat
  , idGate
  )

import qualified Pecac.Verifier.Matrix as Matrix

-----------------------------------------------------------------------------------------
-- * Circuit to Matrix Conversion.

-- | Composes pairs of adjacent matrices in a divide-and-conqure strategy.
composeMatsImpl :: [CycMat] -> [CycMat]
composeMatsImpl []           = []
composeMatsImpl [m]          = [m]
composeMatsImpl (m1:m2:mats) = (Matrix.compose m2 m1) : composeMatsImpl mats

-- | Takes as input the number of qubits (n), and a sequence of cyclotomic operators.
-- Multiplies the sequence in a divide-and-conqure strategy.
composeMats :: Int -> [CycMat] -> CycMat
composeMats n []   = idGate n
composeMats n [m]  = m
composeMats n mats = composeMats n $ composeMatsImpl mats

-- | Takes as input the number of qubits (n), an instantiation for each angle in the gate
-- list (as a rational degree), and a list of gates. Returns a matrix which corresponds
-- to applying each gate of the list, in order, to an n-qubit system, with all rotations
-- instantiated according to the angles.
gatesToMat :: Int -> [Revolution] -> [GateSummary] -> CycMat
gatesToMat n thetas gates = composeMats n $ map (gateToMat n thetas) gates

-- | Takes as input a list of rational degrees and a parameterized circuit. If the length
-- of the list of angles is equal to the number of parameters in the circuit, then a
-- matrix is returned which corresponds to executing the circuit, with the given angles.
-- Otherwise, nothing is returned.
circToMat :: [Revolution] -> ParamCirc -> Maybe CycMat
circToMat thetas (ParamCirc (ParamArr _ psz) (QubitReg _ qsz) gates) =
    if len == psz
    then Just $ gatesToMat qsz thetas gates
    else Nothing
    where len = length thetas

-----------------------------------------------------------------------------------------
-- * Circuit to Matrix Precomputation.

-- | Internal data-type used to represent a partially evaluated circuit. Each precomputed
-- value is either the summary for a parameterized gate, or the cyclotomic operator
-- corresponding to 1 or more parameter-free gates.
type Precomputed = Either GateSummary CycMat

-- | Takes as input the number of qubits (n) and a list of gates. Returns an equivalent
-- list of precomputed values such: (1) the length of the precomputed value list is the
-- same length as the gate list; (2) each gate summary in the list corresponds to a
-- parameterized gate in the circuit.
precomputeGates :: Int -> [GateSummary] -> [Precomputed]
precomputeGates n []           = []
precomputeGates n (gate:gates) =
    if isParameterized gate
    then Left gate : precomputeGates n gates
    else (Right $ gateToMat n [] gate) : precomputeGates n gates

-- | Takes as input a list of recomputed circuit values. Returns a minimal and equivalent
-- sequence of precomputed gates, such that all adjacent cyclotomic matrices have been
-- composed via matrix multiplication.
reduceGates :: [Precomputed] -> [Precomputed]
reduceGates l@[]                 = l
reduceGates l@[_]                = l
reduceGates l@(Left _:_)         = l
reduceGates l@(Right _:Left _:_) = l
reduceGates (Right x:Right y:l)  = reduceGates $ Right mat : l
    where mat = Matrix.compose x y

-- | Takes as input input the number of qubits (n), a list of rational degrees and a
-- precomputed value. Returns a cyclotomic matrix operator which corresponds to the
-- precomputed value, given the rational parameters. In particular, if the precomputed
-- value is a cyclotomic operator, then the operator is returned, otherwise the gate
-- summary is evaluated with respect to the rational parameters.
evalPrecomputed :: Int -> [Revolution] -> Precomputed -> CycMat
evalPrecomputed n thetas (Left g)  = gateToMat n thetas g
evalPrecomputed _ _      (Right m) = m

-- Implementation details for computeMat.
computeMatImpl :: Int -> [Precomputed] -> [Revolution] -> CycMat
computeMatImpl n ps thetas = composeMats n $ map (evalPrecomputed n thetas) ps

-- | Implementation details for precomputeMat. The function takes the qubit count,
-- parameter count, and reduced precomputed gate list from the parameterized circuit,
-- and returns the function described by precomputeMat.
--
-- Note: the reduced precomputed gate list is: reduceGates $ precomputeGates qsz gates.
computeMat :: Int -> Int -> [Precomputed] -> [Revolution] -> Maybe CycMat
computeMat qsz psz circ thetas =
    if len == psz
    then Just $ computeMatImpl qsz circ thetas
    else Nothing
    where len = length thetas

-- | Takes as input a parameterized circuit. Returns a function which take a choice of
-- parameters, and either returns nothing (if the circuit is invalid), or the cyclotomic
-- operation corresponding to the parameterization otherwise. Note that the function will
-- precompute portions of the circuit up-front (regardless of the parameterization), to
-- minimize the time spent evaluating the circuit for each parameterization.
--
-- Note: if the circuit will be evaluated once, then circToMat should be used instead.
precomputeMat :: ParamCirc -> ([Revolution] -> Maybe CycMat)
precomputeMat (ParamCirc (ParamArr _ psz) (QubitReg _ qsz) gates) =
    computeMat qsz psz $ reduceGates $ precomputeGates qsz gates

-----------------------------------------------------------------------------------------
-- * Global Phase Extraction.

-- | Takes as input a pair of parameterized circuit (circ1 and circ2). The two circuits
-- are compared with respect to the zero parameter. If the two instantiations differ by a
-- global phase s, then this global phase is returned (specifically, the instantiation of
-- circ2 is obtained by multiplying circ1 by s). Otherwise, nothing is returned.
findGlobalPhase :: ParamCirc -> ParamCirc -> Maybe Cyclotomic
findGlobalPhase circ1 circ2 =
    branchJust (circToMat theta0 circ1) $ \op1 ->
        branchJust (circToMat theta0 circ2) (Matrix.findScalar op1)
    where angle0 = rationalToRev 0
          theta0 = repeatn angle0 $ toParamCount circ1

-- | For each cyclotomic number s, returns a equivalence relation which states that x is
-- related to y whenever x and y differ by a global phase of s.
--
-- Note: This function actually relates x to y whenever s*x = y, which is not equivalent
-- to s*y = x, except for when s is 1 or (-1). This means that (phaseEquiv s) is not
-- truly an equivalence relation. Indeed, it fails reflexivity, symmetry, and
-- transitivity. However, it does witness the equivalence between x and y by the phase
-- equivalence relation, if one already knows that the phase must be s. Moreover, if x
-- and y are not equivalent up to global phase, then no such s exists.
phaseEquiv :: Cyclotomic -> CycMat -> CycMat -> Bool
phaseEquiv s x y = Matrix.scale s x == y

-----------------------------------------------------------------------------------------
-- * Linear Phase Extraction.

-- | Either indicates the reason global phase analysis fails, or returns the coefficients
-- obtained through the global phase analysis.
data GPhaseResult = CutoffFailure
                  | InferenceFailure
                  | LinearCoeffs [Rational]
                  deriving (Eq, Show)

-- | Helper type to associate a pair of circuits with their candidate global phase.
type GPhaseEquiv = (Cyclotomic, ParamCirc, ParamCirc)

-- | Returns a list of n angles, such that the k-th angle is (1 / bound) revolutions, and
-- all other angles are zero.
toScaledIndicator :: Integer -> Int -> Int -> [Revolution]
toScaledIndicator bound k n = lhs ++ (angle1 : rhs)
    where angle1 = rationalToRev $ 1 % bound
          angle0 = rationalToRev 0
          lhs    = repeatn angle0 k
          rhs    = repeatn angle0 $ n - k - 1

-- | Takes as input the cutoff bound (4*lambda_j+1) together with the rational inverse of
-- e^(i*alpha_j/(4*lambda_j+1)*pi). Returns the value of alpha_j.
extractCoeff :: Integer -> Rational -> Rational
extractCoeff bound q = bound * x % d
    where r = numerator q
          d = denominator q
          x = if (2 * r) <= d then r else r - d

-- Takes as inpute a pair of circuits which have been equated up to constant global
-- phase, the cutoff bound for parameter j, and the corresponding index j. If there
-- exists a rational number q such that q could be the coefficient of the k-th parameter
-- in some affine linear global phase equation, then q is returned. Otherwise, nothing is
-- returned.
findLinearPhaseCoeff :: GPhaseEquiv -> Integer -> Int -> Maybe Rational
findLinearPhaseCoeff (gphase, circ1, circ2) bound k =
    branchJust (circToMat indicator circ1) $ \op1 ->
        branchJust (circToMat indicator circ2) $ \op2 ->
            branchJust (Matrix.findScalar op1 op2) $ \z ->
                maybeApply (einv $ z / gphase) (extractCoeff bound)
    where indicator = toScaledIndicator bound k $ toParamCount circ1

-- | Implementation details for findLinearPhase. This functions requires that the
-- constant global phase and lambda values have already been computed. Given this data,
-- this function iterates over the parameters, and then solves for each rational
-- coefficient. If this fails, then nothing is returned. Otherwise, the coefficients from
-- k up to the last parameter are returned.
findLinearPhaseImpl :: GPhaseEquiv -> [Integer] -> Int -> Maybe [Rational]
findLinearPhaseImpl _     []      _ = Just []
findLinearPhaseImpl equiv (cj:cs) j =
    branchJust (findLinearPhaseCoeff equiv bound j) $ \coeff ->
        maybeApply (findLinearPhaseImpl equiv cs $ j + 1) (coeff :)
    where bound = cj + 1

-- | Takes as input a pair of quantum circuits. If the two circuits could differ by an
-- affine linear global phase, with all linear terms rational, then returns the rational
-- coefficients. Otherwise, nothing is returned.
--
-- Note that this function assumes that the circuits differ by an affine linear global
-- phase, and then solves for the rational coefficients. To validate these results, the
-- first circuit must be modified to include the rational linear phase, and then the two
-- circuits must then be equated up to constant global phase.
findLinearPhase :: ParamCirc -> ParamCirc -> GPhaseResult
findLinearPhase circ1 circ2 =
    case getLambda circ1 circ2 of
        Result cutoffs -> case findGlobalPhase circ1 circ2 of
            Just gphase -> case findLinearPhaseImpl (gphase, circ1, circ2) cutoffs 0 of
                Just coeffs -> LinearCoeffs coeffs
                Nothing     -> InferenceFailure
            Nothing -> InferenceFailure
        _ -> CutoffFailure

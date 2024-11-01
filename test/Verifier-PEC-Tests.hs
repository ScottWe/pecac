module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Data.Ratio
import Pecac.Affine
import Pecac.List
import Pecac.Analyzer.Gate
import Pecac.Analyzer.Problem
import Pecac.Analyzer.Revolution
import Pecac.Verifier.CycloCircuit
import Pecac.Verifier.PEC
import System.Random

-----------------------------------------------------------------------------------------
-- Helper Functions.

checkParamSets :: [Int] -> [[Revolution]] -> Bool
checkParamSets []     []           = True
checkParamSets []     _            = False
checkParamSets _      []           = False
checkParamSets (c:cs) (pset:psets) =
    if length pset == c
    then checkParamSets cs psets
    else False

toyEval :: [Revolution] -> ParamCirc -> Maybe ()
toyEval _ (ParamCirc (ParamArr v _) _ _) = if v == "bad" then Nothing else Just ()

-----------------------------------------------------------------------------------------
-- Helper Functions (Deterministic).

cycloPec :: ParamCirc -> ParamCirc -> PECRes
cycloPec circ1 circ2 = pec circ1 circ2 circToMat (==)

checkValid :: [Int] -> ParamCirc -> ParamCirc -> Bool
checkValid cutoffs circ1 circ2 =
    case cycloPec circ1 circ2 of
        EqSuccess psets -> checkParamSets cutoffs psets
        _               -> False

checkInvalid :: ParamCirc -> ParamCirc -> Bool
checkInvalid circ1 circ2 =
    case cycloPec circ1 circ2 of
        EqFail theta -> circToMat theta circ1 /= circToMat theta circ2
        _            -> False

-----------------------------------------------------------------------------------------
-- Helper Functions (Probabilistic).

rgen :: StdGen
rgen = mkStdGen 101

cycloPPec :: ParamCirc -> ParamCirc -> PECRes
cycloPPec circ1 circ2 = snd $ ppec rgen (1 % 2) circ1 circ2 circToMat (==)

checkValidProb :: ParamCirc -> ParamCirc -> Bool
checkValidProb circ1 circ2 =
    case cycloPPec circ1 circ2 of
        EqSuccess psets -> checkParamSets psetLens psets
        _               -> False
    where psetLens = repeatn 1 $ toParamCount circ1

checkInvalidProb :: ParamCirc -> ParamCirc -> Bool
checkInvalidProb circ1 circ2 =
    case cycloPPec circ1 circ2 of
        EqFail theta -> circToMat theta circ1 /= circToMat theta circ2
        _            -> False

checkProb :: ParamCirc -> ParamCirc -> Bool
checkProb circ1 circ2 =
    case cycloPPec circ1 circ2 of
        EqSuccess psets -> let theta = map (\x -> x !! 0) psets
                           in circToMat theta circ1 == circToMat theta circ2
        EqFail theta -> circToMat theta circ1 /= circToMat theta circ2
        _            -> False

-----------------------------------------------------------------------------------------
-- Helper Circuits

param1 :: Affine Rational Revolution
param1 = linear [1]

param2 :: Affine Rational Revolution
param2 = linear [-1]

param3 :: Affine Rational Revolution
param3 = linear [0, 1, 0]

param4 :: Affine Rational Revolution
param4 = linear [0, 0, 1]

param5 :: Affine Rational Revolution
param5 = linear [0, 0, 2]

param6 :: Affine Rational Revolution
param6 = linear [2]

eqCirc1 :: ParamCirc
eqCirc1 = ParamCirc (ParamArr "pvar" 2) (QubitReg "qs" 4) gates
    where gates = [PlainSummary GateSX $ GateConfigs False [] [0],
                   PlainSummary GateS $ GateConfigs False [] [2],
                   PlainSummary GateSX $ GateConfigs False [] [0],
                   PlainSummary GateS $ GateConfigs False [] [2]]

eqCirc2 :: ParamCirc
eqCirc2 = ParamCirc (ParamArr "thetas" 2) (QubitReg "data" 4) gates
    where gates = [PlainSummary GateX $ GateConfigs False [] [0],
                   PlainSummary GateZ $ GateConfigs False [] [2]]

neqCirc1 :: ParamCirc
neqCirc1 = ParamCirc (ParamArr "pvar" 3) (QubitReg "qs" 4) gates
    where gates = [PlainSummary GateSX $ GateConfigs False [] [0],
                   PlainSummary GateY $ GateConfigs False [] [1],
                   PlainSummary GateS $ GateConfigs False [] [2],
                   PlainSummary GateSX $ GateConfigs False [] [0],
                   PlainSummary GateS $ GateConfigs False [] [2]]

neqCirc2 :: ParamCirc
neqCirc2 = ParamCirc (ParamArr "thetas" 3) (QubitReg "data" 4) gates
    where gates = [PlainSummary GateX $ GateConfigs False [] [0],
                   PlainSummary GateZ $ GateConfigs False [] [2]]

peqCirc1a :: ParamCirc
peqCirc1a = ParamCirc (ParamArr "pvar" 1) (QubitReg "qs" 4) gates
    where gates = [PlainSummary GateX $ GateConfigs False [] [1],
                   RotSummary RotX param1 $ GateConfigs False [] [1]]

peqCirc1b :: ParamCirc
peqCirc1b = ParamCirc (ParamArr "thetas" 1) (QubitReg "data" 4) gates
    where gates = [RotSummary RotX param1 $ GateConfigs False [] [1],
                   PlainSummary GateX $ GateConfigs False [] [1]]

peqCirc2a :: ParamCirc
peqCirc2a = ParamCirc (ParamArr "pvar" 1) (QubitReg "qs" 3) gates
    where gates = [RotSummary RotZ param1 $ GateConfigs False [] [1],
                   PlainSummary GateCX $ GateConfigs False [] [0, 1],
                   RotSummary RotZ param2 $ GateConfigs False [] [1],
                   PlainSummary GateCX $ GateConfigs False [] [0, 1]]

peqCirc2b :: ParamCirc
peqCirc2b = ParamCirc (ParamArr "thetas" 1) (QubitReg "data" 3) gates
    where gates = [RotSummary RotCZ param6 $ GateConfigs False [] [0, 1]]

peqCirc3a :: ParamCirc
peqCirc3a = ParamCirc (ParamArr "pvar" 3) (QubitReg "qs" 3) gates
    where gates = [RotSummary RotX param3 $ GateConfigs False [] [0],
                   RotSummary RotZ param5 $ GateConfigs False [] [2]]

peqCirc3b :: ParamCirc
peqCirc3b = ParamCirc (ParamArr "pvar" 3) (QubitReg "qs" 3) gates
    where gates = [PlainSummary GateH $ GateConfigs False [] [0],
                   PlainSummary GateH $ GateConfigs False [] [1],
                   PlainSummary GateH $ GateConfigs False [] [2],
                   RotSummary RotZ param3 $ GateConfigs False [] [0],
                   RotSummary RotX param4 $ GateConfigs False [] [2],
                   RotSummary RotX param4 $ GateConfigs False [] [2],
                   PlainSummary GateH $ GateConfigs False [] [0],
                   PlainSummary GateH $ GateConfigs False [] [1],
                   PlainSummary GateH $ GateConfigs False [] [2]]

pneqCirc1a :: ParamCirc
pneqCirc1a = ParamCirc (ParamArr "pvar" 1) (QubitReg "qs" 4) gates
    where gates = [PlainSummary GateZ $ GateConfigs False [] [1],
                   RotSummary RotX param1 $ GateConfigs False [] [1]]

pneqCirc1b :: ParamCirc
pneqCirc1b = ParamCirc (ParamArr "thetas" 1) (QubitReg "data" 4) gates
    where gates = [RotSummary RotX param1 $ GateConfigs False [] [1],
                   PlainSummary GateZ $ GateConfigs False [] [1]]

pneqCirc2a :: ParamCirc
pneqCirc2a = ParamCirc (ParamArr "pvar" 1) (QubitReg "qs" 3) gates
    where gates = [RotSummary RotCZ param1 $ GateConfigs False [] [0, 1]]

pneqCirc2b :: ParamCirc
pneqCirc2b = ParamCirc (ParamArr "thetas" 1) (QubitReg "data" 3) gates
    where gates = [RotSummary RotZ param1 $ GateConfigs False [] [1],
                   PlainSummary GateCX $ GateConfigs False [] [0, 1],
                   RotSummary RotZ param2 $ GateConfigs False [] [1],
                   PlainSummary GateCX $ GateConfigs False [] [0, 1]]

pneqCirc3a :: ParamCirc
pneqCirc3a = ParamCirc (ParamArr "pvar" 3) (QubitReg "qs" 3) gates
    where gates = [RotSummary RotX param3 $ GateConfigs False [] [0],
                   RotSummary RotZ param5 $ GateConfigs False [] [2]]

pneqCirc3b :: ParamCirc
pneqCirc3b = ParamCirc (ParamArr "pvar" 3) (QubitReg "qs" 3) gates
    where gates = [PlainSummary GateH $ GateConfigs False [] [0],
                   PlainSummary GateH $ GateConfigs False [] [1],
                   RotSummary RotZ param3 $ GateConfigs False [] [0],
                   RotSummary RotX param4 $ GateConfigs False [] [2],
                   RotSummary RotX param4 $ GateConfigs False [] [2],
                   PlainSummary GateH $ GateConfigs False [] [0],
                   PlainSummary GateH $ GateConfigs False [] [1],
                   PlainSummary GateH $ GateConfigs False [] [2]]

-----------------------------------------------------------------------------------------
-- pec: Parameter-Free Case

test1 = TestCase (assertBool "pec handles equivalent parameter-free circuits."
                             (checkValid [1, 1] eqCirc1 eqCirc2))

test2 = TestCase (assertBool "pec handles inequivalent parameter-free circuits."
                             (checkInvalid neqCirc1 neqCirc2))

-----------------------------------------------------------------------------------------
-- pec: Error Cases

checkEvalFail :: Side -> ParamCirc -> ParamCirc -> Bool
checkEvalFail expected circ1 circ2 =
    case pec circ1 circ2 toyEval (==) of
        EvalFail actual _ -> expected == actual
        otherwise         -> False

test3 = TestCase (assertEqual "pec rejects circuits with misaligned parameters (1/2)."
                              BadCutoff
                              (cycloPec circ1 circ2))
    where circ1 = ParamCirc (ParamArr "pvar" 4) (QubitReg "qs" 4) []
          circ2 = ParamCirc (ParamArr "thetas" 3) (QubitReg "data" 4) []

test4 = TestCase (assertEqual "pec rejects circuits with misaligned parameters (2/2)."
                              BadCutoff
                              (cycloPec circ1 circ2))
    where circ1 = ParamCirc (ParamArr "pvar" 3) (QubitReg "qs" 4) []
          circ2 = ParamCirc (ParamArr "thetas" 4) (QubitReg "data" 4) []

test5 = TestCase (assertBool "pec identifies failed evaluations on the lhs."
                             (checkEvalFail LHS circ1 circ2))
    where circ1 = ParamCirc (ParamArr "bad" 3) (QubitReg "qs" 4) []
          circ2 = ParamCirc (ParamArr "thetas" 3) (QubitReg "data" 4) []

test6 = TestCase (assertBool "pec identifies failed evaluations on the rhs."
                             (checkEvalFail RHS circ1 circ2))
    where circ1 = ParamCirc (ParamArr "pvar" 3) (QubitReg "qs" 4) []
          circ2 = ParamCirc (ParamArr "bad" 3) (QubitReg "data" 4) []

-----------------------------------------------------------------------------------------
-- pec: Equivalent Parameterized Circuits

test7 = TestCase (assertBool "pec verifies RX commutes with Pauli-X (cutoff 3)."
                             (checkValid [3] peqCirc1a peqCirc1b))

test8 = TestCase (assertBool "pec verifies the standard C(RZ) decomposition (cutoff 5)."
                             (checkValid [5] peqCirc2a peqCirc2b))

test9 = TestCase (assertBool "pec verifies a multi-parameter circuit."
                             (checkValid [1, 3, 5] peqCirc3a peqCirc3b))

-----------------------------------------------------------------------------------------
-- pec: Inequivalent Parameterized Circuits

test10 = TestCase (assertBool "pec shows RX does not commute with Pauli-Z (cutoff 3)."
                              (checkInvalid pneqCirc1a pneqCirc1b))

test11 = TestCase (assertBool "pec catches error in a C(RZ) decomposition (cutoff 5)."
                              (checkInvalid pneqCirc2a pneqCirc2b))

test12 = TestCase (assertBool "pec refutes a multi-parameter circuit equivalence."
                              (checkInvalid pneqCirc3a pneqCirc3b))

-----------------------------------------------------------------------------------------
-- pec: Rejects Rational Coefficients

test13 = TestCase (assertEqual "pec handles cutoff failures due to rational coeffs."
                               BadCutoff
                               (cycloPec circ1 circ2))
    where gates1 = [RotSummary RotX param1 $ GateConfigs False [] [1]]
          circ1  = ParamCirc (ParamArr "pvar" 1) (QubitReg "qs" 4) gates1
          badarg = linear [1 % 2]
          gates2 = [RotSummary RotX badarg $ GateConfigs False [] [1]]
          circ2  = ParamCirc (ParamArr "thetas" 1) (QubitReg "data" 4) gates2

-----------------------------------------------------------------------------------------
-- ppec: Parameter-Free Case

test14 = TestCase (assertBool "ppec handles equivalent parameter-free circuits."
                              (checkValidProb eqCirc1 eqCirc2))

test15 = TestCase (assertBool "ppec handles inequivalent parameter-free circuits."
                              (checkInvalidProb neqCirc1 neqCirc2))

-----------------------------------------------------------------------------------------
-- ppec: Error Cases

test16 = TestCase (assertEqual "ppec rejects circuits with misaligned parameters (1/2)."
                               BadCutoff
                               (cycloPPec circ1 circ2))
    where circ1 = ParamCirc (ParamArr "pvar" 4) (QubitReg "qs" 4) []
          circ2 = ParamCirc (ParamArr "thetas" 3) (QubitReg "data" 4) []

test17 = TestCase (assertEqual "ppec rejects circuits with misaligned parameters (2/2)."
                               BadCutoff
                               (cycloPPec circ1 circ2))
    where circ1 = ParamCirc (ParamArr "pvar" 3) (QubitReg "qs" 4) []
          circ2 = ParamCirc (ParamArr "thetas" 4) (QubitReg "data" 4) []

test18 = TestCase (assertBool "ppec identifies failed evaluations on the lhs."
                              (checkEvalFail LHS circ1 circ2))
    where circ1 = ParamCirc (ParamArr "bad" 3) (QubitReg "qs" 4) []
          circ2 = ParamCirc (ParamArr "thetas" 3) (QubitReg "data" 4) []

test19 = TestCase (assertBool "ppec identifies failed evaluations on the rhs."
                              (checkEvalFail RHS circ1 circ2))
    where circ1 = ParamCirc (ParamArr "pvar" 3) (QubitReg "qs" 4) []
          circ2 = ParamCirc (ParamArr "bad" 3) (QubitReg "data" 4) []

-----------------------------------------------------------------------------------------
-- ppec: Equivalent Parameterized Circuits

checkProbEvalFail :: Side -> ParamCirc -> ParamCirc -> Bool
checkProbEvalFail expected circ1 circ2 =
    case ppec rgen (1 % 2) circ1 circ2 toyEval (==) of
        (_, EvalFail actual _) -> expected == actual
        _                      -> False

test20 = TestCase (assertBool "ppec verifies RX commutes with Pauli-X."
                              (checkValidProb peqCirc1a peqCirc1b))

test21 = TestCase (assertBool "ppec verifies the standard C(RZ) decomposition."
                              (checkValidProb peqCirc2a peqCirc2b))

test22 = TestCase (assertBool "ppec verifies a multi-parameter circuit."
                              (checkValidProb peqCirc3a peqCirc3b))

-----------------------------------------------------------------------------------------
-- ppec: Inequivalent Parameterized Circuits

test23 = TestCase (assertBool "ppec may show that RX does not commute with Pauli-Z."
                              (checkProb pneqCirc1a pneqCirc1b))

test24 = TestCase (assertBool "ppec may catch an error in a C(RZ) decomposition."
                              (checkProb pneqCirc2a pneqCirc2b))

test25 = TestCase (assertBool "ppec may refute a multi-parameter circuit equivalence."
                              (checkProb pneqCirc3a pneqCirc3b))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "pec_Cyclo_ParamFree_Equiv" test1,
                                     TestLabel "pec_Cyclo_ParamFree_Inequiv" test2,
                                     TestLabel "pec_Cyclo_BadCutoff_1" test3,
                                     TestLabel "pec_Cyclo_BadCutoff_2" test4,
                                     TestLabel "pec_ToyEval_EvalFail_LHS" test5,
                                     TestLabel "pec_ToyEval_EvalFail_RHS" test6,
                                     TestLabel "pec_Cyclo_Param_Equiv_C_3" test7,
                                     TestLabel "pec_Cyclo_Param_Equiv_C_5" test8,
                                     TestLabel "pec_Cyclo_Param_Equiv_C_1_3_5" test9,
                                     TestLabel "pec_Cyclo_Param_Inequiv_C_3" test10,
                                     TestLabel "pec_Cyclo_Param_Inequiv_C_5" test11,
                                     TestLabel "pec_Cyclo_Param_Inequiv_C_1_3_5" test12,
                                     TestLabel "pec_Cyclo_RationalCoeffs" test13,
                                     TestLabel "ppec_Cyclo_ParamFree_Equiv" test14,
                                     TestLabel "ppec_Cyclo_ParamFree_Inequiv" test15,
                                     TestLabel "ppec_Cyclo_BadCutoff_1" test16,
                                     TestLabel "ppec_Cyclo_BadCutoff_2" test17,
                                     TestLabel "ppec_ToyEval_EvalFail_LHS" test18,
                                     TestLabel "ppec_ToyEval_EvalFail_RHS" test19,
                                     TestLabel "ppec_Cyclo_Param_Equiv_1" test20,
                                     TestLabel "ppec_Cyclo_Param_Equiv_2" test21,
                                     TestLabel "ppec_Cyclo_Param_Equiv_3" test22,
                                     TestLabel "pec_Cyclo_Param_Inequiv_1" test23,
                                     TestLabel "pec_Cyclo_Param_Inequiv_2" test24,
                                     TestLabel "pec_Cyclo_Param_Inequiv_3" test25]

main = defaultMain tests

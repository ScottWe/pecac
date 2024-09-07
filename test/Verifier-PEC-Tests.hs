module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Data.Ratio
import Pecac.Affine
import Pecac.Analyzer.Gate
import Pecac.Analyzer.Problem
import Pecac.Analyzer.Revolution
import Pecac.Verifier.CycloCircuit
import Pecac.Verifier.PEC

-----------------------------------------------------------------------------------------
-- Helper Functions

cycloPec :: ParamCirc -> ParamCirc -> PECRes
cycloPec circ1 circ2 = pec circ1 circ2 circToMat (==)

checkParamSets :: [Int] -> [[Revolution]] -> Bool
checkParamSets []     []           = True
checkParamSets []     _            = False
checkParamSets _      []           = False
checkParamSets (c:cs) (pset:psets) =
    if length pset == c
    then checkParamSets cs psets
    else False

checkValid :: [Int] -> ParamCirc -> ParamCirc -> Bool
checkValid cutoffs circ1 circ2 =
    case cycloPec circ1 circ2 of
        EqSuccess psets -> checkParamSets cutoffs psets
        otherwise       -> False

checkInvalid :: ParamCirc -> ParamCirc -> Bool
checkInvalid circ1 circ2 =
    case cycloPec circ1 circ2 of
        EqFail theta -> circToMat theta circ1 /= circToMat theta circ2
        otherwise    -> False

-----------------------------------------------------------------------------------------
-- pec: Parameter-Free Case

test1 = TestCase (assertBool "pec handles equivalent parameter-free circuits."
                             (checkValid [1, 1] circ1 circ2))
    where gates1 = [PlainSummary GateSX $ GateConfigs False [] [0],
                    PlainSummary GateS $ GateConfigs False [] [2],
                    PlainSummary GateSX $ GateConfigs False [] [0],
                    PlainSummary GateS $ GateConfigs False [] [2]]
          circ1  = ParamCirc (ParamArr "pvar" 2) (QubitReg "qs" 4) gates1
          gates2 = [PlainSummary GateX $ GateConfigs False [] [0],
                    PlainSummary GateZ $ GateConfigs False [] [2]]
          circ2  = ParamCirc (ParamArr "thetas" 2) (QubitReg "data" 4) gates2

test2 = TestCase (assertBool "pec handles inequivalent parameter-free circuits."
                             (checkInvalid circ1 circ2))
    where gates1 = [PlainSummary GateSX $ GateConfigs False [] [0],
                    PlainSummary GateY $ GateConfigs False [] [1],
                    PlainSummary GateS $ GateConfigs False [] [2],
                    PlainSummary GateSX $ GateConfigs False [] [0],
                    PlainSummary GateS $ GateConfigs False [] [2]]
          circ1  = ParamCirc (ParamArr "pvar" 3) (QubitReg "qs" 4) gates1
          gates2 = [PlainSummary GateX $ GateConfigs False [] [0],
                    PlainSummary GateZ $ GateConfigs False [] [2]]
          circ2  = ParamCirc (ParamArr "thetas" 3) (QubitReg "data" 4) gates2

-----------------------------------------------------------------------------------------
-- pec: Error Cases

toyEval :: [Revolution] -> ParamCirc -> Maybe ()
toyEval _ (ParamCirc (ParamArr v _) _ _) = if v == "bad" then Nothing else Just ()

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
-- pec: Equivalence Parameterized Circuits

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

test7 = TestCase (assertBool "pec verifies RX commutes with Pauli-X (cutoff 3)."
                             (checkValid [3] circ1 circ2))
    where gates1 = [PlainSummary GateX $ GateConfigs False [] [1],
                    RotSummary RotX param1 $ GateConfigs False [] [1]]
          circ1  = ParamCirc (ParamArr "pvar" 1) (QubitReg "qs" 4) gates1
          gates2 = [RotSummary RotX param1 $ GateConfigs False [] [1],
                    PlainSummary GateX $ GateConfigs False [] [1]]
          circ2  = ParamCirc (ParamArr "thetas" 1) (QubitReg "data" 4) gates2

test8 = TestCase (assertBool "pec verifies the standard C(RZ) decomposition (cutoff 5)."
                             (checkValid [5] circ1 circ2))
    where gates1 = [RotSummary RotCZ param6 $ GateConfigs False [] [0, 1]]
          circ1  = ParamCirc (ParamArr "pvar" 1) (QubitReg "qs" 3) gates1
          gates2 = [RotSummary RotZ param1 $ GateConfigs False [] [1],
                    PlainSummary GateCX $ GateConfigs False [] [0, 1],
                    RotSummary RotZ param2 $ GateConfigs False [] [1],
                    PlainSummary GateCX $ GateConfigs False [] [0, 1]]
          circ2  = ParamCirc (ParamArr "thetas" 1) (QubitReg "data" 3) gates2

test9 = TestCase (assertBool "pec verifies a multi-parameter circuit."
                             (checkValid [1, 3, 5] circ1 circ2))
    where gates1 = [RotSummary RotX param3 $ GateConfigs False [] [0],
                    RotSummary RotZ param5 $ GateConfigs False [] [2]]
          circ1  = ParamCirc (ParamArr "pvar" 3) (QubitReg "qs" 3) gates1
          gates2 = [PlainSummary GateH $ GateConfigs False [] [0],
                    PlainSummary GateH $ GateConfigs False [] [1],
                    PlainSummary GateH $ GateConfigs False [] [2],
                    RotSummary RotZ param3 $ GateConfigs False [] [0],
                    RotSummary RotX param4 $ GateConfigs False [] [2],
                    RotSummary RotX param4 $ GateConfigs False [] [2],
                    PlainSummary GateH $ GateConfigs False [] [0],
                    PlainSummary GateH $ GateConfigs False [] [1],
                    PlainSummary GateH $ GateConfigs False [] [2]]
          circ2  = ParamCirc (ParamArr "pvar" 3) (QubitReg "qs" 3) gates2

-----------------------------------------------------------------------------------------
-- pec: Inequivalence Parameterized Circuits

test10 = TestCase (assertBool "pec shows RX does not commute with Pauli-Z (cutoff 3)."
                              (checkInvalid circ1 circ2))
    where gates1 = [PlainSummary GateZ $ GateConfigs False [] [1],
                    RotSummary RotX param1 $ GateConfigs False [] [1]]
          circ1  = ParamCirc (ParamArr "pvar" 1) (QubitReg "qs" 4) gates1
          gates2 = [RotSummary RotX param1 $ GateConfigs False [] [1],
                    PlainSummary GateZ $ GateConfigs False [] [1]]
          circ2  = ParamCirc (ParamArr "thetas" 1) (QubitReg "data" 4) gates2

test11 = TestCase (assertBool "pec catches error in a C(RZ) decomposition (cutoff 5)."
                              (checkInvalid circ1 circ2))
    where gates1 = [RotSummary RotCZ param1 $ GateConfigs False [] [0, 1]]
          circ1  = ParamCirc (ParamArr "pvar" 1) (QubitReg "qs" 3) gates1
          gates2 = [RotSummary RotZ param1 $ GateConfigs False [] [1],
                    PlainSummary GateCX $ GateConfigs False [] [0, 1],
                    RotSummary RotZ param2 $ GateConfigs False [] [1],
                    PlainSummary GateCX $ GateConfigs False [] [0, 1]]
          circ2  = ParamCirc (ParamArr "thetas" 1) (QubitReg "data" 3) gates2

test12 = TestCase (assertBool "pec refutes a multi-parameter circuit equivalence."
                              (checkInvalid circ1 circ2))
    where gates1 = [RotSummary RotX param3 $ GateConfigs False [] [0],
                    RotSummary RotZ param5 $ GateConfigs False [] [2]]
          circ1  = ParamCirc (ParamArr "pvar" 3) (QubitReg "qs" 3) gates1
          gates2 = [PlainSummary GateH $ GateConfigs False [] [0],
                    PlainSummary GateH $ GateConfigs False [] [1],
                    RotSummary RotZ param3 $ GateConfigs False [] [0],
                    RotSummary RotX param4 $ GateConfigs False [] [2],
                    RotSummary RotX param4 $ GateConfigs False [] [2],
                    PlainSummary GateH $ GateConfigs False [] [0],
                    PlainSummary GateH $ GateConfigs False [] [1],
                    PlainSummary GateH $ GateConfigs False [] [2]]
          circ2  = ParamCirc (ParamArr "pvar" 3) (QubitReg "qs" 3) gates2

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
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "pec_Cyclo_ParamFree_Equiv" test1,
                                     TestLabel "pec_Cyclo_ParamFree_Inequiv" test2,
                                     TestLabel "pec_Cyclo_BadCutoff_1" test3,
                                     TestLabel "pec_Cyclo_BadCutoff_2" test4,
                                     TestLabel "pec_ToyEval_EvalFail_LHS" test5,
                                     TestLabel "pec_ToyEval_EvalFail_LHS" test6,
                                     TestLabel "pec_Cyclo_Param_Equiv_C_3" test7,
                                     TestLabel "pec_Cyclo_Param_Equiv_C_5" test8,
                                     TestLabel "pec_Cyclo_Param_Equiv_C_1_3_5" test9,
                                     TestLabel "pec_Cyclo_Param_Inequiv_C_3" test10,
                                     TestLabel "pec_Cyclo_Param_Inequiv_C_5" test11,
                                     TestLabel "pec_Cyclo_Param_Inequiv_C_1_3_5" test12,
                                     TestLabel "pec_Cyclo_RationalCoeffs" test13]

main = defaultMain tests

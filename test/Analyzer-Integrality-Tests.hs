module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Ratio
import Pecac.Affine
import Pecac.Analyzer.Gate
import Pecac.Analyzer.Integrality
import Pecac.Analyzer.Problem

-----------------------------------------------------------------------------------------
-- Gate Summary Declarations

-- Parameter-Free Gates.

plain1 :: GateSummary
plain1 = PlainSummary GateX $ GateConfigs False [] [0]

plain2 :: GateSummary
plain2 = PlainSummary GateY $ GateConfigs False [] [1]

plain3 :: GateSummary
plain3 = PlainSummary GateSX $ GateConfigs True [] [2]

plain4 :: GateSummary
plain4 = PlainSummary GateCH $ GateConfigs False [] [0, 1]

plain5 :: GateSummary
plain5 = PlainSummary GateSwap $ GateConfigs True [Pos] [0, 1, 2]

-- Integral Parameter Gates.

iparam1_p1 :: GateSummary
iparam1_p1 = RotSummary RotX aff $ GateConfigs True [] [0]
    where aff = linear [1]

iparam2_p1 :: GateSummary
iparam2_p1 = RotSummary RotX aff $ GateConfigs True [] [0]
    where aff = linear [2]

iparam3_p1 :: GateSummary
iparam3_p1 = RotSummary RotX aff $ GateConfigs True [] [0]
    where aff = linear [0]

iparam1_p3 :: GateSummary
iparam1_p3 = RotSummary RotX aff $ GateConfigs True [] [0]
    where aff = linear [1, 1, 1]

iparam2_p3 :: GateSummary
iparam2_p3 = RotSummary RotX aff $ GateConfigs True [] [0]
    where aff = linear [2, 2, 2]

iparam3_p3 :: GateSummary
iparam3_p3 = RotSummary RotX aff $ GateConfigs True [] [0]
    where aff = linear [0, 0, 0]

-- Rational Parameter Gates.

rparam1_p1 :: GateSummary
rparam1_p1 = RotSummary RotX aff $ GateConfigs True [] [0]
    where aff = linear [1 % 14]

rparam2_p1 :: GateSummary
rparam2_p1 = RotSummary RotX aff $ GateConfigs True [] [0]
    where aff = linear [3 % 10]

rparam1_p3 :: GateSummary
rparam1_p3 = RotSummary RotX aff $ GateConfigs True [] [0]
    where aff = linear [3 % 10, 1 % 2, 1]

rparam2_p3 :: GateSummary
rparam2_p3 = RotSummary RotCX aff $ GateConfigs True [] [0, 2]
    where aff = linear [1 % 20, 1 % 3, 1 % 5]

rparam1_p2 :: GateSummary
rparam1_p2 = RotSummary RotCZ aff $ GateConfigs False [Neg] [0, 1, 2]
    where aff = linear [1 % 11, 1 % 2]

-----------------------------------------------------------------------------------------
-- 

params1 :: ParamArr
params1 = ParamArr "pvar" 1

params3 :: ParamArr
params3 = ParamArr "pvar" 3

qubits :: QubitReg
qubits = QubitReg "qvar" 3

test1 = TestCase (assertEqual "Empty circuits have unit denominators (1 parameter)."
                              [1]
                              (circuitToLcd $ ParamCirc params1 qubits []))

test2 = TestCase (assertEqual "Empty circuits have unit denominators (3 parameter)."
                              [1, 1, 1]
                              (circuitToLcd $ ParamCirc params3 qubits []))

test3 = TestCase (assertEqual "Static circuits have unit denominators (1 parameter)."
                              [1]
                              (circuitToLcd prob))
    where gates = [plain1, plain2, plain3]
          prob  = ParamCirc (ParamArr "pvar" 1) (QubitReg "qvar" 3) gates

test4 = TestCase (assertEqual "Static circuits have unit denominators (3 parameter)."
                              [1, 1, 1]
                              (circuitToLcd prob))
    where gates = [plain4, plain2, plain5]
          prob  = ParamCirc (ParamArr "pvar" 3) (QubitReg "qvar" 3) gates

test5 = TestCase (assertEqual "circuitToLcd, integral case (1 parameter) (1/3)."
                              [1]
                              (circuitToLcd prob))
    where gates = [plain4, plain2, iparam1_p1, plain5]
          prob  = ParamCirc (ParamArr "pvar" 1) (QubitReg "qvar" 3) gates

test6 = TestCase (assertEqual "circuitToLcd, integral case (1 parameter) (2/3)."
                              [1]
                              (circuitToLcd prob))
    where gates = [plain4, plain2, iparam2_p1, plain5]
          prob  = ParamCirc (ParamArr "pvar" 1) (QubitReg "qvar" 3) gates

test7 = TestCase (assertEqual "circuitToLcd, integral case (1 parameter) (3/3)."
                              [1]
                              (circuitToLcd prob))
    where gates = [plain4, plain2, iparam1_p1, plain5, iparam2_p1]
          prob  = ParamCirc (ParamArr "pvar" 1) (QubitReg "qvar" 3) gates

test8 = TestCase (assertEqual "circuitToLcd, zero case (1 parameter)."
                              [1]
                              (circuitToLcd prob))
    where prob = ParamCirc (ParamArr "pvar" 1) (QubitReg "qvar" 3) [iparam3_p1]

test9 = TestCase (assertEqual "circuitToLcd, integral case (3 parameter) (1/3)."
                              [1, 1, 1]
                              (circuitToLcd prob))
    where gates = [plain4, plain2, iparam1_p3, plain5]
          prob  = ParamCirc (ParamArr "pvar" 3) (QubitReg "qvar" 3) gates

test10 = TestCase (assertEqual "circuitToLcd, integral case (3 parameter) (2/3)."
                              [1, 1, 1]
                              (circuitToLcd prob))
    where gates = [plain4, plain2, iparam2_p3, plain5]
          prob  = ParamCirc (ParamArr "pvar" 3) (QubitReg "qvar" 3) gates

test11 = TestCase (assertEqual "circuitToLcd, integral case (3 parameter) (3/3)."
                               [1, 1, 1]
                               (circuitToLcd prob))
    where gates = [plain4, plain2, iparam1_p3, plain5, iparam2_p3]
          prob  = ParamCirc (ParamArr "pvar" 3) (QubitReg "qvar" 3) gates

test12 = TestCase (assertEqual "circuitToLcd, zero case (3 parameter)."
                               [1, 1, 1]
                               (circuitToLcd prob))
    where prob = ParamCirc (ParamArr "pvar" 3) (QubitReg "qvar" 3) [iparam3_p3]

test13 = TestCase (assertEqual "circuitToLcd, rational case (1 parameter) (1/3)."
                               [14]
                               (circuitToLcd prob))
    where gates = [plain4, plain2, rparam1_p1, plain5]
          prob  = ParamCirc (ParamArr "pvar" 1) (QubitReg "qvar" 3) gates

test14 = TestCase (assertEqual "circuitToLcd, rational case (1 parameter) (2/3)."
                               [10]
                               (circuitToLcd prob))
    where gates = [plain4, plain2, rparam2_p1, plain5]
          prob  = ParamCirc (ParamArr "pvar" 1) (QubitReg "qvar" 3) gates

test15 = TestCase (assertEqual "circuitToLcd, rational case (1 parameter) (3/3)."
                               [70]
                               (circuitToLcd prob))
    where gates = [plain4, plain2, rparam1_p1, plain5, rparam2_p1]
          prob  = ParamCirc (ParamArr "pvar" 1) (QubitReg "qvar" 3) gates

test16 = TestCase (assertEqual "circuitToLcd, rational case (3 parameter) (1/3)."
                               [10, 2, 1]
                               (circuitToLcd prob))
    where gates = [plain4, plain2, rparam1_p3, plain5]
          prob  = ParamCirc (ParamArr "pvar" 3) (QubitReg "qvar" 3) gates

test17 = TestCase (assertEqual "circuitToLcd, rational case (3 parameter) (2/3)."
                               [20, 6, 5]
                               (circuitToLcd prob))
    where gates = [plain4, plain2, rparam1_p3, plain5, rparam2_p3]
          prob  = ParamCirc (ParamArr "pvar" 3) (QubitReg "qvar" 3) gates

test18 = TestCase (assertEqual "circuitToLcd, rational case (3 parameter) (3/3)."
                               [220, 6, 5]
                               (circuitToLcd prob))
    where gates = [plain4, plain2, rparam1_p3, plain5, rparam2_p3, rparam1_p2]
          prob  = ParamCirc (ParamArr "pvar" 3) (QubitReg "qvar" 3) gates

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "circuitToLcd_Empty_P1" test1,
                                     TestLabel "circuitToLcd_Empty_P3" test2,
                                     TestLabel "circuitToLcd_Static_P1" test3,
                                     TestLabel "circuitToLcd_Static_P3" test4,
                                     TestLabel "circuitToLcd_Integral_P1_1" test5,
                                     TestLabel "circuitToLcd_Integral_P1_2" test6,
                                     TestLabel "circuitToLcd_Integral_P1_3" test7,
                                     TestLabel "circuitToLcd_Integral_P1_Zero" test8,
                                     TestLabel "circuitToLcd_Integral_P3_1" test9,
                                     TestLabel "circuitToLcd_Integral_P3_2" test10,
                                     TestLabel "circuitToLcd_Integral_P3_3" test11,
                                     TestLabel "circuitToLcd_Integral_P3_Zero" test12,
                                     TestLabel "circuitToLcd_Rational_P1_1" test13,
                                     TestLabel "circuitToLcd_Rational_P1_2" test14,
                                     TestLabel "circuitToLcd_Rational_P1_3" test15,
                                     TestLabel "circuitToLcd_Rational_P3_1" test16,
                                     TestLabel "circuitToLcd_Rational_P3_2" test17,
                                     TestLabel "circuitToLcd_Rational_P3_3" test18]

main = defaultMain tests

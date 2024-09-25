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
iparam1_p3 = RotSummary RotZ aff $ GateConfigs True [] [0]
    where aff = linear [1, 1, 1]

iparam2_p3 :: GateSummary
iparam2_p3 = RotSummary RotX aff $ GateConfigs True [] [0]
    where aff = linear [2, 2, 2]

iparam3_p3 :: GateSummary
iparam3_p3 = RotSummary RotX aff $ GateConfigs True [] [0]
    where aff = linear [0, 0, 0]

iparam4_p3 :: GateSummary
iparam4_p3 = RotSummary RotZ aff $ GateConfigs True [] [0]
    where aff = linear [2, 3, 4]

-- Rational Parameter Gates.

rparam1_p1 :: GateSummary
rparam1_p1 = RotSummary RotX aff $ GateConfigs True [] [0]
    where aff = linear [1 % 14]

rparam2_p1 :: GateSummary
rparam2_p1 = RotSummary RotX aff $ GateConfigs True [] [0]
    where aff = linear [3 % 10]

rparam3_p1 :: GateSummary
rparam3_p1 = RotSummary RotX aff $ GateConfigs True [] [0]
    where aff = linear [3 % 140]

rparam1_p3 :: GateSummary
rparam1_p3 = RotSummary RotX aff $ GateConfigs True [] [0]
    where aff = linear [3 % 10, 1 % 2, 1]

rparam2_p3 :: GateSummary
rparam2_p3 = RotSummary RotZ aff $ GateConfigs True [] [0]
    where aff = linear [1 % 20, 1 % 3, 1 % 5]

rparam1_p2 :: GateSummary
rparam1_p2 = RotSummary RotCZ aff $ GateConfigs False [Neg] [0, 1, 2]
    where aff = linear [1 % 11, 1 % 2]

rparam2_p2 :: GateSummary
rparam2_p2 = RotSummary RotCZ aff $ GateConfigs False [Neg] [0, 1, 2]
    where aff = linear [1 % 220, 1 % 6]

-----------------------------------------------------------------------------------------
-- circuitToLcd

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
                              (circuitToLcd circ))
    where gates = [plain1, plain2, plain3]
          circ  = ParamCirc params1 qubits gates

test4 = TestCase (assertEqual "Static circuits have unit denominators (3 parameter)."
                              [1, 1, 1]
                              (circuitToLcd circ))
    where gates = [plain4, plain2, plain5]
          circ  = ParamCirc params3 qubits gates

test5 = TestCase (assertEqual "circuitToLcd, integral case (1 parameter) (1/3)."
                              [1]
                              (circuitToLcd circ))
    where gates = [plain4, plain2, iparam1_p1, plain5]
          circ  = ParamCirc params1 qubits gates

test6 = TestCase (assertEqual "circuitToLcd, integral case (1 parameter) (2/3)."
                              [1]
                              (circuitToLcd circ))
    where gates = [plain4, plain2, iparam2_p1, plain5]
          circ  = ParamCirc params1 qubits gates

test7 = TestCase (assertEqual "circuitToLcd, integral case (1 parameter) (3/3)."
                              [1]
                              (circuitToLcd circ))
    where gates = [plain4, plain2, iparam1_p1, plain5, iparam2_p1]
          circ  = ParamCirc params1 qubits gates

test8 = TestCase (assertEqual "circuitToLcd, zero case (1 parameter)."
                              [1]
                              (circuitToLcd circ))
    where circ = ParamCirc params1 qubits [iparam3_p1]

test9 = TestCase (assertEqual "circuitToLcd, integral case (3 parameter) (1/3)."
                              [1, 1, 1]
                              (circuitToLcd circ))
    where gates = [plain4, plain2, iparam1_p3, plain5]
          circ  = ParamCirc params3 qubits gates

test10 = TestCase (assertEqual "circuitToLcd, integral case (3 parameter) (2/3)."
                              [1, 1, 1]
                              (circuitToLcd circ))
    where gates = [plain4, plain2, iparam2_p3, plain5]
          circ  = ParamCirc params3 qubits gates

test11 = TestCase (assertEqual "circuitToLcd, integral case (3 parameter) (3/3)."
                               [1, 1, 1]
                               (circuitToLcd circ))
    where gates = [plain4, plain2, iparam1_p3, plain5, iparam2_p3]
          circ  = ParamCirc params3 qubits gates

test12 = TestCase (assertEqual "circuitToLcd, zero case (3 parameter)."
                               [1, 1, 1]
                               (circuitToLcd circ))
    where circ = ParamCirc params3 qubits [iparam3_p3]

test13 = TestCase (assertEqual "circuitToLcd, rational case (1 parameter) (1/3)."
                               [14]
                               (circuitToLcd circ))
    where gates = [plain4, plain2, rparam1_p1, plain5]
          circ  = ParamCirc params1 qubits gates

test14 = TestCase (assertEqual "circuitToLcd, rational case (1 parameter) (2/3)."
                               [10]
                               (circuitToLcd circ))
    where gates = [plain4, plain2, rparam2_p1, plain5]
          circ  = ParamCirc params1 qubits gates

test15 = TestCase (assertEqual "circuitToLcd, rational case (1 parameter) (3/3)."
                               [70]
                               (circuitToLcd circ))
    where gates = [plain4, plain2, rparam1_p1, plain5, rparam2_p1]
          circ  = ParamCirc params1 qubits gates

test16 = TestCase (assertEqual "circuitToLcd, rational case (3 parameter) (1/3)."
                               [10, 2, 1]
                               (circuitToLcd circ))
    where gates = [plain4, plain2, rparam1_p3, plain5]
          circ  = ParamCirc params3 qubits gates

test17 = TestCase (assertEqual "circuitToLcd, rational case (3 parameter) (2/3)."
                               [20, 6, 5]
                               (circuitToLcd circ))
    where gates = [plain4, plain2, rparam1_p3, plain5, rparam2_p3]
          circ  = ParamCirc params3 qubits gates

test18 = TestCase (assertEqual "circuitToLcd, rational case (3 parameter) (3/3)."
                               [220, 6, 5]
                               (circuitToLcd circ))
    where gates = [plain4, plain2, rparam1_p3, plain5, rparam2_p3, rparam1_p2]
          circ  = ParamCirc params3 qubits gates

-----------------------------------------------------------------------------------------
-- reparameterize: Invalid

test19 = TestCase (assertEqual "reparameterize detects too few parameters (1/2)."
                               Nothing
                               (reparameterize [2] circ))
    where circ = ParamCirc params3 qubits [iparam1_p3]

test20 = TestCase (assertEqual "reparameterize detects too few parameters (2/2)."
                               Nothing
                               (reparameterize [2, 5] circ))
    where circ = ParamCirc params3 qubits [iparam1_p3]

test21 = TestCase (assertEqual "reparameterize detects too many parameters (1/2)."
                               Nothing
                               (reparameterize [2, 5] circ))
    where circ = ParamCirc params1 qubits [iparam1_p1]

test22 = TestCase (assertEqual "reparameterize detects too many parameters (2/2)."
                               Nothing
                               (reparameterize [2, 5, 6] circ))
    where circ = ParamCirc params1 qubits [iparam1_p1]

-----------------------------------------------------------------------------------------
-- reparameterize: Valid

test23 = TestCase (assertEqual "reparameterize handles rotation-free circutis (1 param)."
                               (Just circ)
                               (reparameterize [3] circ))
    where gates = [plain1, plain2, plain3]
          circ  = ParamCirc params1 qubits gates

test24 = TestCase (assertEqual "reparameterize handles rotation-free circutis (3 param)."
                               (Just circ)
                               (reparameterize [3, 4, 5] circ))
    where gates = [plain4, plain2, plain5]
          circ  = ParamCirc params3 qubits gates

test25 = TestCase (assertEqual "reparameterize handles single parameters (1/2)."
                               (Just circ2)
                               (reparameterize [2] circ1))
    where gates1 = [plain1, plain2, iparam1_p1, plain3]
          gates2 = [plain1, plain2, iparam2_p1, plain3]
          circ1  = ParamCirc params1 qubits gates1
          circ2  = ParamCirc params1 qubits gates2

test26 = TestCase (assertEqual "reparameterize handles single parameters (2/2)."
                               (Just circ2)
                               (reparameterize [1 % 14] circ1))
    where gates1 = [plain4, plain2, iparam1_p1, plain5, rparam2_p1]
          gates2 = [plain4, plain2, rparam1_p1, plain5, rparam3_p1]
          circ1  = ParamCirc params1 qubits gates1
          circ2  = ParamCirc params1 qubits gates2

test27 = TestCase (assertEqual "reparameterize handles single parameters (1/2)."
                               (Just circ2)
                               (reparameterize [2, 3, 4] circ1))
    where gates1 = [plain1, plain2, iparam1_p3, plain3]
          gates2 = [plain1, plain2, iparam4_p3, plain3]
          circ1  = ParamCirc params3 qubits gates1
          circ2  = ParamCirc params3 qubits gates2

test28 = TestCase (assertEqual "reparameterize handles single parameters (2/2)."
                               (Just circ2)
                               (reparameterize [1 % 20, 1 % 3, 1 % 5] circ1))
    where gates1 = [plain4, plain2, iparam1_p3, plain5, rparam1_p2]
          gates2 = [plain4, plain2, rparam2_p3, plain5, rparam2_p2]
          circ1  = ParamCirc params3 qubits gates1
          circ2  = ParamCirc params3 qubits gates2
        
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
                                     TestLabel "circuitToLcd_Rational_P3_3" test18,
                                     TestLabel "reparameterize_OOB_Lt_1" test19,
                                     TestLabel "reparameterize_OOB_Lt_2" test20,
                                     TestLabel "reparameterize_OOB_Gt_1" test21,
                                     TestLabel "reparameterize_OOB_Gt_2" test22,
                                     TestLabel "reparameterize_Valid_Empty_P1" test23,
                                     TestLabel "reparameterize_Valid_Empty_P3" test24,
                                     TestLabel "reparameterize_Valid_P1_1" test25,
                                     TestLabel "reparameterize_Valid_P1_2" test26,
                                     TestLabel "reparameterize_Valid_P3_1" test27,
                                     TestLabel "reparameterize_Valid_P3_2" test28]

main = defaultMain tests

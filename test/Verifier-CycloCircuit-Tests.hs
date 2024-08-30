module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Complex.Cyclotomic as Cyclotomic
import Data.Ratio ((%))
import Pecac.Analyzer.Gate
import Pecac.Analyzer.Problem
import Pecac.Verifier.CycloCircuit
import Pecac.Verifier.Matrix as Matrix

-----------------------------------------------------------------------------------------
-- Cyclotomic Constants

zero :: Cyclotomic.Cyclotomic
zero = fromInteger 0

one :: Cyclotomic.Cyclotomic
one = fromInteger 1

two :: Cyclotomic.Cyclotomic
two = fromInteger 2

sqrt2 :: Cyclotomic.Cyclotomic
sqrt2 = Cyclotomic.sqrtInteger 2

img :: Cyclotomic.Cyclotomic
img = Cyclotomic.i

mat_I :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_I = Matrix.iden 2

mat_II :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_II = Matrix.kroneckerProduct mat_I mat_I

mat_III :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_III = Matrix.kroneckerProduct mat_II mat_I

-----------------------------------------------------------------------------------------
-- circToMat: Empty Circuits (Valid Parameters)

test1 = TestCase (assertEqual "circToMat handles empty 1-qubit, 1-parameter, circuits."
                              (Just mat_I)
                              (circToMat [0%1] pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 1) []

test2 = TestCase (assertEqual "circToMat handles empty 1-qubit, 2-parameter, circuits."
                              (Just mat_I)
                              (circToMat [0%1, 0%1] pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 1) []

test3 = TestCase (assertEqual "circToMat handles empty 1-qubit, 3-parameter, circuits."
                              (Just mat_I)
                              (circToMat [0%1, 0%1, 0%1] pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 3) (QubitReg "qs" 1) []

test4 = TestCase (assertEqual "circToMat handles empty 2-qubit, 1-parameter, circuits."
                              (Just mat_II)
                              (circToMat [0%1] pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 2) []

test5 = TestCase (assertEqual "circToMat handles empty 2-qubit, 2-parameter, circuits."
                              (Just mat_II)
                              (circToMat [0%1, 0%1] pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 2) []

test6 = TestCase (assertEqual "circToMat handles empty 3-qubit, 1-parameter, circuits."
                              (Just mat_III)
                              (circToMat [0%1] pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 3) []

test7 = TestCase (assertEqual "circToMat handles empty 3-qubit, 2-parameter, circuits."
                              (Just mat_III)
                              (circToMat [0%1, 0%1] pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 3) []

-----------------------------------------------------------------------------------------
-- circToMat: Empty Circuits (Invalid Parameters)

test8 = TestCase (assertEqual "circToMat rejects too few parameters."
                              Nothing
                              (circToMat [0%1, 0%1] pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 3) (QubitReg "qs" 1) []

test9 = TestCase (assertEqual "circToMat rejects too many parameters."
                              Nothing
                              (circToMat [0%1, 0%1, 0%1, 0%1] pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 3) (QubitReg "qs" 1) []

-----------------------------------------------------------------------------------------
-- circToMat: Single Gate Circuits

mat_X :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_X = Matrix.build [[zero, one],
                      [one,  zero]]

mat_Y :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_Y = Matrix.build [[zero, -img],
                      [img,  zero]]

mat_Z :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_Z = Matrix.build [[one,  zero],
                      [zero, -one]]

mat_rotx_deg45 :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_rotx_deg45 = Matrix.build [[one / sqrt2, img / sqrt2],
                               [img / sqrt2, one / sqrt2]]

test10 = TestCase (assertEqual "circToMat handles 1-qubit single gate circuits."
                               (Just mat_X)
                               (circToMat [0%1] pcirc))
    where gates = [PlainSummary GateX $ GateConfigs False [] [0]]
          pcirc = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 1) gates

test11 = TestCase (assertEqual "circToMat handles 2-qubit single gate circuits."
                               (Just $ Matrix.kroneckerProduct mat_I mat_Y)
                               (circToMat [0%1] pcirc))
    where gates = [PlainSummary GateY $ GateConfigs False [] [1]]
          pcirc = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 2) gates

test12 = TestCase (assertEqual "circToMat handles 3-qubit single gate circuits."
                               (Just $ Matrix.kroneckerProduct mat_II mat_Z)
                               (circToMat [0%1] pcirc))
    where gates = [PlainSummary GateZ $ GateConfigs False [] [2]]
          pcirc = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 3) gates

test13 = TestCase (assertEqual "circToMat handles rotations."
                               (Just mat_rotx_deg45)
                               (circToMat [45%360] pcirc))
    where gates = [RotSummary RotX [1] $ GateConfigs False [] [0]]
          pcirc = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 1) gates

-----------------------------------------------------------------------------------------
-- circToMat: Multi-Gate Circuits

mat_RxXYZ :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_RxXYZ = Matrix.kroneckerProduct mat_RxX mat_YZ
    where mat_RxX = Matrix.kroneckerProduct mat_rotx_deg45 mat_X
          mat_YZ  = Matrix.kroneckerProduct mat_Y mat_Z

test14 = TestCase (assertEqual "circToMat handles circuits with multiple gates."
                               (Just mat_RxXYZ)
                               (circToMat [45%(6*360), 45%(2*360)] pcirc))
    where gates = [PlainSummary GateZ $ GateConfigs False [] [0],
                   PlainSummary GateX $ GateConfigs False [] [1],
                   RotSummary RotX [-3, -1] $ GateConfigs False [] [0],
                   PlainSummary GateZ $ GateConfigs False [] [0],
                   PlainSummary GateY $ GateConfigs False [] [2],
                   PlainSummary GateZ $ GateConfigs False [] [3]]
          pcirc = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 4) gates

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "circToMat_Empty_Q1_P1" test1,
                                     TestLabel "circToMat_Empty_Q1_P2" test2,
                                     TestLabel "circToMat_Empty_Q1_P3" test3,
                                     TestLabel "circToMat_Empty_Q2_P1" test4,
                                     TestLabel "circToMat_Empty_Q2_P2" test5,
                                     TestLabel "circToMat_Empty_Q3_P1" test6,
                                     TestLabel "circToMat_Empty_Q3_P2" test7,
                                     TestLabel "circToMat_TooFewParams" test8,
                                     TestLabel "circToMat_TooManyParams" test9,
                                     TestLabel "circToMat_OneGate_Q1" test10,
                                     TestLabel "circToMat_OneGate_Q2" test11,
                                     TestLabel "circToMat_OneGate_Q3" test12,
                                     TestLabel "circToMat_OneRot" test13,
                                     TestLabel "circToMat_ManyGates" test14]

main = defaultMain tests

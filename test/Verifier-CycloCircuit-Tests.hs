module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Complex.Cyclotomic as Cyclotomic
import Data.Maybe
import Data.Ratio ((%))
import Pecac.Affine
import Pecac.Analyzer.Gate
import Pecac.Analyzer.Problem
import Pecac.Analyzer.Revolution
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

null_1angle :: [Revolution]
null_1angle = map rationalToRev [0%1]

null_2angle :: [Revolution]
null_2angle = map rationalToRev [0%1, 0%1]

test1 = TestCase (assertEqual "circToMat handles empty 1-qubit, 1-parameter, circuits."
                              (Just mat_I)
                              (circToMat null_1angle pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 1) []

test2 = TestCase (assertEqual "circToMat handles empty 1-qubit, 2-parameter, circuits."
                              (Just mat_I)
                              (circToMat null_2angle pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 1) []

test3 = TestCase (assertEqual "circToMat handles empty 1-qubit, 3-parameter, circuits."
                              (Just mat_I)
                              (circToMat angles pcirc))
    where angles = map rationalToRev [0%1, 0%1, 0%1]
          pcirc  = ParamCirc (ParamArr "thetas" 3) (QubitReg "qs" 1) []

test4 = TestCase (assertEqual "circToMat handles empty 2-qubit, 1-parameter, circuits."
                              (Just mat_II)
                              (circToMat null_1angle pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 2) []

test5 = TestCase (assertEqual "circToMat handles empty 2-qubit, 2-parameter, circuits."
                              (Just mat_II)
                              (circToMat null_2angle pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 2) []

test6 = TestCase (assertEqual "circToMat handles empty 3-qubit, 1-parameter, circuits."
                              (Just mat_III)
                              (circToMat null_1angle pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 3) []

test7 = TestCase (assertEqual "circToMat handles empty 3-qubit, 2-parameter, circuits."
                              (Just mat_III)
                              (circToMat null_2angle pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 3) []

-----------------------------------------------------------------------------------------
-- circToMat: Empty Circuits (Invalid Parameters)

pcirc1 :: ParamCirc
pcirc1 = ParamCirc (ParamArr "thetas" 3) (QubitReg "qs" 1) []

test8 = TestCase (assertEqual "circToMat rejects too few parameters."
                              Nothing
                              (circToMat null_2angle pcirc1))

test9 = TestCase (assertEqual "circToMat rejects too many parameters."
                              Nothing
                              (circToMat angles pcirc))
    where angles = map rationalToRev [0%1, 0%1, 0%1, 0%1]
          pcirc  = ParamCirc (ParamArr "thetas" 3) (QubitReg "qs" 1) []

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

pcirc2 :: ParamCirc
pcirc2 = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 1) gates
    where gates = [PlainSummary GateX $ GateConfigs False [] [0]]

pcirc3 :: ParamCirc
pcirc3 = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 2) gates
    where gates = [PlainSummary GateY $ GateConfigs False [] [1]]

pcirc4 :: ParamCirc
pcirc4 = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 1) gates
    where aff   = linear [1]
          gates = [RotSummary RotX aff $ GateConfigs False [] [0]]

test10 = TestCase (assertEqual "circToMat handles 1-qubit single gate circuits."
                               (Just mat_X)
                               (circToMat null_1angle pcirc2))

test11 = TestCase (assertEqual "circToMat handles 2-qubit single gate circuits."
                               (Just $ Matrix.kroneckerProduct mat_I mat_Y)
                               (circToMat null_1angle pcirc3))

test12 = TestCase (assertEqual "circToMat handles 3-qubit single gate circuits."
                               (Just $ Matrix.kroneckerProduct mat_II mat_Z)
                               (circToMat null_1angle pcirc))
    where gates = [PlainSummary GateZ $ GateConfigs False [] [2]]
          pcirc = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 3) gates

test13 = TestCase (assertEqual "circToMat handles rotations."
                               (Just mat_rotx_deg45)
                               (circToMat angles pcirc4))
    where angles = map rationalToRev [-2 * 45 % 360]

-----------------------------------------------------------------------------------------
-- circToMat: Multi-Gate Circuits

mat_RxXYZ :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_RxXYZ = Matrix.kroneckerProduct mat_RxX mat_YZ
    where mat_RxX = Matrix.kroneckerProduct mat_rotx_deg45 mat_X
          mat_YZ  = Matrix.kroneckerProduct mat_Y mat_Z

test14 = TestCase (assertEqual "circToMat handles circuits with multiple gates."
                               (Just mat_RxXYZ)
                               (circToMat angles pcirc))
    where angles = map rationalToRev [-2 * 45 % (6 * 360), -2 * 45 % (2 * 360)]
          aff    = linear [-3, -1]
          gates  = [PlainSummary GateZ $ GateConfigs False [] [0],
                    PlainSummary GateX $ GateConfigs False [] [1],
                    RotSummary RotX aff $ GateConfigs False [] [0],
                    PlainSummary GateZ $ GateConfigs False [] [0],
                    PlainSummary GateY $ GateConfigs False [] [2],
                    PlainSummary GateZ $ GateConfigs False [] [3]]
          pcirc = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 4) gates

-----------------------------------------------------------------------------------------
-- findGlobalPhase

pcirc5 :: ParamCirc
pcirc5 = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 1) gates
    where gates = [PlainSummary GateZ $ GateConfigs False [] [0],
                   PlainSummary GateX $ GateConfigs False [] [0],
                   PlainSummary GateZ $ GateConfigs False [] [0]]

test15 = TestCase (assertEqual "findGlobalPhase rejects circuits of distinct qubit cts."
                               Nothing
                               (findGlobalPhase pcirc2 pcirc3))

test16 = TestCase (assertEqual "findGlobalPhase rejects circuits of distinct param cts."
                               Nothing
                               (findGlobalPhase pcirc2 pcirc1))

test17 = TestCase (assertEqual "findGlobalPhase detects circuits differing beyond phase."
                               Nothing
                               (findGlobalPhase pcirc2 pcirc4))

test18 = TestCase (assertEqual "findGlobalPhase handles equal circuits."
                               (Just $ one)
                               (findGlobalPhase pcirc2 pcirc2))

test19 = TestCase (assertEqual "findGlobalPhase can compute global phase."
                               (Just $ -one)
                               (findGlobalPhase pcirc2 pcirc5))

-----------------------------------------------------------------------------------------
-- findLinearPhase

pcirc6 :: ParamCirc
pcirc6 = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 3) gates
    where const = lit $ rationalToRev $ 7 % 11
          gates = [RotSummary GPhase (linear [3, 0]) $ GateConfigs False [] [],
                   RotSummary GPhase (linear [0, 1]) $ GateConfigs False [] [],
                   RotSummary GPhase const $ GateConfigs False [] []]

pcirc7 :: ParamCirc
pcirc7 = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 3) gates
    where const = lit $ rationalToRev $ 7 % 11
          gates = [RotSummary GPhase (linear [7]) $ GateConfigs False [] [],
                   PlainSummary GateX $ GateConfigs False [] [1],
                   RotSummary GPhase const $ GateConfigs False [] []]

pcirc8 :: ParamCirc
pcirc8 = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 3) gates
    where const = lit $ rationalToRev $ 7 % 11
          gates = [RotSummary GPhase (linear [10]) $ GateConfigs False [] [],
                   PlainSummary GateX $ GateConfigs False [] [1],
                   RotSummary GPhase const $ GateConfigs False [] []]

pcirc9 :: ParamCirc
pcirc9 = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 3) gates
    where const = lit $ rationalToRev $ 7 % 11
          gates = [RotSummary GPhase (linear [-5, 0]) $ GateConfigs False [] [],
                   RotSummary GPhase (linear [0, -12]) $ GateConfigs False [] [],
                   RotSummary GPhase const $ GateConfigs False [] []]

pcirc10 :: ParamCirc
pcirc10 = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 3) gates
    where const = lit $ rationalToRev $ 7 % 11
          gates = [RotSummary GPhase (linear [35, 0]) $ GateConfigs False [] [],
                   RotSummary GPhase (linear [-2, 0]) $ GateConfigs False [] [],
                   RotSummary GPhase (linear [0, -6]) $ GateConfigs False [] [],
                   RotSummary RotX (linear [-3, -1]) $ GateConfigs False [] [0],
                   RotSummary GPhase const $ GateConfigs False [] []]

pcirc11 :: ParamCirc
pcirc11 = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 3) gates
    where const = lit $ rationalToRev $ 7 % 11
          gates = [RotSummary GPhase const $ GateConfigs False [] [],
                   RotSummary RotX (linear [-3, -1]) $ GateConfigs False [] [0]]

test20 = TestCase (assertEqual "findLinearPhase rejects circuits of distinct qubit cts."
                               InferenceFailure
                               (findLinearPhase pcirc2 pcirc3))

test21 = TestCase (assertEqual "findLinearPhase rejects circuits of distinct param cts."
                               CutoffFailure
                               (findLinearPhase pcirc2 pcirc1))

test22 = TestCase (assertEqual "findLinearPhase detects circuits differing beyond phase."
                               InferenceFailure
                               (findLinearPhase pcirc2 pcirc4))

test23 = TestCase (assertEqual "findLinearPhase handles 1 parameter, equal circuits."
                               (LinearCoeffs [0])
                               (findLinearPhase pcirc2 pcirc2))

test24 = TestCase (assertEqual "findLinearPhase handles 2 parameter, equal circuits."
                               (LinearCoeffs [0, 0])
                               (findLinearPhase pcirc pcirc))
    where pcirc = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 1) []

test25 = TestCase (assertEqual "findLinearPhase handles 1 parameter, constant phase."
                               (LinearCoeffs [0])
                               (findLinearPhase pcirc2 pcirc5))

test26 = TestCase (assertEqual "findLinearPhase handles 2 parameter, pos linear phase."
                               (LinearCoeffs [3, 1])
                               (findLinearPhase pcirc pcirc6))
    where pcirc = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 3) []

test27 = TestCase (assertEqual "findLinearPhase handles 1 parameter, pos linear phase."
                               (LinearCoeffs [3])
                               (findLinearPhase pcirc7 pcirc8))

test28 = TestCase (assertEqual "findLinearPhase handles 2 parameter, neg linear phase."
                               (LinearCoeffs [-5, -12])
                               (findLinearPhase pcirc pcirc9))
    where pcirc = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 3) []

test29 = TestCase (assertEqual "findLinearPhase handles 2 parameter, mixed linear phase."
                               (LinearCoeffs [33, -6])
                               (findLinearPhase pcirc11 pcirc10))
    where pcirc = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 3) []

-----------------------------------------------------------------------------------------
-- precomputeMat: Empty Circuits (Valid Parameters)

test30 = TestCase (assertEqual "precomputeMat handles empty 1-qubit, 1-param, circuits."
                               (Just mat_I)
                               (precomputeMat pcirc null_1angle))
    where pcirc = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 1) []

test31 = TestCase (assertEqual "precomputeMat handles empty 1-qubit, 2-param, circuits."
                               (Just mat_I)
                               (precomputeMat pcirc null_2angle))
    where pcirc = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 1) []

test32 = TestCase (assertEqual "precomputeMat handles empty 1-qubit, 3-param, circuits."
                               (Just mat_I)
                               (precomputeMat pcirc angles))
    where angles = map rationalToRev [0%1, 0%1, 0%1]
          pcirc  = ParamCirc (ParamArr "thetas" 3) (QubitReg "qs" 1) []

test33 = TestCase (assertEqual "precomputeMat handles empty 2-qubit, 1-param, circuits."
                               (Just mat_II)
                               (precomputeMat pcirc null_1angle))
    where pcirc = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 2) []

test34 = TestCase (assertEqual "precomputeMat handles empty 2-qubit, 2-param, circuits."
                               (Just mat_II)
                               (precomputeMat pcirc null_2angle))
    where pcirc = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 2) []

test35 = TestCase (assertEqual "precomputeMat handles empty 3-qubit, 1-param, circuits."
                               (Just mat_III)
                               (precomputeMat pcirc null_1angle))
    where pcirc = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 3) []

test36 = TestCase (assertEqual "precomputeMat handles empty 3-qubit, 2-param, circuits."
                               (Just mat_III)
                               (precomputeMat pcirc null_2angle))
    where pcirc = ParamCirc (ParamArr "thetas" 2) (QubitReg "qs" 3) []

-----------------------------------------------------------------------------------------
-- precomputeMat: Empty Circuits (Invalid Parameters)

test37 = TestCase (assertEqual "precomputeMat rejects too few parameters."
                               Nothing
                               (precomputeMat pcirc1 null_2angle))

test38 = TestCase (assertEqual "precomputeMat rejects too many parameters."
                               Nothing
                               (precomputeMat pcirc angles))
    where angles = map rationalToRev [0%1, 0%1, 0%1, 0%1]
          pcirc  = ParamCirc (ParamArr "thetas" 3) (QubitReg "qs" 1) []

-----------------------------------------------------------------------------------------
-- precomputeMat: Single Gate Circuits

test39 = TestCase (assertEqual "precomputeMat handles 1-qubit single gate circuits."
                               (Just mat_X)
                               (precomputeMat pcirc2 null_1angle))

test40 = TestCase (assertEqual "precomputeMat handles 2-qubit single gate circuits."
                               (Just $ Matrix.kroneckerProduct mat_I mat_Y)
                               (precomputeMat pcirc3 null_1angle))

test41 = TestCase (assertEqual "precomputeMat handles 3-qubit single gate circuits."
                               (Just $ Matrix.kroneckerProduct mat_II mat_Z)
                               (precomputeMat pcirc null_1angle))
    where gates = [PlainSummary GateZ $ GateConfigs False [] [2]]
          pcirc = ParamCirc (ParamArr "thetas" 1) (QubitReg "qs" 3) gates

test42 = TestCase (assertEqual "precomputeMat handles rotations."
                               (Just mat_rotx_deg45)
                               (precomputeMat pcirc4 angles))
    where angles = map rationalToRev [-2 * 45 % 360]

-----------------------------------------------------------------------------------------
-- precomputeMat: Multi-Gate Circuits

test43 = TestCase (assertEqual "precomputeMat handles circuits with multiple gates."
                               (Just mat_RxXYZ)
                               (precomputeMat pcirc angles))
    where angles = map rationalToRev [-2 * 45 % (6 * 360), -2 * 45 % (2 * 360)]
          aff    = linear [-3, -1]
          gates  = [PlainSummary GateZ $ GateConfigs False [] [0],
                    PlainSummary GateX $ GateConfigs False [] [1],
                    RotSummary RotX aff $ GateConfigs False [] [0],
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
                                     TestLabel "circToMat_ManyGates" test14,
                                     TestLabel "findGlobalPhase_QubitMismatch" test15,
                                     TestLabel "findGlobalPhase_ParamMismatch" test16,
                                     TestLabel "findGlobalPhase_NoSolution" test17,
                                     TestLabel "findGlobalPhase_EqualCircs" test18,
                                     TestLabel "findGlobalPhase_ConstPhase" test19,
                                     TestLabel "findLinearPhase_QubitMismatch" test20,
                                     TestLabel "findLinearPhase_ParamMismatch" test21,
                                     TestLabel "findLinearPhase_NoSolution" test22,
                                     TestLabel "findLinearPhase_OneParam_Equal" test23,
                                     TestLabel "findLinearPhase_TwoParam_Equal" test24,
                                     TestLabel "findLinearPhase_OneParam_Const" test25,
                                     TestLabel "findLinearPhase_TwoParam_Affine" test26,
                                     TestLabel "findLinearPhase_OneParam_Affine" test27,
                                     TestLabel "findLinearPhase_TwoParam_Neg" test28,
                                     TestLabel "findLinearPhase_TwoParam_Mixed" test29,
                                     TestLabel "precomputeMat_Empty_Q1_P1" test30,
                                     TestLabel "precomputeMat_Empty_Q1_P2" test31,
                                     TestLabel "precomputeMat_Empty_Q1_P3" test32,
                                     TestLabel "precomputeMat_Empty_Q2_P1" test33,
                                     TestLabel "precomputeMat_Empty_Q2_P2" test34,
                                     TestLabel "precomputeMat_Empty_Q3_P1" test35,
                                     TestLabel "precomputeMat_Empty_Q3_P2" test36,
                                     TestLabel "precomputeMat_TooFewParams" test37,
                                     TestLabel "precomputeMat_TooManyParams" test38,
                                     TestLabel "precomputeMat_OneGate_Q1" test39,
                                     TestLabel "precomputeMat_OneGate_Q2" test40,
                                     TestLabel "precomputeMat_OneGate_Q3" test41,
                                     TestLabel "precomputeMat_OneRot" test42,
                                     TestLabel "precomputeMat_ManyGates" test43]

main = defaultMain tests

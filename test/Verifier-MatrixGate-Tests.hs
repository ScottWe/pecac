module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Pecac.Verifier.Matrix as Matrix
import Pecac.Verifier.MatrixGate

-----------------------------------------------------------------------------------------
-- applyMatrixBetween

mat_X :: Matrix.Matrix Int
mat_X = Matrix.build [[0, 1],
                      [1, 0]]

mat_Z :: Matrix.Matrix Int
mat_Z = Matrix.build [[1, 0],
                      [0, -1]]

mat_I :: Matrix.Matrix Int
mat_I = Matrix.iden 2

mat_IX :: Matrix.Matrix Int
mat_IX = Matrix.kroneckerProduct mat_I mat_X

mat_XI :: Matrix.Matrix Int
mat_XI = Matrix.kroneckerProduct mat_X mat_I

mat_IIX :: Matrix.Matrix Int
mat_IIX = Matrix.kroneckerProduct mat_I mat_IX

mat_IXI :: Matrix.Matrix Int
mat_IXI = Matrix.kroneckerProduct mat_IX mat_I

mat_XII :: Matrix.Matrix Int
mat_XII = Matrix.kroneckerProduct mat_XI mat_I

mat_IIIX :: Matrix.Matrix Int
mat_IIIX = Matrix.kroneckerProduct mat_I mat_IIX

mat_IIXI :: Matrix.Matrix Int
mat_IIXI = Matrix.kroneckerProduct mat_I mat_IXI

mat_IXII :: Matrix.Matrix Int
mat_IXII = Matrix.kroneckerProduct mat_I mat_XII

mat_XIII :: Matrix.Matrix Int
mat_XIII = Matrix.kroneckerProduct mat_XII mat_I

test1 = TestCase (assertEqual "applyMatrixBetween is the identity when n = m = 0 (1/2)."
                              mat_X
                              (applyMatrixBetween 0 0 mat_X))

test2 = TestCase (assertEqual "applyMatrixBetween is the identity when n = m = 0 (2/2)."
                              mat_Z
                              (applyMatrixBetween 0 0 mat_Z))

test3 = TestCase (assertEqual "applyMatrixBetween works when n = 0 and m = 1."
                              mat_XI
                              (applyMatrixBetween 0 1 mat_X))

test4 = TestCase (assertEqual "applyMatrixBetween works when n = 1 and m = 0."
                              mat_IX
                              (applyMatrixBetween 1 0 mat_X))

test5 = TestCase (assertEqual "applyMatrixBetween works when n = 0 and m = 2."
                              mat_XII
                              (applyMatrixBetween 0 2 mat_X))

test6 = TestCase (assertEqual "applyMatrixBetween works when n = 1 and m = 1."
                              mat_IXI
                              (applyMatrixBetween 1 1 mat_X))

test7 = TestCase (assertEqual "applyMatrixBetween works when n = 2 and m = 0."
                              mat_IIX
                              (applyMatrixBetween 2 0 mat_X))

test8 = TestCase (assertEqual "applyMatrixBetween works when n = 0 and m = 3."
                              mat_XIII
                              (applyMatrixBetween 0 3 mat_X))

test9 = TestCase (assertEqual "applyMatrixBetween works when n = 1 and m = 2."
                              mat_IXII
                              (applyMatrixBetween 1 2 mat_X))

test10 = TestCase (assertEqual "applyMatrixBetween works when n = 2 and m = 1."
                               mat_IIXI
                               (applyMatrixBetween 2 1 mat_X))

test11 = TestCase (assertEqual "applyMatrixBetween works when n = 3 and m = 0."
                               mat_IIIX
                               (applyMatrixBetween 3 0 mat_X))

-----------------------------------------------------------------------------------------
-- addCtrlToMatrix

mat_CX :: Matrix.Matrix Int
mat_CX = Matrix.build [[1, 0, 0, 0],
                       [0, 1, 0, 0],
                       [0, 0, 0, 1],
                       [0, 0, 1, 0]]

mat_CSwap :: Matrix.Matrix Int
mat_CSwap = Matrix.build [[1, 0, 0, 0, 0, 0, 0, 0],
                          [0, 1, 0, 0, 0, 0, 0, 0],
                          [0, 0, 1, 0, 0, 0, 0, 0],
                          [0, 0, 0, 1, 0, 0, 0, 0],
                          [0, 0, 0, 0, 1, 0, 0, 0],
                          [0, 0, 0, 0, 0, 0, 1, 0],
                          [0, 0, 0, 0, 0, 1, 0, 0],
                          [0, 0, 0, 0, 0, 0, 0, 1]]

test12 = TestCase (assertEqual "addCtrlToMatrix works on 2x2 matrices."
                               mat_CX
                               (addCtrlToMatrix mat_X))

test13 = TestCase (assertEqual "addCtrlToMatrix works on 4x4 matrices."
                               mat_CSwap
                               (addCtrlToMatrix swap))

-----------------------------------------------------------------------------------------
-- addNegCtrlToMatrix

mat_NX :: Matrix.Matrix Int
mat_NX = Matrix.build [[0, 1, 0, 0],
                       [1, 0, 0, 0],
                       [0, 0, 1, 0],
                       [0, 0, 0, 1]]

mat_NSwap :: Matrix.Matrix Int
mat_NSwap = Matrix.build [[1, 0, 0, 0, 0, 0, 0, 0],
                          [0, 0, 1, 0, 0, 0, 0, 0],
                          [0, 1, 0, 0, 0, 0, 0, 0],
                          [0, 0, 0, 1, 0, 0, 0, 0],
                          [0, 0, 0, 0, 1, 0, 0, 0],
                          [0, 0, 0, 0, 0, 1, 0, 0],
                          [0, 0, 0, 0, 0, 0, 1, 0],
                          [0, 0, 0, 0, 0, 0, 0, 1]]

test14 = TestCase (assertEqual "addNegCtrlToMatrix works on 2x2 matrices."
                               mat_NX
                               (addNegCtrlToMatrix mat_X))

test15 = TestCase (assertEqual "addNegCtrlToMatrix works on 4x4 matrices."
                               mat_NSwap
                               (addNegCtrlToMatrix swap))

-----------------------------------------------------------------------------------------
-- swapAndApply

mat_H :: Matrix.Matrix Int
mat_H = Matrix.build [[1,  1],
                      [1, -1]]

mat_XZH :: Matrix.Matrix Int
mat_XZH = Matrix.kroneckerProduct mat_X $ Matrix.kroneckerProduct mat_Z mat_H

mat_ZXH :: Matrix.Matrix Int
mat_ZXH = Matrix.kroneckerProduct mat_Z $ Matrix.kroneckerProduct mat_X mat_H

mat_HZX :: Matrix.Matrix Int
mat_HZX = Matrix.kroneckerProduct mat_H $ Matrix.kroneckerProduct mat_Z mat_X

mat_XHZ :: Matrix.Matrix Int
mat_XHZ = Matrix.kroneckerProduct mat_X $ Matrix.kroneckerProduct mat_H mat_Z

mat_HXZH :: Matrix.Matrix Int
mat_HXZH = Matrix.kroneckerProduct mat_H mat_XZH

mat_HZXH :: Matrix.Matrix Int
mat_HZXH = Matrix.kroneckerProduct mat_H mat_ZXH

test16 = TestCase (assertEqual "swapAndApply is the identity when a = b = 0 with k = 3."
                               mat_XZH
                               (swapAndApply 3 0 0 mat_XZH))

test17 = TestCase (assertEqual "swapAndApply is the identity when a = b = 1 with k = 3."
                               mat_XZH
                               (swapAndApply 3 1 1 mat_XZH))

test18 = TestCase (assertEqual "swapAndApply is the identity when a = b = 2 with k = 3."
                               mat_XZH
                               (swapAndApply 3 2 2 mat_XZH))

test19 = TestCase (assertEqual "swapAndApply works when a = 0 and b = 1 with k = 3."
                               mat_ZXH
                               (swapAndApply 3 0 1 mat_XZH))

test20 = TestCase (assertEqual "swapAndApply works when a = 1 and b = 0 with k = 3."
                               mat_ZXH
                               (swapAndApply 3 1 0 mat_XZH))

test21 = TestCase (assertEqual "swapAndApply works when a = 0 and b = 2 with k = 3."
                               mat_HZX
                               (swapAndApply 3 0 2 mat_XZH))

test22 = TestCase (assertEqual "swapAndApply works when a = 2 and b = 0 with k = 3."
                               mat_HZX
                               (swapAndApply 3 2 0 mat_XZH))

test23 = TestCase (assertEqual "swapAndApply works when a = 1 and b = 2 with k = 3."
                               mat_XHZ
                               (swapAndApply 3 1 2 mat_XZH))

test24 = TestCase (assertEqual "swapAndApply works when a = 2 and b = 1 with k = 3."
                               mat_XHZ
                               (swapAndApply 3 2 1 mat_XZH))

test25 = TestCase (assertEqual "swapAndApply works when a = 1 and b = 2 with k = 4."
                               mat_HZXH
                               (swapAndApply 4 2 1 mat_HXZH))

-----------------------------------------------------------------------------------------
-- applyAt

mat_ZHX :: Matrix.Matrix Int
mat_ZHX = Matrix.kroneckerProduct mat_Z $ Matrix.kroneckerProduct mat_H mat_X

mat_HXZ :: Matrix.Matrix Int
mat_HXZ = Matrix.kroneckerProduct mat_H $ Matrix.kroneckerProduct mat_X mat_Z

mat_IHIXIIZI :: Matrix.Matrix Int
mat_IHIXIIZI = Matrix.kroneckerProduct mat_IHIXII mat_ZI
    where mat_IH     = Matrix.kroneckerProduct (Matrix.iden 2) mat_H
          mat_IHIXII = Matrix.kroneckerProduct mat_IH mat_IXII
          mat_ZI     = Matrix.kroneckerProduct mat_Z $ Matrix.iden 2

test26 = TestCase (assertEqual "applyAt can generate all permutations of a gate (1/6)."
                               mat_XZH
                               (applyAt 3 [0, 1, 2] mat_XZH))

test27 = TestCase (assertEqual "applyAt can generate all permutations of a gate (2/6)."
                               mat_XHZ
                               (applyAt 3 [0, 2, 1] mat_XZH))

test28 = TestCase (assertEqual "applyAt can generate all permutations of a gate (3/6)."
                               mat_ZXH
                               (applyAt 3 [1, 0, 2] mat_XZH))

test29 = TestCase (assertEqual "applyAt can generate all permutations of a gate (4/6)."
                               mat_ZHX
                               (applyAt 3 [2, 0, 1] mat_XZH))

test30 = TestCase (assertEqual "applyAt can generate all permutations of a gate (5/6)."
                               mat_HXZ
                               (applyAt 3 [1, 2, 0] mat_XZH))

test31 = TestCase (assertEqual "applyAt can generate all permutations of a gate (6/6)."
                               mat_HZX
                               (applyAt 3 [2, 1, 0] mat_XZH))

test32 = TestCase (assertEqual "applyAt can apply gates within a larger system."
                               mat_IHIXIIZI
                               (applyAt 8 [3, 6, 1] mat_XZH))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "applyMatrixBetween_n0_m0_1" test1,
                                     TestLabel "applyMatrixBetween_n0_m0_2" test2,
                                     TestLabel "applyMatrixBetween_n0_m1" test3,
                                     TestLabel "applyMatrixBetween_n1_m0" test4,
                                     TestLabel "applyMatrixBetween_n0_m2" test5,
                                     TestLabel "applyMatrixBetween_n1_m1" test6,
                                     TestLabel "applyMatrixBetween_n2_m0" test7,
                                     TestLabel "applyMatrixBetween_n0_m3" test8,
                                     TestLabel "applyMatrixBetween_n1_m2" test9,
                                     TestLabel "applyMatrixBetween_n2_m1" test10,
                                     TestLabel "applyMatrixBetween_n3_m0" test11,
                                     TestLabel "addCtrlToMatrix_2x2" test12,
                                     TestLabel "addCtrlToMatrix_4x4" test13,
                                     TestLabel "addNegCtrlToMatrix_2x2" test14,
                                     TestLabel "addNegCtrlToMatrix_4x4" test15,
                                     TestLabel "swapAndApply_a0_b0_k3" test16,
                                     TestLabel "swapAndApply_a1_b1_k3" test17,
                                     TestLabel "swapAndApply_a2_b2_k3" test18,
                                     TestLabel "swapAndApply_a0_b1_k3" test19,
                                     TestLabel "swapAndApply_a1_b0_k3" test20,
                                     TestLabel "swapAndApply_a0_b2_k3" test21,
                                     TestLabel "swapAndApply_a2_b0_k3" test22,
                                     TestLabel "swapAndApply_a1_b2_k3" test23,
                                     TestLabel "swapAndApply_a2_b1_k3" test24,
                                     TestLabel "swapAndApply_a1_b2_k4" test25,
                                     TestLabel "applyAt_NoPad_1" test26,
                                     TestLabel "applyAt_NoPad_2" test27,
                                     TestLabel "applyAt_NoPad_3" test28,
                                     TestLabel "applyAt_NoPad_4" test29,
                                     TestLabel "applyAt_NoPad_5" test30,
                                     TestLabel "applyAt_NoPad_6" test31,
                                     TestLabel "applyAt_Padded" test32]

main = defaultMain tests

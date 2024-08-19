module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Ratio ((%))
import Data.Matrix as DMatrix
import Pecac.Verifier.Matrix as PMatrix

-----------------------------------------------------------------------------------------
-- Basic Vectors.

basis_dim1_Z_1 :: DMatrix.Matrix Int
basis_dim1_Z_1 = DMatrix.fromLists [[1]]

basis_dim2_Q_1 :: DMatrix.Matrix Rational
basis_dim2_Q_1 = DMatrix.fromLists [[1%1], [0%1]]

basis_dim2_Q_2 :: DMatrix.Matrix Rational
basis_dim2_Q_2 = DMatrix.fromLists [[0%1], [1%1]]

basis_dim3_Q_1 :: DMatrix.Matrix Rational
basis_dim3_Q_1 = DMatrix.fromLists [[1%1], [0%1], [0%1]]

basis_dim3_Q_2 :: DMatrix.Matrix Rational
basis_dim3_Q_2 = DMatrix.fromLists [[0%1], [1%1], [0%1]]

basis_dim3_Q_3 :: DMatrix.Matrix Rational
basis_dim3_Q_3 = DMatrix.fromLists [[0%1], [0%1], [1%1]]

basis_dim4_Q_1 :: DMatrix.Matrix Rational
basis_dim4_Q_1 = DMatrix.fromLists [[1%1], [0%1], [0%1], [0%1]]

basis_dim4_Q_2 :: DMatrix.Matrix Rational
basis_dim4_Q_2 = DMatrix.fromLists [[0%1], [1%1], [0%1], [0%1]]

basis_dim4_Q_3 :: DMatrix.Matrix Rational
basis_dim4_Q_3 = DMatrix.fromLists [[0%1], [0%1], [1%1], [0%1]]

basis_dim4_Q_4 :: DMatrix.Matrix Rational
basis_dim4_Q_4 = DMatrix.fromLists [[0%1], [0%1], [0%1], [1%1]]

-----------------------------------------------------------------------------------------
-- Function: build.

mat1 :: PMatrix.Matrix Int
mat1 = PMatrix.build [[5]]

mat1_img1 :: PMatrix.Matrix Int
mat1_img1 = DMatrix.fromLists [[5]]

mat2 :: PMatrix.Matrix Rational
mat2 = PMatrix.build [[1%2, 1%3,  1%5],
                      [1%7, 1%11, 1%13]]

mat2_img1 :: PMatrix.Matrix Rational
mat2_img1 = DMatrix.fromLists [[1%2], [1%7]]

mat2_img2 :: PMatrix.Matrix Rational
mat2_img2 = DMatrix.fromLists [[1%3], [1%11]]

mat2_img3 :: PMatrix.Matrix Rational
mat2_img3 = DMatrix.fromLists [[1%5], [1%13]]

mat3 :: PMatrix.Matrix Rational
mat3 = PMatrix.build [[1%2,  1%3],
                      [1%5,  1%7],
                      [1%11, 1%13]]

mat3_img1 :: PMatrix.Matrix Rational
mat3_img1 = DMatrix.fromLists [[1%2], [1%5], [1%11]]

mat3_img2 :: PMatrix.Matrix Rational
mat3_img2 = DMatrix.fromLists [[1%3], [1%7], [1%13]]

test1 = TestCase (assertEqual "A 1x1 Z-matrix acts correctly on e1 for Z^1."
                              mat1_img1
                              (mat1 * basis_dim1_Z_1))

test2 = TestCase (assertEqual "A 2x3 Q-matrix acts correctly on e1 for Q^3."
                              mat2_img1
                              (mat2 * basis_dim3_Q_1))

test3 = TestCase (assertEqual "A 2x3 Q-matrix acts correctly on e2 for Q^3."
                              mat2_img2
                              (mat2 * basis_dim3_Q_2))

test4 = TestCase (assertEqual "A 2x3 Q-matrix acts correctly on e3 for Q^3."
                              mat2_img3
                              (mat2 * basis_dim3_Q_3))

test5 = TestCase (assertEqual "A 3x2 Q-matrix acts correctly on e1 for Q^2."
                              mat3_img1
                              (mat3 * basis_dim2_Q_1))

test6 = TestCase (assertEqual "A 3x2 Q-matrix acts correctly on e3 for Q^2."
                              mat3_img2
                              (mat3 * basis_dim2_Q_2))

-----------------------------------------------------------------------------------------
-- Function: iden.

iden_Z_1x1 :: DMatrix.Matrix Int
iden_Z_1x1 = PMatrix.iden 1

iden_Q_2x2 :: DMatrix.Matrix Rational
iden_Q_2x2 = PMatrix.iden 2

iden_Q_3x3 :: DMatrix.Matrix Rational
iden_Q_3x3 = PMatrix.iden 3

test7 = TestCase (assertEqual "The 1x1 Z-identity acts correctly on e1 for Z^1."
                              basis_dim1_Z_1
                              (iden_Z_1x1 * basis_dim1_Z_1))

test8 = TestCase (assertEqual "The 2x2 Q-identity acts correctly on e1 for Q^3."
                              basis_dim2_Q_1
                              (iden_Q_2x2 * basis_dim2_Q_1))

test9 = TestCase (assertEqual "The 2x2 Q-identity acts correctly on e2 for Q^3."
                              basis_dim2_Q_2
                              (iden_Q_2x2 * basis_dim2_Q_2))

test10 = TestCase (assertEqual "The 3x3 Q-identity acts correctly on e3 for Q^3."
                               basis_dim3_Q_1
                               (iden_Q_3x3 * basis_dim3_Q_1))

test11 = TestCase (assertEqual "The 3x3 Q-identity acts correctly on e1 for Q^2."
                               basis_dim3_Q_2
                               (iden_Q_3x3 * basis_dim3_Q_2))

test12 = TestCase (assertEqual "The 3x3 Q-identity acts correctly on e3 for Q^2."
                               basis_dim3_Q_3
                               (iden_Q_3x3 * basis_dim3_Q_3))

-----------------------------------------------------------------------------------------
-- Function: swap.

test13 = TestCase (assertEqual "The 3x3 Q-identity acts correctly on e3 for Q^3."
                               basis_dim4_Q_1
                               (PMatrix.swap * basis_dim4_Q_1))

test14 = TestCase (assertEqual "The 3x3 Q-identity acts correctly on e3 for Q^3."
                               basis_dim4_Q_3
                               (PMatrix.swap * basis_dim4_Q_2))

test15 = TestCase (assertEqual "The 3x3 Q-identity acts correctly on e3 for Q^3."
                               basis_dim4_Q_2
                               (PMatrix.swap * basis_dim4_Q_3))

test16 = TestCase (assertEqual "The 3x3 Q-identity acts correctly on e3 for Q^3."
                               basis_dim4_Q_4
                               (PMatrix.swap * basis_dim4_Q_4))

-----------------------------------------------------------------------------------------
-- Function: directSum.

sum1 :: DMatrix.Matrix Int
sum1 = DMatrix.fromLists [[5, 0, 0],
                          [0, 1, 0],
                          [0, 0, 1]]

sum2 :: DMatrix.Matrix Rational
sum2 = DMatrix.fromLists [[1%2, 1%3,  1%5,  0%1],
                          [1%7, 1%11, 1%13, 0%1],
                          [0%1, 0%1,  0%1,  1%2],
                          [0%1, 0%1,  0%1,  1%7]]

test17 = TestCase (assertEqual "Tests the direct sum of diagonal matrices"
                               sum1
                               (PMatrix.directSum mat1 $ PMatrix.iden 2))

test18 = TestCase (assertEqual "Tests the direct sum of non-square matrices"
                               sum2
                               (PMatrix.directSum mat2 mat2_img1))

-----------------------------------------------------------------------------------------
-- Function: kroneckerProduct.

prod1 :: DMatrix.Matrix Int
prod1 = DMatrix.fromLists [[5, 0],
                           [0, 5]]

prod2 :: DMatrix.Matrix Rational
prod2 = DMatrix.fromLists [[1%4,  1%6,  1%10],
                           [1%14, 1%21, 1%35],
                           [1%14, 1%22, 1%26],
                           [1%49, 1%77, 1%91]]

prod3 :: DMatrix.Matrix Rational
prod3 = DMatrix.fromLists [[1%2, 1%3,  1%5,  0%1, 0%1,  0%1],
                           [1%7, 1%11, 1%13, 0%1, 0%1,  0%1],
                           [0%1, 0%1,  0%1,  1%2, 1%3,  1%5],
                           [0%1, 0%1,  0%1,  1%7, 1%11, 1%13]]

test19 = TestCase (assertEqual "Tests the Kronecker product of diagonal matrices."
                               prod1
                               (PMatrix.kroneckerProduct mat1 $ PMatrix.iden 2))

test20 = TestCase (assertEqual "Tests the Kronecker product of non-square matrices."
                               prod2
                               (PMatrix.kroneckerProduct mat2 mat2_img1))

test21 = TestCase (assertEqual "Tests the Kronecker product of non-square matrices."
                               prod3
                               (PMatrix.kroneckerProduct iden_Q_2x2 mat2))

-----------------------------------------------------------------------------------------
-- Function: scale.

scale1 :: PMatrix.Matrix Rational
scale1 = PMatrix.build [[1%6,  1%9,  1%15],
                        [1%21, 1%33, 1%39]]

test22 = TestCase (assertEqual "Tests the ability to scale diagonal matrices."
                                prod1
                                (PMatrix.scale 5 $ PMatrix.iden 2))

test23 = TestCase (assertEqual "Tests the ability to scale non-square matrices."
                                scale1
                                (PMatrix.scale (1%3) mat2))

-----------------------------------------------------------------------------------------
-- Function: compose.

comp1 :: DMatrix.Matrix Rational
comp1 = DMatrix.fromLists [[1%4  + 1%15 + 1%55,  1%6  + 1%21 + 1%65],
                           [1%14 + 1%55 + 1%143, 1%21 + 1%77 + 1%169]]

comp2 :: DMatrix.Matrix Rational
comp2 = DMatrix.fromLists [[1%4  + 1%21, 1%6  + 1%33,  1%10 + 1%39],
                           [1%10 + 1%49, 1%15 + 1%77,  1%25 + 1%91],
                           [1%22 + 1%91, 1%33 + 1%143, 1%55 + 1%169]]

test24 = TestCase (assertEqual "Composition works correctly (1/5)."
                               mat2_img1
                               (PMatrix.compose mat2 basis_dim3_Q_1))

test25 = TestCase (assertEqual "Composition works correctly (2/5)."
                               mat2_img2
                               (PMatrix.compose mat2 basis_dim3_Q_2))

test26 = TestCase (assertEqual "Composition works correctly (3/5)."
                               mat2_img3
                               (PMatrix.compose mat2 basis_dim3_Q_3))

test27 = TestCase (assertEqual "Composition works correctly (4/5)."
                               comp1
                               (PMatrix.compose mat2 mat3))

test28 = TestCase (assertEqual "Composition works correctly (5/5)."
                               comp2
                               (PMatrix.compose mat3 mat2))

-----------------------------------------------------------------------------------------
-- Function: add.

mat4 :: PMatrix.Matrix Rational
mat4 = DMatrix.fromLists [[0%1, 1%3],
                          [2%5, 3%7],
                          [0%1, 10%13]]

add1 :: DMatrix.Matrix Int
add1 = DMatrix.fromLists [[6, 0],
                          [0, 6]]

add2 :: DMatrix.Matrix Rational
add2 = DMatrix.fromLists [[1%2,  2%3],
                          [3%5,  4%7],
                          [1%11, 11%13]]

test29 = TestCase (assertEqual "Can add diagonal matrices."
                               add1
                               (PMatrix.add prod1 $ PMatrix.iden 2))

test30 = TestCase (assertEqual "Can add non-square matrices."
                               add2
                               (PMatrix.add mat3 mat4))

-----------------------------------------------------------------------------------------
-- Function: size.

test31 = TestCase (assertEqual "Can compute the size of a 1x1 matrix"
                               (1, 1)
                               (PMatrix.size mat1))

test32 = TestCase (assertEqual "Can compute the size of a 2x1 matrix"
                               (2, 1)
                               (PMatrix.size basis_dim2_Q_2))

test33 = TestCase (assertEqual "Can compute the size of a 2x2 matrix"
                               (2, 2)
                               (PMatrix.size prod1))

test34 = TestCase (assertEqual "Can compute the size of a 2x3 matrix"
                               (2, 3)
                               (PMatrix.size mat2))

test35 = TestCase (assertEqual "Can compute the size of a 3x1 matrix"
                               (3, 1)
                               (PMatrix.size basis_dim3_Q_3))

test36 = TestCase (assertEqual "Can compute the size of a 3x2 matrix"
                               (3, 2)
                               (PMatrix.size mat3))

test37 = TestCase (assertEqual "Can compute the size of a 3x3 matrix"
                               (3, 3)
                               (PMatrix.size sum1))

test38 = TestCase (assertEqual "Can compute the size of a 4x3 matrix"
                               (4, 3)
                               (PMatrix.size prod2))

test39 = TestCase (assertEqual "Can compute the size of a 4x4 matrix"
                               (4, 4)
                               (PMatrix.size sum2))

test40 = TestCase (assertEqual "Can compute the size of a 4x6 matrix"
                               (4, 6)
                               (PMatrix.size prod3))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "build_1x1_basis1" test1,
                                     TestLabel "build_2x3_basis1" test2,
                                     TestLabel "build_2x3_basis2" test3,
                                     TestLabel "build_2x3_basis3" test4,
                                     TestLabel "build_3x2_basis1" test5,
                                     TestLabel "build_3x2_basis2" test6,
                                     TestLabel "iden_1x1_basis_1" test7,
                                     TestLabel "iden_2x2_basis_1" test8,
                                     TestLabel "iden_2x2_basis_2" test9,
                                     TestLabel "iden_3x3_basis_1" test10,
                                     TestLabel "iden_3x3_basis_2" test11,
                                     TestLabel "iden_3x3_basis_3" test12,
                                     TestLabel "swap_basis_1" test13,
                                     TestLabel "swap_basis_2" test14,
                                     TestLabel "swap_basis_3" test15,
                                     TestLabel "swap_basis_4" test16,
                                     TestLabel "directSum_1" test17,
                                     TestLabel "directSum_2" test18,
                                     TestLabel "kroneckerProduct_1" test19,
                                     TestLabel "kroneckerProduct_2" test20,
                                     TestLabel "kroneckerProduct_3" test21,
                                     TestLabel "scale_1" test22,
                                     TestLabel "scale_2" test23,
                                     TestLabel "comp_1" test24,
                                     TestLabel "comp_2" test25,
                                     TestLabel "comp_3" test26,
                                     TestLabel "comp_4" test27,
                                     TestLabel "comp_5" test28,
                                     TestLabel "add_1" test29,
                                     TestLabel "add_2" test30,
                                     TestLabel "size_1x1" test31,
                                     TestLabel "size_2x1" test32,
                                     TestLabel "size_2x2" test33,
                                     TestLabel "size_2x3" test34,
                                     TestLabel "size_3x1" test35,
                                     TestLabel "size_3x2" test36,
                                     TestLabel "size_3x3" test37,
                                     TestLabel "size_4x3" test38,
                                     TestLabel "size_4x4" test39,
                                     TestLabel "size_4x6" test40]

main = defaultMain tests

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Monoid
import Data.Ratio
import Pecac.Affine
import Pecac.List

-----------------------------------------------------------------------------------------
-- * Rational numbers are a Z-module.

instance RMod Integer (Sum Rational) where
    scale a (Sum b) = Sum $ (a % 1) * b

type AffineZQ = Affine Integer (Sum Rational)

-----------------------------------------------------------------------------------------
-- * Ensures that var agrees with linear.

test1 = TestCase (assertEqual "The function var agrees with linear (1/4)."
                              (linear [1] :: AffineZQ)
                              (var 0))

test2 = TestCase (assertEqual "The function var agrees with linear (2/4)."
                              (linear [0, 1] :: AffineZQ)
                              (var 1))

test3 = TestCase (assertEqual "The function var agrees with linear (3/4)."
                              (linear [0, 0, 1] :: AffineZQ)
                              (var 2))

test4 = TestCase (assertEqual "The function var agrees with linear (4/4)."
                              (linear [0, 0, 0, 1] :: AffineZQ)
                              (var 3))

-----------------------------------------------------------------------------------------
-- * Ensures that lit agrees with affine.

test5 = TestCase (assertEqual "The function lit agrees with affine (1/4)."
                              (affine [] 0 :: AffineZQ)
                              (lit 0))

test6 = TestCase (assertEqual "The function lit agrees with affine (2/4)."
                              (affine [] 1 :: AffineZQ)
                              (lit 1))

test7 = TestCase (assertEqual "The function lit agrees with affine (3/4)."
                              (affine [] 2 :: AffineZQ)
                              (lit 2))

test8 = TestCase (assertEqual "The function lit agrees with affine (4/4)."
                              (affine [] 3 :: AffineZQ)
                              (lit 3))

-----------------------------------------------------------------------------------------
-- * (<>)

test9 = TestCase (assertEqual "Tests the addition of affine functions of same size."
                              (affine [2, 3, 4] 1 :: AffineZQ)
                              (lhs <> rhs))
    where lhs = affine [1, 2, 3] 2
          rhs = affine [1, 1, 1] (-1)

test10 = TestCase (assertEqual "Tests the addition of affine functions (lhs is larger)."
                               (affine [1, 2, 3, 5] 3 :: AffineZQ)
                               (lhs <> rhs))
    where lhs = affine [1, 2, 1, 5] 2
          rhs = affine [0, 0, 2] 1

test11 = TestCase (assertEqual "Tests the addition of affine functions (rhs is larger)."
                               (affine [1, 2, 3, 5] 4 :: AffineZQ)
                               (lhs <> rhs))
    where lhs = affine [1, 2, 3] 2
          rhs = affine [0, 0, 0, 5] 2

test12 = TestCase (assertEqual "Tests affine linear function resizing (4/4)."
                               (affine [1, 2] 2 :: AffineZQ)
                               (lhs <> rhs))
    where lhs = affine [1, 2, 0, 0,  1,  3] 3
          rhs = affine [0, 0, 0, 0, -1, -3] (-1)

-----------------------------------------------------------------------------------------
-- * mempty

lidTest aff = TestCase (assertEqual msg aff $ mempty <> aff)
    where msg = "Identity failed on the left-hand side of " ++ show aff ++ "."

ridTest aff = TestCase (assertEqual msg aff $ aff <> mempty)
    where msg = "Identity failed on the right-hand side of " ++ show aff ++ "."

aff1 :: AffineZQ
aff1 = affine [] 5

aff2 :: AffineZQ
aff2 = affine [1] 4

aff3 :: AffineZQ
aff3 = affine [1, 7] 3

aff4 :: AffineZQ
aff4 = affine [1, 7, 4, -1, 5, 0, 0, 1, 2, 3] 107

test13 = lidTest aff1
test14 = ridTest aff1
test15 = lidTest aff2
test16 = ridTest aff2
test17 = lidTest aff3
test18 = ridTest aff3
test19 = lidTest aff4
test20 = ridTest aff4

-----------------------------------------------------------------------------------------
-- * Tests group inversion.

linvTest aff = TestCase (assertEqual msg mempty $ invert aff <> aff)
    where msg = "Inverse failed on the left-hand side of " ++ show aff ++ "."

rinvTest aff = TestCase (assertEqual msg mempty $ aff <> invert aff)
    where msg = "Inverse failed on the left-hand side of " ++ show aff ++ "."

test21 = linvTest aff1
test22 = rinvTest aff1
test23 = linvTest aff2
test24 = rinvTest aff2
test25 = linvTest aff3
test26 = rinvTest aff3
test27 = linvTest aff4
test28 = rinvTest aff4

-----------------------------------------------------------------------------------------
-- * scale

test29 = TestCase (assertEqual "Tests the R-mod structure of affine linear sums (1/4)."
                               aff1
                               (scale r aff1))
    where r = 1 :: Integer

test30 = TestCase (assertEqual "Tests the R-mod structure of affine linear sums (2/4)."
                               mempty
                               (scale r aff2))
    where r = 0 :: Integer

test31 = TestCase (assertEqual "Tests the R-mod structure of affine linear sums (3/4)."
                               (affine [2, 14] 6)
                               (scale r aff3))
    where r = 2 :: Integer

test32 = TestCase (assertEqual "Tests the R-mod structure of affine linear sums (4/4)."
                               (invert aff3)
                               (scale r aff3))
    where r = -1 :: Integer

-----------------------------------------------------------------------------------------
-- * eval

litEvalTest v = TestCase (assertEqual msg (Just v) $ eval aff [])
    where msg = "Failed to evaluate a constant affine linear sum " ++ show v ++ "."
          aff = lit v :: AffineZQ

test33 = litEvalTest 0
test34 = litEvalTest 1
test35 = litEvalTest 2
test36 = litEvalTest 32

varEvalTest j = TestCase (assertEqual msg (Just expt) $ eval aff inst)
    where msg  = "Failed to evaluate a affine linear sum x" ++ show j ++ "."
          aff  = var j :: AffineZQ
          expt = 5
          inst = repeatn 0 j ++ [expt]

test37 = varEvalTest 0
test38 = varEvalTest 1
test39 = varEvalTest 2
test40 = varEvalTest 300

test41 = TestCase (assertEqual "Can evaluate multi-term affine linear sums (1/2)."
                               (Just 11)
                               (eval aff2 [7]))

test42 = TestCase (assertEqual "Can evaluate multi-term affine linear sums (2/2)."
                               (Just 28)
                               (eval aff3 [4, 3]))

test43 = TestCase (assertEqual "Evaluations with too few arguments fail."
                               Nothing
                               (eval aff3 [4]))

test44 = TestCase (assertEqual "Evaluations ignore extra arguments."
                               (Just 28)
                               (eval aff3 [4, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1]))

-----------------------------------------------------------------------------------------
-- * cfold

test45 = TestCase (assertEqual "cfold works."
                               ([1, 7, 4, -1, 5, 0, 0, 1, 2, 3, 11, 22, 33])
                               (cfold (:) [11, 22, 33] aff4))

test46 = TestCase (assertEqual "cfold ignores trailing zeros."
                               1
                               (cfold min 1 aff))
    where aff = linear [1, 2, 3, 0, 0, 0] :: AffineZQ

-----------------------------------------------------------------------------------------
-- * cmap

test47 = TestCase (assertEqual "cmap works."
                               ([1, 7, 4, -1, 5, 0, 0, 1, 2, 3])
                               (cmap id aff4))

-----------------------------------------------------------------------------------------
-- * skew

test48 = TestCase (assertEqual "skew handles with same number of coefficients."
                               (affine [6, 21] 9)
                               (skew [2, 3] aff))
    where aff = affine [3, 7] 9 :: AffineZQ

test49 = TestCase (assertEqual "skew handles with missing coefficients."
                               (affine [3, 4] 10)
                               (skew [3, 2] aff))
    where aff = affine [1, 2, 3] 10 :: AffineZQ

test50 = TestCase (assertEqual "skew handles with extra coefficients."
                               (affine [3, 4, 3] 11)
                               (skew [3, 2, 1, 5] aff))
    where aff = affine [1, 2, 3] 11 :: AffineZQ

test51 = TestCase (assertEqual "skew shrinks affine linear sum."
                               (affine [3] 72)
                               (skew [3, 0] aff))
    where aff = affine [1, 2] 72 :: AffineZQ

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Compare_var_linear_1" test1,
                                     TestLabel "Compare_var_linear_2" test2,
                                     TestLabel "Compare_var_linear_3" test3,
                                     TestLabel "Compare_var_linear_4" test4,
                                     TestLabel "Compare_lit_affine_1" test5,
                                     TestLabel "Compare_lit_affine_2" test6,
                                     TestLabel "Compare_lit_affine_3" test7,
                                     TestLabel "Compare_lit_affine_4" test8,
                                     TestLabel "Addition_1" test9,
                                     TestLabel "Addition_2" test10,
                                     TestLabel "Addition_3" test11,
                                     TestLabel "Addition_4" test12,
                                     TestLabel "LIdentity_1" test13,
                                     TestLabel "RIdentity_1" test14,
                                     TestLabel "LIdentity_2" test15,
                                     TestLabel "RIdentity_2" test16,
                                     TestLabel "LIdentity_3" test17,
                                     TestLabel "RIdentity_3" test18,
                                     TestLabel "LIdentity_4" test19,
                                     TestLabel "RIdentity_4" test20,
                                     TestLabel "LInv_1" test21,
                                     TestLabel "RInv_1" test22,
                                     TestLabel "LInv_2" test23,
                                     TestLabel "RInv_2" test24,
                                     TestLabel "LInv_3" test25,
                                     TestLabel "RInv_3" test26,
                                     TestLabel "LInv_4" test27,
                                     TestLabel "RInv_4" test28,
                                     TestLabel "scale_1" test29,
                                     TestLabel "scale_2" test30,
                                     TestLabel "scale_3" test31,
                                     TestLabel "scale_4" test32,
                                     TestLabel "EvalLit_1" test33,
                                     TestLabel "EvalLit_2" test34,
                                     TestLabel "EvalLit_3" test35,
                                     TestLabel "EvalLit_4" test36,
                                     TestLabel "EvalVar_1" test37,
                                     TestLabel "EvalVar_2" test38,
                                     TestLabel "EvalVar_3" test39,
                                     TestLabel "EvalVar_4" test40,
                                     TestLabel "eval_1" test41,
                                     TestLabel "eval_2" test42,
                                     TestLabel "EvalMissingArgs" test43,
                                     TestLabel "EvalExtraArgs" test44,
                                     TestLabel "cfold_1" test45,
                                     TestLabel "cfold_2" test46,
                                     TestLabel "cmap" test47,
                                     TestLabel "skew_Exact" test48,
                                     TestLabel "skew_Fewer" test49,
                                     TestLabel "skew_Extra" test50,
                                     TestLabel "skew_Shrink" test51]

main = defaultMain tests

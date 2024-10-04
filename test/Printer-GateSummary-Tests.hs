module Main where

import Data.Ratio
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Pecac.Affine
import Pecac.Analyzer.Problem
import Pecac.Analyzer.Revolution
import Pecac.Parser.Syntax
import Pecac.Printer.GateSummary

-----------------------------------------------------------------------------------------
-- Affine Linear Combinations.

aff1 :: Affine Rational Revolution
aff1 = var 0

aff2 :: Affine Rational Revolution
aff2 = var 2

aff3 :: Affine Rational Revolution
aff3 = lit $ rationalToRev 3

aff4 :: Affine Rational Revolution
aff4 = lit $ rationalToRev $ -3

aff5 :: Affine Rational Revolution
aff5 = lit $ rationalToRev $ 3 % 4

aff6 :: Affine Rational Revolution
aff6 = var 2 <> var 4

aff7 :: Affine Rational Revolution
aff7 = skew [1%2, 2%3, 0, -5] base
    where base = var 0 <> var 1 <> var 3

aff8 :: Affine Rational Revolution
aff8 = litv <> skew [2%3, 0, 1%5, 5] base
    where base = var 0 <> var 2 <> var 3
          litv = lit $ rationalToRev $ 5 % 2

expr1 :: String -> Expr
expr1 pvar = CellId pvar 0

expr2 :: String -> Expr
expr2 pvar = CellId pvar 2

expr3 :: Expr
expr3 = Times (ConstNat 3) Tau

expr4 :: Expr
expr4 = Times (ConstNat $ -3) Tau

expr5 :: Expr
expr5 = Div (Times (ConstNat 3) Tau) (ConstNat 4)

expr6 :: String -> Expr
expr6 pvar = Plus (CellId pvar 2) (CellId pvar 4)

expr7 :: String -> Expr
expr7 pvar = Plus term1 $ Plus term2 term3
    where term1 = Div (CellId pvar 0) (ConstNat 2)
          term2 = Div (Times (ConstNat 2) (CellId pvar 1)) (ConstNat 3)
          term3 = Times (ConstNat $ -5) (CellId pvar 3)

expr8 :: String -> Expr
expr8 pvar = Plus (Plus term1 (Plus term2 term3)) const
    where term1 = Div (Times (ConstNat 2) (CellId pvar 0)) (ConstNat 3)
          term2 = Div (CellId pvar 2) (ConstNat 5)
          term3 = Times (ConstNat 5) (CellId pvar 3)
          const = Div (Times (ConstNat 5) Tau) (ConstNat 2)

-----------------------------------------------------------------------------------------
-- coeffsToExpr

test1 = TestCase (assertEqual "coeffsToExpr handles single params without coeffs (1/2)."
                              (expr1 "pvar")
                              (coeffsToExpr "pvar" aff1))

test2 = TestCase (assertEqual "coeffsToExpr handles single params without coeffs (2/2)."
                              (expr2 "params")
                              (coeffsToExpr "params" aff2))

test3 = TestCase (assertEqual "coeffsToExpr handles constant expressions (1/3)."
                              expr3
                              (coeffsToExpr "pvar" aff3))

test4 = TestCase (assertEqual "coeffsToExpr handles constant expressions (2/3)."
                              expr4
                              (coeffsToExpr "params" aff4))

test5 = TestCase (assertEqual "coeffsToExpr handles constant expressions (3/3)."
                              expr5
                              (coeffsToExpr "ps" aff5))

test6 = TestCase (assertEqual "coeffsToExpr handles sums of expressions."
                              (expr6 "ps")
                              (coeffsToExpr "ps" aff6))

test7 = TestCase (assertEqual "coeffsToExpr handles multiple parameters with coeffients."
                              (expr7 "pvar")
                              (coeffsToExpr "pvar" aff7))

test8 = TestCase (assertEqual "coeffsToExpr handles expressions with all types of terms."
                              (expr8 "ps")
                              (coeffsToExpr "ps" aff8))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "coeffsToExpr_OneVar_1" test1,
                                     TestLabel "coeffsToExpr_OneVar_1" test2,
                                     TestLabel "coeffsToExpr_Const_1" test3,
                                     TestLabel "coeffsToExpr_Const_2" test4,
                                     TestLabel "coeffsToExpr_Const_3" test5,
                                     TestLabel "coeffsToExpr_TwoVar" test6,
                                     TestLabel "coeffsToExpr_ThreeVar" test7,
                                     TestLabel "coeffsToExpr_Mixed" test8]

main = defaultMain tests

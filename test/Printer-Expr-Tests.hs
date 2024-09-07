module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Pecac.Parser.Syntax
import Pecac.Printer.Expr

-----------------------------------------------------------------------------------------
-- Test cases.

test1 = TestCase (assertEqual "Can print a VarId."
                              name
                              (printExpr $ VarId name))
    where name = "varname"

test2 = TestCase (assertEqual "Can print a CellId."
                              "varname[12]"
                              (printExpr $ CellId "varname" 12))

test3 = TestCase (assertEqual "Can print a ConstNat."
                              "73"
                              (printExpr $ ConstNat 73))

test4 = TestCase (assertEqual "Can print a plus expression."
                              "5 + var"
                              (printExpr $ Plus (ConstNat 5) (VarId "var")))

test5 = TestCase (assertEqual "Can print a minus expression."
                              "6 - n"
                              (printExpr $ Minus (ConstNat 6) (VarId "n")))

test6 = TestCase (assertEqual "Can print a times expression."
                              "7 * vec"
                              (printExpr $ Times (ConstNat 7) (VarId "vec")))

test7 = TestCase (assertEqual "Can print a brack expression."
                              "(5)"
                              (printExpr $ Brack $ ConstNat 5))

test8 = TestCase (assertEqual "Can print a negate expression."
                              "-5"
                              (printExpr $ Negate $ ConstNat 5))

test9 = TestCase (assertEqual "Can print a mixed expression."
                              "7 * var[5] + (name + 2)"
                              (printExpr expr))
    where expr = Plus (Times (ConstNat 7) (CellId "var" 5))
                      (Brack $ Plus (VarId "name") (ConstNat 2))

test10 = TestCase (assertEqual "Can print pi."
                               "pi"
                               (printExpr Pi))

test11 = TestCase (assertEqual "Can print tau."
                               "tau"
                               (printExpr Tau))

test12 = TestCase (assertEqual "Can print division statements"
                              "(7 * var[5]) + (pi / 2)"
                              (printExpr expr))
    where expr = Plus (Brack (Times (ConstNat 7) (CellId "var" 5)))
                      (Brack (Div Pi (ConstNat 2)))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "printExpr_VarId" test1,
                                     TestLabel "printExpr_CellId" test2,
                                     TestLabel "printExpr_ConstNat" test3,
                                     TestLabel "printExpr_Plus" test4,
                                     TestLabel "printExpr_Minus" test5,
                                     TestLabel "printExpr_Times" test6,
                                     TestLabel "printExpr_Brack" test7,
                                     TestLabel "printExpr_Negate" test8,
                                     TestLabel "printExpr_Mixed" test9,
                                     TestLabel "printExpr_Pi" test10,
                                     TestLabel "printExpr_Tau" test11]

main = defaultMain tests

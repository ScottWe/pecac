module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Pecac.Parser.FDecl
import Pecac.Parser.Syntax

-----------------------------------------------------------------------------------------
-- RegisterMap: Allocation

checkAllocFail :: String -> Either String RegisterMap -> Bool
checkAllocFail exp (Left act) = exp == act
checkAllocFail _   _          = False

assertSuccess :: Either String RegisterMap -> RegisterMap
assertSuccess (Left _)    = error "Failed to allocate map."
assertSuccess (Right map) = map

nulla = createRegistry "var"
nullb = createRegistry "myvar"

res1a = allocate nulla ["a", "b", "c", "c"]
res2a = allocate nulla ["a", "b", "c"]
res3a = allocate (assertSuccess res2a) ["x", "c", "y"]
res4a = allocate (assertSuccess res2a) ["x", "y"]

res1b = allocate nullb ["a", "b", "c", "c"]
res2b = allocate nullb ["a", "b", "c"]
res3b = allocate (assertSuccess res2b) ["x", "c", "y"]
res4b = allocate (assertSuccess res2b) ["x", "y"]

mapa = assertSuccess res4a
mapb = assertSuccess res4b

test1 = TestCase (assertBool "Detects duplicates in a single list (1/2)."
                             (checkAllocFail "c" res1a))

test2 = TestCase (assertBool "Detects duplicates in a single list (2/2)."
                             (checkAllocFail "c" res1b))

test3 = TestCase (assertBool "Detects duplicates across multiple lists (1/2)."
                             (checkAllocFail "c" res3a))

test4 = TestCase (assertBool "Detects duplicates across multiple lists (2/2)."
                             (checkAllocFail "c" res3b))

-----------------------------------------------------------------------------------------
-- RegisterMap: getAsOperand

test5 = TestCase (assertEqual "Can get variable as an operand (1/6)."
                              (Just $ QReg "var" 0)
                              (getAsOperand mapa "a"))

test6 = TestCase (assertEqual "Can get variable as an operand (2/6)."
                              (Just $ QReg "var" 1)
                              (getAsOperand mapa "b"))

test7 = TestCase (assertEqual "Can get variable as an operand (3/6)."
                              (Just $ QReg "var" 2)
                              (getAsOperand mapa "c"))

test8 = TestCase (assertEqual "Can get variable as an operand (4/6)."
                              (Just $ QReg "var" 3)
                              (getAsOperand mapa "x"))

test9 = TestCase (assertEqual "Can get variable as an operand (5/6)."
                              (Just $ QReg "var" 4)
                              (getAsOperand mapa "y"))

test10 = TestCase (assertEqual "Can get variable as an operand (6/6)."
                               (Just $ QReg "myvar" 2)
                               (getAsOperand mapb "c"))

test11 = TestCase (assertEqual "getAsOperand identifies missing records (1/4)."
                               Nothing
                               (getAsOperand mapa "d"))

test12 = TestCase (assertEqual "getAsOperand identifies missing records (2/4)"
                               Nothing
                               (getAsOperand mapa "z"))

test13 = TestCase (assertEqual "getAsOperand identifies missing records (3/4)"
                               Nothing
                               (getAsOperand mapb "d"))

test14 = TestCase (assertEqual "getAsOperand identifies missing records (4/4)"
                               Nothing
                               (getAsOperand mapb "z"))

-----------------------------------------------------------------------------------------
-- RegisterMap: getAsExpr

test15 = TestCase (assertEqual "Can get variable as an expression (1/6)."
                               (Just $ CellId "var" 0)
                               (getAsExpr mapa "a"))

test16 = TestCase (assertEqual "Can get variable as an expression (2/6)."
                               (Just $ CellId "var" 1)
                               (getAsExpr mapa "b"))

test17 = TestCase (assertEqual "Can get variable as an expression (3/6)."
                               (Just $ CellId "var" 2)
                               (getAsExpr mapa "c"))

test18 = TestCase (assertEqual "Can get variable as an expression (4/6)."
                               (Just $ CellId "var" 3)
                               (getAsExpr mapa "x"))

test19 = TestCase (assertEqual "Can get variable as an expression (5/6)."
                               (Just $ CellId "var" 4)
                               (getAsExpr mapa "y"))

test20 = TestCase (assertEqual "Can get variable as an expression (6/6)."
                                (Just $ CellId "myvar" 2)
                                (getAsExpr mapb "c"))

test21 = TestCase (assertEqual "getAsExpr identifies missing records (1/4)."
                                Nothing
                                (getAsExpr mapa "d"))

test22 = TestCase (assertEqual "getAsExpr identifies missing records (2/4)"
                                Nothing
                                (getAsExpr mapa "z"))

test23 = TestCase (assertEqual "getAsExpr identifies missing records (3/4)"
                                Nothing
                                (getAsExpr mapb "d"))

test24 = TestCase (assertEqual "getAsExpr identifies missing records (4/4)"
                                Nothing
                                (getAsExpr mapb "z"))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "alloc_Fail_Dup_1" test1,
                                     TestLabel "alloc_Fail_Dup_2" test2,
                                     TestLabel "alloc_Fail_Dup_3" test3,
                                     TestLabel "alloc_Fail_Dup_4" test4,
                                     TestLabel "getAsOperand_1" test5,
                                     TestLabel "getAsOperand_2" test6,
                                     TestLabel "getAsOperand_3" test7,
                                     TestLabel "getAsOperand_4" test8,
                                     TestLabel "getAsOperand_5" test9,
                                     TestLabel "getAsOperand_6" test10,
                                     TestLabel "getAsOperand_Fail_1" test11,
                                     TestLabel "getAsOperand_Fail_2" test12,
                                     TestLabel "getAsOperand_Fail_3" test13,
                                     TestLabel "getAsOperand_Fail_4" test14,
                                     TestLabel "getAsExpr_1" test15,
                                     TestLabel "getAsExpr_2" test16,
                                     TestLabel "getAsExpr_3" test17,
                                     TestLabel "getAsExpr_4" test18,
                                     TestLabel "getAsExpr_5" test19,
                                     TestLabel "getAsExpr_6" test20,
                                     TestLabel "getAsExpr_Fail_1" test21,
                                     TestLabel "getAsExpr_Fail_2" test22,
                                     TestLabel "getAsExpr_Fail_3" test23,
                                     TestLabel "getAsExpr_Fail_4" test24]

main = defaultMain tests

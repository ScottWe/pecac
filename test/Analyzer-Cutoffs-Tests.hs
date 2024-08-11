module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Pecac.Analyzer.Cutoffs
import Pecac.Analyzer.Gate
import Pecac.Analyzer.Problem

-----------------------------------------------------------------------------------------
-- Gate Summary Declarations

-- Parameter-Free Gates.

plain1 :: GateSummary
plain1 = PlainSummary GateX $ GateConfigs False [Pos, Neg] [0, 1, 2]

plain2 :: GateSummary
plain2 = PlainSummary GateCH $ GateConfigs False [] [3, 10]

plain3 :: GateSummary
plain3 = PlainSummary GateT $ GateConfigs True [Pos, Pos, Pos] [2, 4, 6, 8]

-- Rotation Gates (k = 3)

rot1_k3 :: GateSummary
rot1_k3 = RotSummary RotX [0, 5, -2] $ GateConfigs True [Pos] [2, 5]
-- Lambda: [0, 5, 2]
-- Kappa:  5

rot2_k3 :: GateSummary
rot2_k3 = RotSummary RotZ [-6, 0, 4] $ GateConfigs True [Pos, Neg] [2, 5, 3]
-- Lambda: [6, 0, 4]
-- Kappa:  6

rot3_k3 :: GateSummary
rot3_k3 = RotSummary RotCY [1, 1, 4] $ GateConfigs False [] [2, 5]
-- Lambda: [1, 1, 4]
-- Kappa:  6

-- Rotation Gates (k = 5)

rot1_k5 :: GateSummary
rot1_k5 = RotSummary RotX [0, 1, 0, -1, 0] $ GateConfigs True [Pos] [2, 5]
-- Lambda: [0, 1, 0, 1, 0]
-- Kappa:  1

rot2_k5 :: GateSummary
rot2_k5 = RotSummary RotZ [-1, -1, 2, 3, -1] $ GateConfigs True [Pos, Neg] [2, 5, 3]
-- Lambda: [1, 1, 2, 3, 1]
-- Kappa:  5

rot3_k5 :: GateSummary
rot3_k5 = RotSummary RotCY [-2, 7, -3, 0, -10] $ GateConfigs False [] [2, 5]
-- Lambda: [2, 7, 3, 0, 10]
-- Kappa:  15

mixed_k3 :: [GateSummary]
mixed_k3 = [rot1_k3, plain1, rot2_k3, plain3, rot3_k3]

mixed_k5 :: [GateSummary]
mixed_k5 = [rot1_k5, plain1, rot2_k5, plain3, rot3_k5]

-----------------------------------------------------------------------------------------
-- Circuit Declarations

qvar :: QubitReg
qvar = QubitReg "qs" 100

-----------------------------------------------------------------------------------------
-- gatesToAlphas

test1 = TestCase (assertEqual "Empty gate lists have empty alpha lists."
                              []
                              (gatesToAlphas []))

test2 = TestCase (assertEqual "A single plain gate has an empty alpha list."
                              []
                              (gatesToAlphas [plain1]))

test3 = TestCase (assertEqual "A list of several plain gates has an empty alpha list."
                              []
                              (gatesToAlphas [plain1, plain2, plain3]))

test4 = TestCase (assertEqual "A single rot gate has the correct alpha list (1/2)."
                              [[0, 5, -2]]
                              (gatesToAlphas [rot1_k3]))

test5 = TestCase (assertEqual "A single rot gate has the correct alpha list (2/2)."
                              [[0, 1, 0, -1, 0]]
                              (gatesToAlphas [rot1_k5]))

test6 = TestCase (assertEqual "A list of rot gates has the correct alpha list (1/2)."
                              [[0, 5, -2], [-6, 0, 4], [1, 1, 4]]
                              (gatesToAlphas [rot1_k3, rot2_k3, rot3_k3]))

test7 = TestCase (assertEqual "A list of rot gates has the correct alpha list (2/2)."
                              [[0, 1, 0, -1, 0], [-1, -1, 2, 3, -1], [-2, 7, -3, 0, -10]]
                              (gatesToAlphas [rot1_k5, rot2_k5, rot3_k5]))

test8 = TestCase (assertEqual "A list of mixed gates has the correct alpha list (1/2)."
                              [[0, 5, -2], [-6, 0, 4], [1, 1, 4]]
                              (gatesToAlphas mixed_k3))

test9 = TestCase (assertEqual "A list of mixed gates has the correct alpha list (2/2)."
                              [[0, 1, 0, -1, 0], [-1, -1, 2, 3, -1], [-2, 7, -3, 0, -10]]
                              (gatesToAlphas mixed_k5))

-----------------------------------------------------------------------------------------
-- gatesToLambda

test10 = TestCase (assertEqual "gatesToLambda handles empty gate lists (1/2)."
                               [0, 0, 0]
                               (gatesToLambda 3 []))

test11 = TestCase (assertEqual "gatesToLambda handles empty gate lists (2/2)."
                               [0, 0, 0, 0, 0]
                               (gatesToLambda 5 []))

test12 = TestCase (assertEqual "gatesToLambda handles single plain gates (1/2)."
                               [0, 0, 0]
                               (gatesToLambda 3 [plain1]))

test13 = TestCase (assertEqual "gatesToLambda handles single plain gates (1/2)."
                               [0, 0, 0, 0, 0]
                               (gatesToLambda 5 [plain1]))

test14 = TestCase (assertEqual "gatesToLambda handles lists of plain gates (1/2)."
                               [0, 0, 0]
                               (gatesToLambda 3 [plain1, plain2, plain3]))

test15 = TestCase (assertEqual "gatesToLambda handles lists of plain gates (2/2)."
                               [0, 0, 0, 0, 0]
                               (gatesToLambda 5 [plain1, plain2, plain3]))

test16 = TestCase (assertEqual "gatesToLambda handles single rot gates (1/2)."
                               [0, 5, 2]
                               (gatesToLambda 3 [rot1_k3]))

test17 = TestCase (assertEqual "gatesToLambda handles single rot gates (2/2)."
                               [0, 1, 0, 1, 0]
                               (gatesToLambda 5 [rot1_k5]))

test18 = TestCase (assertEqual "gatesToLambda handles a list of rot gates (1/2)."
                               [7, 6, 10]
                               (gatesToLambda 3 [rot1_k3, rot2_k3, rot3_k3]))

test19 = TestCase (assertEqual "gatesToLambda handles a list of rot gates (2/2)."
                               [3, 9, 5, 4, 11]
                               (gatesToLambda 5 [rot1_k5, rot2_k5, rot3_k5]))

test20 = TestCase (assertEqual "gatesToLambda handles mixed gate lists (1/2)."
                               [7, 6, 10]
                               (gatesToLambda 3 mixed_k3))

test21 = TestCase (assertEqual "gatesToLambda handles mixed gate lists (2/2)."
                               [3, 9, 5, 4, 11]
                               (gatesToLambda 5 mixed_k5))

-----------------------------------------------------------------------------------------
-- circToLambda

test22 = TestCase (assertEqual "circToLambda handles empty gate lists (1/2)."
                               [0, 0, 0]
                               (circToLambda $ ParamCirc pvar qvar []))
    where pvar = ParamArr "theta" 3

test23 = TestCase (assertEqual "circToLambda handles empty gate lists (2/2)."
                               [0, 0, 0, 0, 0]
                               (circToLambda $ ParamCirc pvar qvar []))
    where pvar = ParamArr "theta" 5

test24 = TestCase (assertEqual "circToLambda handles mixed gate lists (1/2)."
                               [7, 6, 10]
                               (circToLambda $ ParamCirc pvar qvar mixed_k3))
    where pvar = ParamArr "theta" 3

test25 = TestCase (assertEqual "circToLambda handles mixed gate lists (2/2)."
                               [3, 9, 5, 4, 11]
                               (circToLambda $ ParamCirc pvar qvar mixed_k5))
    where pvar = ParamArr "theta" 5

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "gatesToAlphas_Empty" test1,
                                     TestLabel "gatesToAlphas_1Plain" test2,
                                     TestLabel "gatesToAlphas_3Plain" test3,
                                     TestLabel "gatesToAlphas_1Rot_1" test4,
                                     TestLabel "gatesToAlphas_1Rot_2" test5,
                                     TestLabel "gatesToAlphas_3Rot_1" test6,
                                     TestLabel "gatesToAlphas_3Rot_2" test7,
                                     TestLabel "gatesToAlphas_Mixed_1" test8,
                                     TestLabel "gatesToAlphas_Mixed_2" test9,
                                     TestLabel "gatesToLambda_Empty_1" test10,
                                     TestLabel "gatesToLambda_Empty_2" test11,
                                     TestLabel "gatesToLambda_1Plain_1" test12,
                                     TestLabel "gatesToLambda_1Plain_2" test13,
                                     TestLabel "gatesToLambda_3Plain_1" test14,
                                     TestLabel "gatesToLambda_3Plain_2" test15,
                                     TestLabel "gatesToLambda_1Rot_1" test16,
                                     TestLabel "gatesToLambda_1Rot_2" test17,
                                     TestLabel "gatesToLambda_3Rot_1" test18,
                                     TestLabel "gatesToLambda_3Rot_2" test19,
                                     TestLabel "gatesToLambda_Mixed_1" test20,
                                     TestLabel "gatesToLambda_Mixed_2" test21,
                                     TestLabel "circToLambda_Empty_1" test22,
                                     TestLabel "circToLambda_Empty_2" test23,
                                     TestLabel "circToLambda_Mixed_1" test24,
                                     TestLabel "circToLambda_Mixed_2" test25]

main = defaultMain tests


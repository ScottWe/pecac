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

-----------------------------------------------------------------------------------------
-- Gate Summary Lists

plain_list :: [GateSummary]
plain_list = [plain1, plain2, plain3]

rot_k3 :: [GateSummary]
rot_k3 = [rot1_k3, rot2_k3, rot3_k3]

rot_k5 :: [GateSummary]
rot_k5 = [rot1_k5, rot2_k5, rot3_k5]

mixed_k3 :: [GateSummary]
mixed_k3 = [rot1_k3, plain1, rot2_k3, plain3, rot3_k3]

mixed_k5 :: [GateSummary]
mixed_k5 = [rot1_k5, plain1, rot2_k5, plain3, rot3_k5]

-----------------------------------------------------------------------------------------
-- Circuit Declarations

qvar :: QubitReg
qvar = QubitReg "qs" 100

pvar_k3 :: ParamArr
pvar_k3 = ParamArr "theta" 3

pvar_k5 :: ParamArr
pvar_k5 = ParamArr "theta" 5

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
                              (gatesToAlphas plain_list))

test4 = TestCase (assertEqual "A single rot gate has the correct alpha list (1/2)."
                              [[0, 5, -2]]
                              (gatesToAlphas [rot1_k3]))

test5 = TestCase (assertEqual "A single rot gate has the correct alpha list (2/2)."
                              [[0, 1, 0, -1, 0]]
                              (gatesToAlphas [rot1_k5]))

test6 = TestCase (assertEqual "A list of rot gates has the correct alpha list (1/2)."
                              [[0, 5, -2], [-6, 0, 4], [1, 1, 4]]
                              (gatesToAlphas rot_k3))

test7 = TestCase (assertEqual "A list of rot gates has the correct alpha list (2/2)."
                              [[0, 1, 0, -1, 0], [-1, -1, 2, 3, -1], [-2, 7, -3, 0, -10]]
                              (gatesToAlphas rot_k5))

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

test13 = TestCase (assertEqual "gatesToLambda handles single plain gates (2/2)."
                               [0, 0, 0, 0, 0]
                               (gatesToLambda 5 [plain1]))

test14 = TestCase (assertEqual "gatesToLambda handles lists of plain gates (1/2)."
                               [0, 0, 0]
                               (gatesToLambda 3 plain_list))

test15 = TestCase (assertEqual "gatesToLambda handles lists of plain gates (2/2)."
                               [0, 0, 0, 0, 0]
                               (gatesToLambda 5 plain_list))

test16 = TestCase (assertEqual "gatesToLambda handles single rot gates (1/2)."
                               [0, 5, 2]
                               (gatesToLambda 3 [rot1_k3]))

test17 = TestCase (assertEqual "gatesToLambda handles single rot gates (2/2)."
                               [0, 1, 0, 1, 0]
                               (gatesToLambda 5 [rot1_k5]))

test18 = TestCase (assertEqual "gatesToLambda handles a list of rot gates (1/2)."
                               [7, 6, 10]
                               (gatesToLambda 3 rot_k3))

test19 = TestCase (assertEqual "gatesToLambda handles a list of rot gates (2/2)."
                               [3, 9, 5, 4, 11]
                               (gatesToLambda 5 rot_k5))

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
                               (circToLambda $ ParamCirc pvar_k3 qvar []))

test23 = TestCase (assertEqual "circToLambda handles empty gate lists (2/2)."
                               [0, 0, 0, 0, 0]
                               (circToLambda $ ParamCirc pvar_k5 qvar []))

test24 = TestCase (assertEqual "circToLambda handles mixed gate lists (1/2)."
                               [7, 6, 10]
                               (circToLambda $ ParamCirc pvar_k3 qvar mixed_k3))

test25 = TestCase (assertEqual "circToLambda handles mixed gate lists (2/2)."
                               [3, 9, 5, 4, 11]
                               (circToLambda $ ParamCirc pvar_k5 qvar mixed_k5))

-----------------------------------------------------------------------------------------
-- gatesToKappa

test26 = TestCase (assertEqual "gatesToKappa handles empty lists."
                               0
                               (gatesToKappa []))

test27 = TestCase (assertEqual "gatesToKappa handles single plain gates"
                               0
                               (gatesToKappa [plain1]))

test28 = TestCase (assertEqual "gatesToKappa handles lists of plain gates."
                               0
                               (gatesToKappa plain_list))

test29 = TestCase (assertEqual "gatesToKappa handles single rot gates (1/2)."
                               5
                               (gatesToKappa [rot1_k3]))

test30 = TestCase (assertEqual "gatesToKappa handles single rot gates (2/2)."
                               5
                               (gatesToKappa [rot2_k5]))

test31 = TestCase (assertEqual "gatesToKappa handles lists of rot gates (1/2)."
                               17
                               (gatesToKappa rot_k3))

test32 = TestCase (assertEqual "gatesToKappa handles lists of rot gates (2/2)."
                               21
                               (gatesToKappa rot_k5))

test33 = TestCase (assertEqual "gatesToKappa handles lists of mixed gates (1/2)."
                               17
                               (gatesToKappa mixed_k3))

test34 = TestCase (assertEqual "gatesToKappa handles lists of mixed gates (2/2)."
                               21
                               (gatesToKappa mixed_k5))

-----------------------------------------------------------------------------------------
-- circToKappa

test35 = TestCase (assertEqual "circToKappa handles empty gate lists (1/2)."
                               0
                               (circToKappa $ ParamCirc pvar_k3 qvar []))

test36 = TestCase (assertEqual "circToLambda handles empty gate lists (2/2)."
                               0
                               (circToKappa $ ParamCirc pvar_k5 qvar []))

test37 = TestCase (assertEqual "circToLambda handles mixed gate lists (1/2)."
                               17
                               (circToKappa $ ParamCirc pvar_k3 qvar mixed_k3))

test38 = TestCase (assertEqual "circToLambda handles mixed gate lists (2/2)."
                               21
                               (circToKappa $ ParamCirc pvar_k5 qvar mixed_k5))

-----------------------------------------------------------------------------------------
-- forallElimSize

defaultElim_k3 :: Maybe [Int]
defaultElim_k3 = Just $ [1, 1, 1]

defaultElim_k5 :: Maybe [Int]
defaultElim_k5 = Just $ [1, 1, 1, 1, 1]

test39 = TestCase (assertEqual "forallElimSize handles empty gate lists (1/2)."
                               defaultElim_k3
                               (forallElimSize circ circ))
    where circ = ParamCirc pvar_k3 qvar []

test40 = TestCase (assertEqual "forallElimSize handles empty gate lists (2/2)."
                               defaultElim_k5
                               (forallElimSize circ circ))
    where circ = ParamCirc pvar_k5 qvar []

test41 = TestCase (assertEqual "forallElimSize handles single plain gates (1/2)."
                               defaultElim_k3
                               (forallElimSize circ circ))
    where circ = ParamCirc pvar_k3 qvar [plain1]

test42 = TestCase (assertEqual "forallElimSize handles single plain gates (2/2)."
                               defaultElim_k5
                               (forallElimSize circ circ))
    where circ = ParamCirc pvar_k5 qvar [plain1]

test43 = TestCase (assertEqual "forallElimSize handles lists of plain gates (1/2)."
                               defaultElim_k3
                               (forallElimSize circ circ))
    where circ = ParamCirc pvar_k3 qvar plain_list

test44 = TestCase (assertEqual "forallElimSize handles lists of plain gates (2/2)."
                               defaultElim_k5
                               (forallElimSize circ circ))
    where circ = ParamCirc pvar_k5 qvar plain_list

test45 = TestCase (assertEqual "forallElimSize handles single rot gates (1/2)."
                               (Just [1, 11, 5])
                               (forallElimSize circ circ))
    where circ = ParamCirc pvar_k3 qvar [rot1_k3]

test46 = TestCase (assertEqual "forallElimSize handles single rot gates (2/2)."
                               (Just [1, 3, 1, 3, 1])
                               (forallElimSize circ circ))
    where circ = ParamCirc pvar_k5 qvar [rot1_k5]

test47 = TestCase (assertEqual "forallElimSize handles distinct single rot circs (1/2)."
                               (Just [13, 11, 9])
                               (forallElimSize circ1 circ2))
    where circ1 = ParamCirc pvar_k3 qvar [rot1_k3]
          circ2 = ParamCirc pvar_k3 qvar [rot2_k3]

test48 = TestCase (assertEqual "forallElimSize handles distinct single rot circs (2/2)."
                               (Just [3, 3, 5, 7, 3])
                               (forallElimSize circ1 circ2))
    where circ1 = ParamCirc pvar_k5 qvar [rot1_k5]
          circ2 = ParamCirc pvar_k5 qvar [rot2_k5]

test49 = TestCase (assertEqual "forallElimSize handles a list of rot gates (1/2)."
                               (Just [15, 13, 21])
                               (forallElimSize circ circ))
    where circ = ParamCirc pvar_k3 qvar rot_k3

test50 = TestCase (assertEqual "forallElimSize handles a list of rot gates (2/2)."
                               (Just [7, 19, 11, 9, 23])
                               (forallElimSize circ circ))
    where circ = ParamCirc pvar_k5 qvar rot_k5

test51 = TestCase (assertEqual "forallElimSize handles distinct multi-rot circs (1/2)."
                               (Just [15, 11, 17])
                               (forallElimSize circ1 circ2))
    where circ1 = ParamCirc pvar_k3 qvar [rot1_k3, rot2_k3]
          circ2 = ParamCirc pvar_k3 qvar [rot2_k3, rot3_k3]

test52 = TestCase (assertEqual "forallElimSize handles distinct multi-rot circs (2/2)."
                               (Just [7, 17, 11, 9, 23])
                               (forallElimSize circ1 circ2))
    where circ1 = ParamCirc pvar_k5 qvar [rot1_k5, rot2_k5]
          circ2 = ParamCirc pvar_k5 qvar [rot2_k5, rot3_k5]

test53 = TestCase (assertEqual "forallElimSize handles mixed gate lists (1/2)."
                               (Just [15, 13, 21])
                               (forallElimSize circ circ))
    where circ = ParamCirc pvar_k3 qvar mixed_k3

test54 = TestCase (assertEqual "forallElimSize handles mixed gate lists (2/2)."
                               (Just [7, 19, 11, 9, 23])
                               (forallElimSize circ circ))
    where circ = ParamCirc pvar_k5 qvar mixed_k5

test55 = TestCase (assertEqual "forallElimSize rejects misaligned sizes (1/2)."
                               Nothing
                               (forallElimSize circ1 circ2))
    where circ1 = ParamCirc pvar_k3 qvar []
          circ2 = ParamCirc pvar_k5 qvar []

test56 = TestCase (assertEqual "forallElimSize rejects misaligned sizes (2/2)."
                               Nothing
                               (forallElimSize circ1 circ2))
    where circ1 = ParamCirc pvar_k3 qvar mixed_k3
          circ2 = ParamCirc pvar_k5 qvar mixed_k5

-----------------------------------------------------------------------------------------
-- randomSampleSize

test57 = TestCase (assertEqual "randomSampleSize handles empty gate lists (1/2)."
                               (Just 0)
                               (randomSampleSize circ circ))
    where circ = ParamCirc pvar_k3 qvar []

test58 = TestCase (assertEqual "randomSampleSize handles empty gate lists (2/2)."
                               (Just 0)
                               (randomSampleSize circ circ))
    where circ = ParamCirc pvar_k5 qvar []

test59 = TestCase (assertEqual "randomSampleSize handles single plain gates (1/2)."
                               (Just 0)
                               (randomSampleSize circ circ))
    where circ = ParamCirc pvar_k3 qvar [plain1]

test60 = TestCase (assertEqual "randomSampleSize handles single plain gates (2/2)."
                               (Just 0)
                               (randomSampleSize circ circ))
    where circ = ParamCirc pvar_k5 qvar [plain1]

test61 = TestCase (assertEqual "randomSampleSize handles lists of plain gates (1/2)."
                               (Just 0)
                               (randomSampleSize circ circ))
    where circ = ParamCirc pvar_k3 qvar plain_list

test62 = TestCase (assertEqual "randomSampleSize handles lists of plain gates (2/2)."
                               (Just 0)
                               (randomSampleSize circ circ))
    where circ = ParamCirc pvar_k5 qvar plain_list

test63 = TestCase (assertEqual "randomSampleSize handles single rot gates (1/2)."
                               (Just 12)
                               (randomSampleSize circ circ))
    where circ = ParamCirc pvar_k3 qvar [rot1_k3]

test64 = TestCase (assertEqual "randomSampleSize handles single rot gates (2/2)."
                               (Just 3)
                               (randomSampleSize circ circ))
    where circ = ParamCirc pvar_k5 qvar [rot1_k5]

test65 = TestCase (assertEqual "randomSampleSize handles distinct single rot circs (1/2)."
                               (Just 21)
                               (randomSampleSize circ1 circ2))
    where circ1 = ParamCirc pvar_k3 qvar [rot1_k3]
          circ2 = ParamCirc pvar_k3 qvar [rot2_k3]

test66 = TestCase (assertEqual "randomSampleSize handles distinct single rot circs (2/2)."
                               (Just 13)
                               (randomSampleSize circ1 circ2))
    where circ1 = ParamCirc pvar_k5 qvar [rot1_k5]
          circ2 = ParamCirc pvar_k5 qvar [rot2_k5]

test67 = TestCase (assertEqual "randomSampleSize handles a list of rot gates (1/2)."
                               (Just 40)
                               (randomSampleSize circ circ))
    where circ = ParamCirc pvar_k3 qvar rot_k3

test68 = TestCase (assertEqual "randomSampleSize handles a list of rot gates (2/2)."
                               (Just 53)
                               (randomSampleSize circ circ))
    where circ = ParamCirc pvar_k5 qvar rot_k5

test69 = TestCase (assertEqual "randomSampleSize handles distinct multi-rot circs (1/2)."
                               (Just 32)
                               (randomSampleSize circ1 circ2))
    where circ1 = ParamCirc pvar_k3 qvar [rot1_k3, rot2_k3]
          circ2 = ParamCirc pvar_k3 qvar [rot2_k3, rot3_k3]

test70 = TestCase (assertEqual "randomSampleSize handles distinct multi-rot circs (2/2)."
                               (Just 51)
                               (randomSampleSize circ1 circ2))
    where circ1 = ParamCirc pvar_k5 qvar [rot1_k5, rot2_k5]
          circ2 = ParamCirc pvar_k5 qvar [rot2_k5, rot3_k5]

test71 = TestCase (assertEqual "randomSampleSize handles mixed gate lists (1/2)."
                               (Just 40)
                               (randomSampleSize circ circ))
    where circ = ParamCirc pvar_k3 qvar mixed_k3

test72 = TestCase (assertEqual "randomSampleSize handles mixed gate lists (2/2)."
                               (Just 53)
                               (randomSampleSize circ circ))
    where circ = ParamCirc pvar_k5 qvar mixed_k5

test73 = TestCase (assertEqual "randomSampleSize rejects misaligned sizes (1/2)."
                               Nothing
                               (randomSampleSize circ1 circ2))
    where circ1 = ParamCirc pvar_k3 qvar []
          circ2 = ParamCirc pvar_k5 qvar []

test74 = TestCase (assertEqual "randomSampleSize rejects misaligned sizes (2/2)."
                               Nothing
                               (randomSampleSize circ1 circ2))
    where circ1 = ParamCirc pvar_k3 qvar mixed_k3
          circ2 = ParamCirc pvar_k5 qvar mixed_k5

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
                                     TestLabel "circToLambda_Mixed_2" test25,
                                     TestLabel "gatesToKappa_Empty" test26,
                                     TestLabel "gatesToKappa_1Plain" test27,
                                     TestLabel "gatesToKappa_3Plain" test28,
                                     TestLabel "gatesToKappa_1Rot_1" test29,
                                     TestLabel "gatesToKappa_1Rot_2" test30,
                                     TestLabel "gatesToKappa_3Rot_1" test31,
                                     TestLabel "gatesToKappa_3Rot_2" test32,
                                     TestLabel "gatesToKappa_Mixed_1" test33,
                                     TestLabel "gatesToKappa_Mixed_2" test34,
                                     TestLabel "circToKappa_Empty_1" test35,
                                     TestLabel "circToKappa_Empty_2" test36,
                                     TestLabel "circToKappa_Mixed_1" test37,
                                     TestLabel "circToKappa_Mixed_2" test38,
                                     TestLabel "forallElimSize_Empty_1" test39,
                                     TestLabel "forallElimSize_Empty_2" test40,
                                     TestLabel "forallElimSize_1Plain_1" test41,
                                     TestLabel "forallElimSize_1Plain_2" test42,
                                     TestLabel "forallElimSize_3Plain_1" test43,
                                     TestLabel "forallElimSize_3Plain_2" test44,
                                     TestLabel "forallElimSize_1Rot_1" test45,
                                     TestLabel "forallElimSize_1Rot_2" test46,
                                     TestLabel "forallElimSize_1Rot_2Circ_1" test47,
                                     TestLabel "forallElimSize_1Rot_2Circ_2" test48,
                                     TestLabel "forallElimSize_3Rot_1" test49,
                                     TestLabel "forallElimSize_3Rot_2" test50,
                                     TestLabel "forallElimSize_3Rot_2Circ_1" test51,
                                     TestLabel "forallElimSize_3Rot_2Circ_2" test52,
                                     TestLabel "forallElimSize_Mixed_1" test53,
                                     TestLabel "forallElimSize_Mixed_2" test54,
                                     TestLabel "forallElimSize_Misaligned_1" test55,
                                     TestLabel "forallElimSize_Misaligned_1" test56,
                                     TestLabel "randomSampleSize_Empty_1" test57,
                                     TestLabel "randomSampleSize_Empty_2" test58,
                                     TestLabel "randomSampleSize_1Plain_1" test59,
                                     TestLabel "randomSampleSize_1Plain_2" test60,
                                     TestLabel "randomSampleSize_3Plain_1" test61,
                                     TestLabel "randomSampleSize_3Plain_2" test62,
                                     TestLabel "randomSampleSize_1Rot_1" test63,
                                     TestLabel "randomSampleSize_1Rot_2" test64,
                                     TestLabel "randomSampleSize_1Rot_2Circ_1" test65,
                                     TestLabel "randomSampleSize_1Rot_2Circ_2" test66,
                                     TestLabel "randomSampleSize_3Rot_1" test67,
                                     TestLabel "randomSampleSize_3Rot_2" test68,
                                     TestLabel "randomSampleSize_3Rot_2Circ_1" test69,
                                     TestLabel "randomSampleSize_3Rot_2Circ_2" test70,
                                     TestLabel "randomSampleSize_Mixed_1" test71,
                                     TestLabel "randomSampleSize_Mixed_2" test72,
                                     TestLabel "randomSampleSize_Misaligned_1" test73,
                                     TestLabel "randomSampleSize_Misaligned_1" test74]

main = defaultMain tests


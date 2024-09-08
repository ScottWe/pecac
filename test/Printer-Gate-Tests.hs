module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Pecac.Parser.Syntax
import Pecac.Printer.Gate

-----------------------------------------------------------------------------------------
-- Test cases.

test1 = TestCase (assertEqual "Can print plain gates (1/2)."
                              "x qs"
                              (printGate $ Gate gate))
    where gate = PlainGate "x" [QVar "qs"]

test2 = TestCase (assertEqual "Can print plain gates (2/2)."
                              "cx qs, q[5]"
                              (printGate $ Gate gate))
    where gate = PlainGate "cx" [QVar "qs", QReg "q" 5]

test3 = TestCase (assertEqual "Can print rotation gates (1/2)."
                              "rz(2 * param[1]) reg"
                              (printGate $ Gate gate))
    where expr = Times (ConstNat 2) (CellId "param" 1)
          gate = RotGate "rz" expr [QVar "reg"]

test4 = TestCase (assertEqual "Can print rotation gates (2/2)."
                              "crz(2 * param[1]) reg, qs[5]"
                              (printGate $ Gate gate))
    where expr = Times (ConstNat 2) (CellId "param" 1)
          gate = RotGate "crz" expr [QVar "reg", QReg "qs" 5]

test5 = TestCase (assertEqual "Can print plain gates with control modifiers."
                              "ctrl @ ch qs, q[5], q[7]"
                              (printGate $ CtrlMod $ Gate gate))
    where gate = PlainGate "ch" [QVar "qs", QReg "q" 5, QReg "q" 7]

test6 = TestCase (assertEqual "Can print plain gates with negative control modifiers."
                              "negctrl @ cswap qvar, q[2], r[9]"
                              (printGate $ NegCtrlMod $ Gate gate))
    where gate = PlainGate "cswap" [QVar "qvar", QReg "q" 2, QReg "r" 9]

test7 = TestCase (assertEqual "Can print plain gates with inversion modifiers."
                              "inv @ cswap qvar, q[2]"
                              (printGate $ InvMod $ Gate gate))
    where gate = PlainGate "cswap" [QVar "qvar", QReg "q" 2]

test8 = TestCase (assertEqual "Can print plain gates with mixed modifiers."
                              "inv @ ctrl @ inv @ negctrl @ t qvar, q[2]"
                              (printGate gmod))
    where gate = PlainGate "t" [QVar "qvar", QReg "q" 2]
          gmod = InvMod $ CtrlMod $ InvMod $ NegCtrlMod $ Gate gate

test9 = TestCase (assertEqual "Can print rotation gates with mixed modifiers."
                              "inv @ ctrl @ inv @ negctrl @ rz(2 * param[1]) qvar, q[2]"
                              (printGate gmod))
    where expr = Times (ConstNat 2) (CellId "param" 1)
          gate = RotGate "rz" expr [QVar "qvar", QReg "q" 2]
          gmod = InvMod $ CtrlMod $ InvMod $ NegCtrlMod $ Gate gate

test10 = TestCase (assertEqual "Can print a gate without operands."
                               "gphase(pi)"
                               (printGate $ Gate $ RotGate "gphase" Pi []))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "printGate_Plain_1" test1,
                                     TestLabel "printGate_Plain_2" test2,
                                     TestLabel "printGate_Rot_1" test3,
                                     TestLabel "printGate_Rot_2" test4,
                                     TestLabel "printGate_CtrlMod" test5,
                                     TestLabel "printGate_NegCtrlMod" test6,
                                     TestLabel "printGate_InvMod" test7,
                                     TestLabel "printGate_MixedMods" test8,
                                     TestLabel "printGate_RotMixedMods" test9,
                                     TestLabel "printGate_NoOperands" test10]

main = defaultMain tests

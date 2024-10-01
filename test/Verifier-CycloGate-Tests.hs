module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Complex.Cyclotomic as Cyclotomic
import Data.Maybe
import Data.Ratio ((%))
import Pecac.Affine
import Pecac.Analyzer.Gate
import Pecac.Analyzer.Revolution
import Pecac.Verifier.CycloGate
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

omega :: Cyclotomic.Cyclotomic
omega = Cyclotomic.e 8

mat_I :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_I = Matrix.iden 2

-----------------------------------------------------------------------------------------
-- gateToMat: Unmodified Plain Gates and Placement

mat_X :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_X = Matrix.build [[zero, one],
                     [one,  zero]]

mat_Y :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_Y = Matrix.build [[zero, -img],
                      [img,  zero]]

mat_Z :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_Z = Matrix.build [[one,  zero],
                      [zero, -one]]

mat_H :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_H = Matrix.build [[one / sqrt2, one / sqrt2],
                      [one / sqrt2, -one / sqrt2]]

mat_S :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_S = Matrix.build [[one,  zero],
                      [zero, img]]

mat_Sdg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_Sdg = Matrix.build [[one,  zero],
                        [zero, -img]]

mat_T :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_T = Matrix.build [[one,  zero],
                      [zero, omega]]

mat_Tdg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_Tdg = Matrix.build [[one,  zero],
                        [zero, omega^7]]

mat_SX :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_SX = Matrix.build [[(one + img) / two, (one - img) / two],
                       [(one - img) / two, (one + img) / two]]

mat_CX :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_CX = Matrix.build [[one,  zero, zero, zero],
                       [zero, one,  zero, zero],
                       [zero, zero, zero, one],
                       [zero, zero, one,  zero]]

mat_CY :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_CY = Matrix.build [[one,  zero, zero, zero],
                       [zero, one,  zero, zero],
                       [zero, zero, zero, -img],
                       [zero, zero, img,  zero]]

mat_CZ :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_CZ = Matrix.build [[one,  zero, zero, zero],
                       [zero, one,  zero, zero],
                       [zero, zero, one,  zero],
                       [zero, zero, zero, -one]]

mat_CH :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_CH = Matrix.build [[one,  zero, zero,        zero],
                       [zero, one,  zero,        zero],
                       [zero, zero, one / sqrt2, one / sqrt2],
                       [zero, zero, one / sqrt2, -one / sqrt2]]

mat_CCX :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_CCX = Matrix.build [[one,  zero, zero, zero, zero, zero, zero, zero],
                        [zero, one,  zero, zero, zero, zero, zero, zero],
                        [zero, zero, one,  zero, zero, zero, zero, zero],
                        [zero, zero, zero, one,  zero, zero, zero, zero],
                        [zero, zero, zero, zero, one,  zero, zero, zero],
                        [zero, zero, zero, zero, zero, one,  zero, zero],
                        [zero, zero, zero, zero, zero, zero, zero, one],
                        [zero, zero, zero, zero, zero, zero, one,  zero]]

mat_CSwap :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_CSwap = Matrix.build [[one,  zero, zero, zero, zero, zero, zero, zero],
                          [zero, one,  zero, zero, zero, zero, zero, zero],
                          [zero, zero, one,  zero, zero, zero, zero, zero],
                          [zero, zero, zero, one,  zero, zero, zero, zero],
                          [zero, zero, zero, zero, one,  zero, zero, zero],
                          [zero, zero, zero, zero, zero, zero, one , zero],
                          [zero, zero, zero, zero, zero, one,  zero, zero],
                          [zero, zero, zero, zero, zero, zero, zero, one]]

mat_IXII :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_IXII = Matrix.kroneckerProduct mat_I mat_XII
    where mat_XII = Matrix.kroneckerProduct mat_X $ Matrix.iden 4

mat_IIYI :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_IIYI = Matrix.kroneckerProduct (Matrix.iden 4) mat_YI
    where mat_YI = Matrix.kroneckerProduct mat_Y mat_I

mat_IZI :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_IZI = Matrix.kroneckerProduct mat_I mat_ZI
    where mat_ZI = Matrix.kroneckerProduct mat_Z mat_I

mat_IIIIH :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_IIIIH = Matrix.kroneckerProduct (Matrix.iden 16) mat_H

mat_IIISdg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_IIISdg = Matrix.kroneckerProduct (Matrix.iden 8) mat_Sdg

mat_TdgI :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_TdgI = Matrix.kroneckerProduct mat_Tdg mat_I

mat_ISwp :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_ISwp = Matrix.kroneckerProduct mat_I swap

nullAngles :: [Revolution]
nullAngles = map rationalToRev [0%1]

test1 = TestCase (assertEqual "gateToMat handles modifier-free Pauli-X gates."
                              mat_IXII
                              (gateToMat 4 nullAngles (PlainSummary GateX configs)))
    where configs = GateConfigs False [] [1]

test2 = TestCase (assertEqual "gateToMat handles modifier-free Pauli-Y gates."
                              mat_IIYI
                              (gateToMat 4 angles (PlainSummary GateY configs)))
    where angles  = map rationalToRev [5%1]
          configs = GateConfigs False [] [2]

test3 = TestCase (assertEqual "gateToMat handles modifier-free Pauli-Z gates."
                              mat_IZI
                              (gateToMat 3 angles (PlainSummary GateZ configs)))
    where angles  = map rationalToRev [5%2]
          configs = GateConfigs False [] [1]

test4 = TestCase (assertEqual "gateToMat handles modifier-free Hadamard gates."
                              mat_IIIIH
                              (gateToMat 5 angles (PlainSummary GateH configs)))
    where angles  = map rationalToRev [4%2]
          configs = GateConfigs False [] [4]

test5 = TestCase (assertEqual "gateToMat handles modifier-free S gates."
                              mat_S
                              (gateToMat 1 nullAngles (PlainSummary GateS configs)))
    where configs = GateConfigs False [] [0]

test6 = TestCase (assertEqual "gateToMat handles modifier-free Sdg gates."
                              mat_IIISdg
                              (gateToMat 4 nullAngles (PlainSummary GateSdg configs)))
    where configs = GateConfigs False [] [3]

test7 = TestCase (assertEqual "gateToMat handles modifier-free T gates."
                              mat_T
                              (gateToMat 1 nullAngles (PlainSummary GateT configs)))
    where configs = GateConfigs False [] [0]

test8 = TestCase (assertEqual "gateToMat handles modifier-free Tdg gates."
                              mat_TdgI
                              (gateToMat 2 nullAngles (PlainSummary GateTdg configs)))
    where configs = GateConfigs False [] [0]

test9 = TestCase (assertEqual "gateToMat handles modifier-free SX gates."
                              mat_SX
                              (gateToMat 1 nullAngles (PlainSummary GateSX configs)))
    where configs = GateConfigs False [] [0]

test10 = TestCase (assertEqual "gateToMat handles modifier-free CX gates."
                               mat_CX
                               (gateToMat 2 nullAngles (PlainSummary GateCX configs)))
    where configs = GateConfigs False [] [0, 1]

test11 = TestCase (assertEqual "gateToMat handles modifier-free CY gates."
                               mat_CY
                               (gateToMat 2 nullAngles (PlainSummary GateCY configs)))
    where configs = GateConfigs False [] [0, 1]

test12 = TestCase (assertEqual "gateToMat handles modifier-free CZ gates."
                               (Matrix.compose swap $ Matrix.compose mat_CZ swap)
                               (gateToMat 2 nullAngles (PlainSummary GateCZ configs)))
    where configs = GateConfigs False [] [1, 0]

test13 = TestCase (assertEqual "gateToMat handles modifier-free CH gates."
                               (Matrix.compose swap $ Matrix.compose mat_CH swap)
                               (gateToMat 2 nullAngles (PlainSummary GateCH configs)))
    where configs = GateConfigs False [] [1, 0]

test14 = TestCase (assertEqual "gateToMat handles modifier-free swap gates."
                               mat_ISwp
                               (gateToMat 3 nullAngles (PlainSummary GateSwap configs)))
    where configs = GateConfigs False [] [2, 1]

test15 = TestCase (assertEqual "gateToMat handles modifier-free CCX gates."
                               (Matrix.compose mat_ISwp $ Matrix.compose mat_CCX mat_ISwp)
                               (gateToMat 3 nullAngles (PlainSummary GateCCX configs)))
    where configs = GateConfigs False [] [0, 2, 1]

test16 = TestCase (assertEqual "gateToMat handles modifier-free CSwap gates."
                               mat_CSwap
                               (gateToMat 3 nullAngles (PlainSummary GateCSwap configs)))
    where configs = GateConfigs False [] [0, 1, 2]

-----------------------------------------------------------------------------------------
-- gateToMat: Inverted Plain Gates

mat_SXdg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_SXdg = Matrix.build [[(one - img) / two, (one + img) / two],
                         [(one + img) / two, (one - img) / two]]

test17 = TestCase (assertEqual "gateToMat handles inverse Pauli-X gates."
                               mat_IXII
                               (gateToMat 4 nullAngles (PlainSummary GateX configs)))
    where configs = GateConfigs True [] [1]

test18 = TestCase (assertEqual "gateToMat handles inverse Pauli-Y gates."
                               mat_IIYI
                               (gateToMat 4 angles (PlainSummary GateY configs)))
    where angles  = map rationalToRev [5%1]
          configs = GateConfigs True [] [2]

test19 = TestCase (assertEqual "gateToMat handles inverse Pauli-Z gates."
                               mat_IZI
                               (gateToMat 3 angles (PlainSummary GateZ configs)))
    where angles  = map rationalToRev [5%2]
          configs = GateConfigs True [] [1]

test20 = TestCase (assertEqual "gateToMat handles inverse Hadamard gates."
                               mat_IIIIH
                               (gateToMat 5 angles (PlainSummary GateH configs)))
    where angles  = map rationalToRev [4%2]
          configs = GateConfigs True [] [4]

test21 = TestCase (assertEqual "gateToMat handles inverse S gates."
                                mat_IIISdg
                               (gateToMat 4 angles (PlainSummary GateS configs)))
    where angles  = map rationalToRev [4%2]
          configs = GateConfigs True [] [3]

test22 = TestCase (assertEqual "gateToMat handles inverse Sdg gates."
                               mat_S
                               (gateToMat 1 nullAngles (PlainSummary GateSdg configs)))
    where configs = GateConfigs True [] [0]

test23 = TestCase (assertEqual "gateToMat handles inverse T gates."
                               mat_TdgI
                               (gateToMat 2 nullAngles (PlainSummary GateT configs)))
    where configs = GateConfigs True [] [0]

test24 = TestCase (assertEqual "gateToMat handles inverse Tdg gates."
                               mat_T
                               (gateToMat 1 nullAngles (PlainSummary GateTdg configs)))
    where configs = GateConfigs True [] [0]

test25 = TestCase (assertEqual "gateToMat handles inverse SX gates."
                               mat_SXdg
                               (gateToMat 1 nullAngles (PlainSummary GateSX configs)))
    where configs = GateConfigs True [] [0]

test26 = TestCase (assertEqual "gateToMat handles inverse CX gates."
                               mat_CX
                               (gateToMat 2 nullAngles (PlainSummary GateCX configs)))
    where configs = GateConfigs True [] [0, 1]

test27 = TestCase (assertEqual "gateToMat handles inverse CY gates."
                               mat_CY
                               (gateToMat 2 nullAngles (PlainSummary GateCY configs)))
    where configs = GateConfigs True [] [0, 1]

test28 = TestCase (assertEqual "gateToMat handles inverse CZ gates."
                               (Matrix.compose swap $ Matrix.compose mat_CZ swap)
                               (gateToMat 2 nullAngles (PlainSummary GateCZ configs)))
    where configs = GateConfigs True [] [1, 0]

test29 = TestCase (assertEqual "gateToMat handles inverse CH gates."
                               (Matrix.compose swap $ Matrix.compose mat_CH swap)
                               (gateToMat 2 nullAngles (PlainSummary GateCH configs)))
    where configs = GateConfigs True [] [1, 0]

test30 = TestCase (assertEqual "gateToMat handles inverse swap gates."
                               mat_ISwp
                               (gateToMat 3 nullAngles (PlainSummary GateSwap configs)))
    where configs = GateConfigs True [] [2, 1]

test31 = TestCase (assertEqual "gateToMat handles inverse CCX gates."
                               (Matrix.compose mat_ISwp $ Matrix.compose mat_CCX mat_ISwp)
                               (gateToMat 3 nullAngles (PlainSummary GateCCX configs)))
    where configs = GateConfigs True [] [0, 2, 1]

test32 = TestCase (assertEqual "gateToMat handles inverse CSwap gates."
                               mat_CSwap
                               (gateToMat 3 nullAngles (PlainSummary GateCSwap configs)))
    where configs = GateConfigs True [] [0, 1, 2]

-----------------------------------------------------------------------------------------
-- gateToMat: Unmodified Rotation Gates

mat_rotx_45deg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_rotx_45deg = Matrix.build [[one / sqrt2, img / sqrt2],
                               [img / sqrt2, one / sqrt2]]

mat_rotx_90deg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_rotx_90deg = Matrix.build [[zero, img],
                               [img,  zero]]

mat_roty_45deg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_roty_45deg = Matrix.build [[one / sqrt2,  one / sqrt2],
                               [-one / sqrt2, one / sqrt2]]

mat_roty_90deg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_roty_90deg = Matrix.build [[zero, one],
                               [-one, zero]]

mat_rotz_45deg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_rotz_45deg = Matrix.build [[(one + img) / sqrt2, zero],
                               [zero,                (one - img) / sqrt2]]

mat_rotz_90deg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_rotz_90deg = Matrix.build [[img,  zero],
                               [zero, -img]]

mat_rotcx_45deg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_rotcx_45deg = Matrix.build [[one,  zero, zero,        zero],
                                [zero, one,  zero,        zero],
                                [zero, zero, one / sqrt2, img / sqrt2],
                                [zero, zero, img / sqrt2, one / sqrt2]]

mat_rotcy_45deg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_rotcy_45deg = Matrix.build [[one,  zero, zero,         zero],
                                [zero, one,  zero,         zero],
                                [zero, zero, one / sqrt2,  one / sqrt2],
                                [zero, zero, -one / sqrt2, one / sqrt2]]

mat_rotcz_45deg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_rotcz_45deg = Matrix.build [[one,  zero, zero,                zero],
                                [zero, one,  zero,                zero],
                                [zero, zero, (one + img) / sqrt2, zero],
                                [zero, zero, zero,                (one - img) / sqrt2]]

mat_I_rotx_I :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_I_rotx_I = Matrix.kroneckerProduct mat_I mat_rotx_I
    where mat_rotx_I = Matrix.kroneckerProduct mat_rotx_45deg mat_I

mat_II_rotx :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_II_rotx = Matrix.kroneckerProduct (Matrix.iden 4) mat_rotx_90deg

angles_45 :: [Revolution]
angles_45 = map rationalToRev [45%360]

angles_90 :: [Revolution]
angles_90 = map rationalToRev [90%360]

param1 :: Affine Rational Revolution
param1 = linear [1]

test33 = TestCase (assertEqual "gateToMat handles modifier-free RotX gates (1/3)."
                               (Matrix.iden 8)
                               (gateToMat 3 nullAngles (RotSummary RotX param1 configs)))
    where configs = GateConfigs False [] [0]

test34 = TestCase (assertEqual "gateToMat handles modifier-free RotX gates (2/3)."
                               mat_I_rotx_I
                               (gateToMat 3 angles_45 (RotSummary RotX param1 configs)))
    where configs = GateConfigs False [] [1]

test35 = TestCase (assertEqual "gateToMat handles modifier-free RotX gates (3/3)."
                               mat_II_rotx
                               (gateToMat 3 angles_90 (RotSummary RotX param1 configs)))
    where configs = GateConfigs False [] [2]

test36 = TestCase (assertEqual "gateToMat handles modifier-free RotY gates (1/3)."
                               mat_I
                               (gateToMat 1 nullAngles (RotSummary RotY param1 configs)))
    where configs = GateConfigs False [] [0]

test37 = TestCase (assertEqual "gateToMat handles modifier-free RotY gates (2/3)."
                               mat_roty_45deg
                               (gateToMat 1 angles_45 (RotSummary RotY param1 configs)))
    where configs = GateConfigs False [] [0]

test38 = TestCase (assertEqual "gateToMat handles modifier-free RotY gates (3/3)."
                               mat_roty_90deg
                               (gateToMat 1 angles_90 (RotSummary RotY param1 configs)))
    where configs = GateConfigs False [] [0]

test39 = TestCase (assertEqual "gateToMat handles modifier-free RotZ gates (1/3)."
                               mat_I
                               (gateToMat 1 nullAngles (RotSummary RotZ param1 configs)))
    where configs = GateConfigs False [] [0]

test40 = TestCase (assertEqual "gateToMat handles modifier-free RotZ gates (2/3)."
                               mat_rotz_45deg
                               (gateToMat 1 angles_45 (RotSummary RotZ param1 configs)))
    where configs = GateConfigs False [] [0]

test41 = TestCase (assertEqual "gateToMat handles modifier-free RotZ gates (3/3)."
                               mat_rotz_90deg
                               (gateToMat 1 angles_90 (RotSummary RotZ param1 configs)))
    where configs = GateConfigs False [] [0]

test42 = TestCase (assertEqual "gateToMat handles modifier-free RotCX gates."
                               mat_rotcx_45deg
                               (gateToMat 2 angles_45 (RotSummary RotCX param1 configs)))
    where configs = GateConfigs False [] [0, 1]

test43 = TestCase (assertEqual "gateToMat handles modifier-free RotCY gates."
                               (swap * mat_rotcy_45deg * swap)
                               (gateToMat 2 angles_45 (RotSummary RotCY param1 configs)))
    where configs = GateConfigs False [] [1, 0]

test44 = TestCase (assertEqual "gateToMat handles modifier-free RotCZ gates."
                               mat_rotcz_45deg
                               (gateToMat 2 angles_45 (RotSummary RotCZ param1 configs)))
    where configs = GateConfigs False [] [0, 1]

-----------------------------------------------------------------------------------------
-- gateToMat: Inverted Rotation Gates

mat_inv_rotx_90deg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_inv_rotx_90deg = Matrix.build [[zero, -img],
                                   [-img, zero]]

mat_inv_roty_90deg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_inv_roty_90deg = Matrix.build [[zero, -one],
                                   [one,  zero]]

mat_inv_rotz_90deg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_inv_rotz_90deg = Matrix.build [[-img, zero],
                                   [zero, img]]

mat_inv_rotcx_90deg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_inv_rotcx_90deg = Matrix.build [[one,  zero, zero, zero],
                                    [zero, one,  zero, zero],
                                    [zero, zero, zero, -img],
                                    [zero, zero, -img, zero]]

mat_inv_rotcy_90deg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_inv_rotcy_90deg = Matrix.build [[one,  zero, zero, zero],
                                    [zero, one,  zero, zero],
                                    [zero, zero, zero, -one],
                                    [zero, zero, one,  zero]]

mat_inv_rotcz_90deg :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_inv_rotcz_90deg = Matrix.build [[one,  zero, zero, zero],
                                    [zero, one,  zero, zero],
                                    [zero, zero, -img, zero],
                                    [zero, zero, zero, img]]

test45 = TestCase (assertEqual "gateToMat handles inverted RotX gates."
                               mat_inv_rotx_90deg
                               (gateToMat 1 angles_90 (RotSummary RotX param1 configs)))
    where configs = GateConfigs True [] [0]

test46 = TestCase (assertEqual "gateToMat handles inverted RotY gates."
                               mat_inv_roty_90deg
                               (gateToMat 1 angles_90 (RotSummary RotY param1 configs)))
    where configs = GateConfigs True [] [0]

test47 = TestCase (assertEqual "gateToMat handles inverted RotZ gates."
                               mat_inv_rotz_90deg
                               (gateToMat 1 angles_90 (RotSummary RotZ param1 configs)))
    where configs = GateConfigs True [] [0]

test48 = TestCase (assertEqual "gateToMat handles inverted RotCX gates."
                               mat_inv_rotcx_90deg
                               (gateToMat 2 angles_90 (RotSummary RotCX param1 configs)))
    where configs = GateConfigs True [] [0, 1]

test49 = TestCase (assertEqual "gateToMat handles inverted RotCY gates."
                               mat_inv_rotcy_90deg
                               (gateToMat 2 angles_90 (RotSummary RotCY param1 configs)))
    where configs = GateConfigs True [] [0, 1]

test50 = TestCase (assertEqual "gateToMat handles inverted RotCZ gates."
                               mat_inv_rotcz_90deg
                               (gateToMat 2 angles_90 (RotSummary RotCZ param1 configs)))
    where configs = GateConfigs True [] [0, 1]

-----------------------------------------------------------------------------------------
-- gateToMat: Linear Sums of Angles

test51 = TestCase (assertEqual "gateToMat handles linear sums of angles for plain gates."
                               mat_X
                               (gateToMat 1 angles (PlainSummary GateX configs)))
    where angles  = map rationalToRev [45%1, 90%1]
          configs = GateConfigs False [] [0]

test52 = TestCase (assertEqual "gateToMat handles linear sums of angles on rots (1/3)."
                               mat_roty_45deg
                               (gateToMat 1 angles (RotSummary RotY coeffs configs)))
    where angles  = map rationalToRev [25%360, 25%(2*360), 5%360]
          coeffs  = linear [1, 2, -1]
          configs = GateConfigs False [] [0]

test53 = TestCase (assertEqual "gateToMat handles modifier-free RotY gates (2/3)."
                               mat_roty_90deg
                               (gateToMat 1 angles (RotSummary RotY coeffs configs)))
    where angles  = map rationalToRev [-110%360, 20%(11*360)]
          coeffs  = linear [-1, -11]
          configs = GateConfigs False [] [0]

test54 = TestCase (assertEqual "gateToMat handles modifier-free RotY gates (3/3)."
                               mat_I
                               (gateToMat 1 angles (RotSummary RotY coeffs configs)))
    where angles  = map rationalToRev [1%(11*360), 1%(7*360), 3%(11*360), 3%(7*360)]
          coeffs  = linear [3, 6, -1, -2]
          configs = GateConfigs False [] [0]

-----------------------------------------------------------------------------------------
-- gateToMat: Controlled

mat_NX :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_NX = Matrix.build [[zero, one,  zero, zero],
                       [one,  zero, zero, zero],
                       [zero, zero, one,  zero],
                       [zero, zero, zero, one]]

mat_CNX :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_CNX = Matrix.build [[one,  zero, zero, zero, zero, zero, zero, zero],
                        [zero, one,  zero, zero, zero, zero, zero, zero],
                        [zero, zero, one,  zero, zero, zero, zero, zero],
                        [zero, zero, zero, one,  zero, zero, zero, zero],
                        [zero, zero, zero, zero, zero, one,  zero, zero],
                        [zero, zero, zero, zero, one,  zero, zero, zero],
                        [zero, zero, zero, zero, zero, zero, one,  zero],
                        [zero, zero, zero, zero, zero, zero, zero, one]]

angles_45_90 :: [Revolution]
angles_45_90 = map rationalToRev [45%1, 90%1]

test55 = TestCase (assertEqual "gateToMat handles positive controls."
                               mat_CX
                               (gateToMat 2 angles_45_90 (PlainSummary GateX configs)))
    where configs = GateConfigs False [Pos] [0, 1]

test56 = TestCase (assertEqual "gateToMat handles negative controls."
                               mat_NX
                               (gateToMat 2 angles_45_90 (PlainSummary GateX configs)))
    where configs = GateConfigs False [Neg] [0, 1]

test57 = TestCase (assertEqual "gateToMat handles mixed controls."
                               mat_CNX
                               (gateToMat 3 angles_45_90 (PlainSummary GateX configs)))
    where configs = GateConfigs False [Pos, Neg] [0, 1, 2]

test58 = TestCase (assertEqual "gateToMat can permute inputs to controlled gates."
                               (Matrix.compose mat_ISwp $ Matrix.compose mat_CCX mat_ISwp)
                               (gateToMat 3 nullAngles (PlainSummary GateX configs)))
    where configs = GateConfigs False [Pos, Pos] [0, 2, 1]

-----------------------------------------------------------------------------------------
-- idGate

test59 = TestCase (assertEqual "The wrapper idGate handles dimensions correctly (1/3)."
                               mat_I
                               (idGate 1))

test60 = TestCase (assertEqual "The wrapper idGate handles dimensions correctly (2/3)."
                               (Matrix.iden 4)
                               (idGate 2))

test61 = TestCase (assertEqual "The wrapper idGate handles dimensions correctly (3/3)."
                               (Matrix.iden 8)
                               (idGate 3))

-----------------------------------------------------------------------------------------
-- Full support for affine q-linear sums.

test62 = TestCase (assertEqual "gateToMat handles rational coefficients."
                               mat_roty_45deg
                               (gateToMat 1 angles_90 (RotSummary RotY params configs)))
    where params  = linear [1 % 2]
          configs = GateConfigs False [] [0]

test63 = TestCase (assertEqual "gateToMat handles constant parameters."
                               mat_roty_45deg
                               (gateToMat 1 angles_90 (RotSummary RotY params configs)))
    where params  = lit $ rationalToRev $ 1 % 8
          configs = GateConfigs False [] [0]

-----------------------------------------------------------------------------------------
-- Global Phase.

mat_gphase_1 :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_gphase_1 = Matrix.build [[one,  zero],
                             [zero, one]]

mat_gphase_2 :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_gphase_2 = Matrix.build [[img,  zero, zero, zero],
                             [zero, img,  zero, zero],
                             [zero, zero, img,  zero],
                             [zero, zero, zero, img]]

mat_gphase_3 :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_gphase_3 = Matrix.build [[-img, zero, zero, zero, zero, zero, zero, zero],
                             [zero, -img, zero, zero, zero, zero, zero, zero],
                             [zero, zero, -img, zero, zero, zero, zero, zero],
                             [zero, zero, zero, -img, zero, zero, zero, zero],
                             [zero, zero, zero, zero, -img, zero, zero, zero],
                             [zero, zero, zero, zero, zero, -img, zero, zero],
                             [zero, zero, zero, zero, zero, zero, -img, zero],
                             [zero, zero, zero, zero, zero, zero, zero, -img]]

mat_gphase_4 :: Matrix.Matrix Cyclotomic.Cyclotomic
mat_gphase_4 = Matrix.build [[one,  zero],
                             [zero, -img]]

test64 = TestCase (assertEqual "A global phase gate acts as a scalar (1/3)."
                               mat_gphase_1
                               (gateToMat 1 angles_90 (RotSummary GPhase params configs)))
    where params  = lit $ rationalToRev 0
          configs = GateConfigs False [] []

test65 = TestCase (assertEqual "A global phase gate acts as a scalar (2/3)."
                               mat_gphase_2
                               (gateToMat 2 angles_90 (RotSummary GPhase params configs)))
    where params  = lit $ rationalToRev $ 1 % 4
          configs = GateConfigs False [] []

test66 = TestCase (assertEqual "A global phase gate acts as a scalar (3/3)."
                               mat_gphase_3
                               (gateToMat 3 angles_90 (RotSummary GPhase params configs)))
    where params  = lit $ rationalToRev $ 3 % 4
          configs = GateConfigs False [] []

test67 = TestCase (assertEqual "Can add a control to a global phase gate."
                               mat_gphase_4
                               (gateToMat 1 angles_90 (RotSummary GPhase params configs)))
    where params  = lit $ rationalToRev $ 3 % 4
          configs = GateConfigs False [Pos] [0]

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "gateToMat_NoMod_GateX" test1,
                                     TestLabel "gateToMat_NoMod_GateY" test2,
                                     TestLabel "gateToMat_NoMod_GateZ" test3,
                                     TestLabel "gateToMat_NoMod_GateH" test4,
                                     TestLabel "gateToMat_NoMod_GateS" test5,
                                     TestLabel "gateToMat_NoMod_GateSdg" test6,
                                     TestLabel "gateToMat_NoMod_GateT" test7,
                                     TestLabel "gateToMat_NoMod_GateTdg" test8,
                                     TestLabel "gateToMat_NoMod_GateSX" test9,
                                     TestLabel "gateToMat_NoMod_GateCX" test10,
                                     TestLabel "gateToMat_NoMod_GateCY" test11,
                                     TestLabel "gateToMat_NoMod_GateCZ" test12,
                                     TestLabel "gateToMat_NoMod_GateCH" test13,
                                     TestLabel "gateToMat_NoMod_GateSwap" test14,
                                     TestLabel "gateToMat_NoMod_GateCCX" test15,
                                     TestLabel "gateToMat_NoMod_GateCSwap" test16,
                                     TestLabel "gateToMat_Inv_GateX" test17,
                                     TestLabel "gateToMat_Inv_GateY" test18,
                                     TestLabel "gateToMat_Inv_GateZ" test19,
                                     TestLabel "gateToMat_Inv_GateH" test20,
                                     TestLabel "gateToMat_Inv_GateS" test21,
                                     TestLabel "gateToMat_Inv_GateSdg" test22,
                                     TestLabel "gateToMat_Inv_GateT" test23,
                                     TestLabel "gateToMat_Inv_GateTdg" test24,
                                     TestLabel "gateToMat_Inv_GateSX" test25,
                                     TestLabel "gateToMat_Inv_GateCX" test26,
                                     TestLabel "gateToMat_Inv_GateCY" test27,
                                     TestLabel "gateToMat_Inv_GateCZ" test28,
                                     TestLabel "gateToMat_Inv_GateCH" test29,
                                     TestLabel "gateToMat_Inv_GateSwap" test30,
                                     TestLabel "gateToMat_Inv_GateCCX" test31,
                                     TestLabel "gateToMat_Inv_GateCSwap" test32,
                                     TestLabel "gateToMat_NodMod_RotX_0deg" test33,
                                     TestLabel "gateToMat_NodMod_RotX_45deg" test34,
                                     TestLabel "gateToMat_NodMod_RotX_90deg" test35,
                                     TestLabel "gateToMat_NodMod_RotY_0deg" test36,
                                     TestLabel "gateToMat_NodMod_RotY_45deg" test37,
                                     TestLabel "gateToMat_NodMod_RotY_90deg" test38,
                                     TestLabel "gateToMat_NodMod_RotZ_0deg" test39,
                                     TestLabel "gateToMat_NodMod_RotZ_45deg" test40,
                                     TestLabel "gateToMat_NodMod_RotZ_90deg" test41,
                                     TestLabel "gateToMat_NodMod_RotCX_45deg" test42,
                                     TestLabel "gateToMat_NodMod_RotCY_45deg" test43,
                                     TestLabel "gateToMat_NodMod_RotCZ_45deg" test44,
                                     TestLabel "gateToMat_Inv_RotX_90deg" test45,
                                     TestLabel "gateToMat_Inv_RotY_90deg" test46,
                                     TestLabel "gateToMat_Inv_RotZ_90deg" test47,
                                     TestLabel "gateToMat_Inv_RotCX_90deg" test48,
                                     TestLabel "gateToMat_Inv_RotCY_90deg" test49,
                                     TestLabel "gateToMat_Inv_RotCZ_90deg" test50,
                                     TestLabel "gateToMat_LinSum_Plain" test51,
                                     TestLabel "gateToMat_LinSum_Rot_1" test52,
                                     TestLabel "gateToMat_LinSum_Rot_2" test53,
                                     TestLabel "gateToMat_LinSum_Rot_3" test54,
                                     TestLabel "gateToMat_Ctrl_Pos" test55,
                                     TestLabel "gateToMat_Ctrl_Neg" test56,
                                     TestLabel "gateToMat_Ctrl_Mixed" test57,
                                     TestLabel "gateToMat_Ctrl_Perm" test58,
                                     TestLabel "idGate_1" test59,
                                     TestLabel "idGate_2" test60,
                                     TestLabel "idGate_3" test61,
                                     TestLabel "RationalCoeff" test62,
                                     TestLabel "ConstantTerm" test63,
                                     TestLabel "gateToMat_GPhase_1" test64,
                                     TestLabel "gateToMat_GPhase_2" test65,
                                     TestLabel "gateToMat_GPhase_3" test66,
                                     TestLabel "gateToMat_GPhase_Ctrl" test67]

main = defaultMain tests

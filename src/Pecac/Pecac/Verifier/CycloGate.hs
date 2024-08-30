-- | A library for constructing quantum gates over the cyclotomic numbers.

module Pecac.Verifier.CycloGate
  ( CycMat
  , gateToMat
  , idGate
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.Ratio ((%))
import Pecac.Analyzer.Gate
  ( GateConfigs (..)
  , GateSummary (..)
  , PlainName (..)
  , Polarity (..)
  , RotName (..)
  , isInverted
  )
import Pecac.Verifier.MatrixGate
  ( addCtrlToMatrix
  , addNegCtrlToMatrix
  , applyAt
  )

import qualified Data.Complex.Cyclotomic as Cyclotomic
import qualified Pecac.Verifier.Matrix as Matrix

-----------------------------------------------------------------------------------------
-- * Gate Entries.

-- | The inclusion of 0 into the cyclotomic numbers.
zero :: Cyclotomic.Cyclotomic
zero = fromInteger 0

-- | The inclusion of 1 into the cyclotomic numbers.
one :: Cyclotomic.Cyclotomic
one = fromInteger 1

-- | The inclusion of 2 into the cyclotomic numbers.
two :: Cyclotomic.Cyclotomic
two = fromInteger 2

-- | The inclusion of 1/2 into the cyclotomic numbers.
half :: Cyclotomic.Cyclotomic
half = one / two

-- | The cyclotomic number sqrt(2).
sqrt2 :: Cyclotomic.Cyclotomic
sqrt2 = Cyclotomic.sqrtInteger 2

-- | The cyclotomic number i.
img :: Cyclotomic.Cyclotomic
img = Cyclotomic.i

-- | The cyclotomic number omega = exp(i*pi/8).
omega :: Cyclotomic.Cyclotomic
omega = Cyclotomic.e 8

-----------------------------------------------------------------------------------------
-- * Plain Gate Definitions

-- | Utility type for cyclotomic matrices.
type CycMat = Matrix.Matrix Cyclotomic.Cyclotomic

-- | Represents a gate which applies a phase of p to a qubit if and only if it is in
-- basis state |1>.
pgate :: Cyclotomic.Cyclotomic -> CycMat
pgate p = Matrix.build [[one,  zero],
                        [zero, p]]

-- | The Pauli-X gate as a cyclotomic operator.
matX :: CycMat
matX = Matrix.build [[zero, one],
                     [one,  zero]]

-- | The Pauli-Y gate as a cyclotomic operator.
matY :: CycMat
matY = Matrix.build [[zero, -img],
                     [img,  zero]]

-- | The Pauli-Z gate as a cyclotomic operator.
matZ :: CycMat
matZ = pgate $ -one

-- | The Pauli-H gate as a cyclotomic operator.
matH :: CycMat
matH = Matrix.scale (sqrt2 / two) $ Matrix.build [[one,  one],
                                                  [one, -one]]

-- | The S gate (i.e., sqrt(Z)) as a cyclotomic operator.
matS :: CycMat
matS = pgate img

-- | The inverse of the S gate.
matSdg :: CycMat
matSdg = pgate $ -img

-- | The T gate (i.e., sqrt(S)) as a cyclotomic operator.
matT :: CycMat
matT = pgate omega

-- | The inverse of the T gate.
matTdg :: CycMat
matTdg = pgate $ -img * omega

-- | The sqrt(X) as a cyclotomic operator.
matSX :: CycMat
matSX = Matrix.scale half $ Matrix.build [[one + img, one - img],
                                          [one - img, one + img]]

-- | The inverse of the SX gate.
matSXdg :: CycMat
matSXdg = Matrix.compose matX matSX

-- | The swap gate as a cyclotomic operator.
matSwap :: CycMat
matSwap = Matrix.swap

-----------------------------------------------------------------------------------------
-- * Modified Plain Gates.

-- | A controlled version of matX.
matCX :: CycMat
matCX = addCtrlToMatrix matX

-- | A controlled version of matY.
matCY :: CycMat
matCY = addCtrlToMatrix matY

-- | A controlled version of matZ.
matCZ :: CycMat
matCZ = addCtrlToMatrix matZ

-- | A controlled version of matH.
matCH :: CycMat
matCH = addCtrlToMatrix matH

-- | A controlled version of swap.
matCSwap :: CycMat
matCSwap = addCtrlToMatrix matSwap

-- | The Toffoli gate as a cyclotomic operator.
matCCX :: CycMat
matCCX = addCtrlToMatrix matCX

-----------------------------------------------------------------------------------------
-- * Rotation Gate Definitions.

-- | Takes as input a unitary Hermitian matrix to exponentiate (mat), whether the
-- exponential should be inverted, and an angle theta (in revolutions). Returns the
-- operator exp(i*mat*tau) where tau = theta when the circuit is not inverted, or
-- tau = -theta otherwise. Note that if mat is not unitary Hermitian matrix, then the
-- result will be incorrect (in particular, a special decomposition is used).
makeExponential :: CycMat -> Bool -> Rational -> CycMat
makeExponential mat False theta = Matrix.add cosmat sinmat
    where (n, m) = Matrix.size mat
          cosmat = Matrix.scale (Cyclotomic.cosRev theta) $ Matrix.iden n
          sinmat = Matrix.scale (img * Cyclotomic.sinRev theta) mat
makeExponential mat True theta = makeExponential mat False $ -theta

-- | Instantiates makeExponential for the Pauli-X operator.
matRX :: Bool -> Rational -> CycMat
matRX = makeExponential matX

-- | Instantiates makeExponential for the Pauli-Y operator.
matRY :: Bool -> Rational -> CycMat
matRY = makeExponential matY

-- | Instantiates makeExponential for the Pauli-Z operator.
matRZ :: Bool -> Rational -> CycMat
matRZ = makeExponential matZ

-----------------------------------------------------------------------------------------
-- * Modified Rotation Gates.

-- | A controlled version of matRX.
matCRX :: Bool -> Rational -> CycMat
matCRX inv = addCtrlToMatrix . matRX inv

-- | A controlled version of matRY.
matCRY :: Bool -> Rational -> CycMat
matCRY inv = addCtrlToMatrix . matRY inv

-- | A controlled version of matRZ.
matCRZ :: Bool -> Rational -> CycMat
matCRZ inv = addCtrlToMatrix . matRZ inv

-----------------------------------------------------------------------------------------
-- * Modified Rotation Gates.

-- | Converts a plain gate name to the corresponding cyclotomic operator, taking into
-- account gate inversion.
makePlain :: PlainName -> Bool -> CycMat
makePlain GateX     _     = matX
makePlain GateY     _     = matY
makePlain GateZ     _     = matZ
makePlain GateH     _     = matH
makePlain GateS     True  = matSdg
makePlain GateS     False = matS
makePlain GateSdg   True  = matS
makePlain GateSdg   False = matSdg
makePlain GateT     True  = matTdg
makePlain GateT     False = matT
makePlain GateTdg   True  = matT
makePlain GateTdg   False = matTdg
makePlain GateSX    True  = matSXdg
makePlain GateSX    False = matSX
makePlain GateCX    _     = matCX
makePlain GateCY    _     = matCY
makePlain GateCZ    _     = matCZ
makePlain GateCH    _     = matCH
makePlain GateSwap  _     = matSwap
makePlain GateCCX   _     = matCCX
makePlain GateCSwap _     = matCSwap

-- | Converts a rotation gate name to a corresponding cyclotomic operator, taking into
-- account gate inversion, and the angle of rotation.
makeRot :: RotName -> Bool -> Rational -> CycMat
makeRot RotX  = matRX
makeRot RotY  = matRY
makeRot RotZ  = matRZ
makeRot RotCX = matCRX
makeRot RotCY = matCRY
makeRot RotCZ = matCRZ

-- | Helper method to add controls to a cyclotomic operator.
addCtrls :: [Polarity] -> CycMat -> CycMat
addCtrls []          mat = mat
addCtrls (Pos:ctrls) mat = addCtrlToMatrix $ addCtrls ctrls mat
addCtrls (Neg:ctrls) mat = addNegCtrlToMatrix $ addCtrls ctrls mat

-- | Implementation details for gateToMat. This method strips away the gate specific
-- information, taking instead the base matrix and the gate configurations.
gateToMatImpl :: Int -> CycMat -> GateConfigs -> CycMat
gateToMatImpl n base (GateConfigs _ ctrls qubits) = applyAt n qubits cbase
    where cbase = addCtrls ctrls base

-- | Helper method to evaluate integer linear sums of angles, given the coefficients in
-- the sum, and an instantiation for each angle.
toArg :: [Integer] -> [Rational] -> Rational
toArg []     []     = 0
toArg (x:xs) (y:ys) = ((x % 1) * y) + toArg xs ys

-- | Takes as input the number of qubits in a circuit, an instantiation for each angle in
-- the circuit, and a gate summary. Returns the cyclotomic operator corresponding to this
-- gate summary, with respect to the circuit size (qubit count) and angle instantiations.
gateToMat :: Int -> [Rational] -> GateSummary -> CycMat
gateToMat n _ (PlainSummary name confs) = gateToMatImpl n base confs
    where base = makePlain name $ isInverted confs
gateToMat n thetas (RotSummary name coeffs confs) = gateToMatImpl n base confs
    where base = makeRot name (isInverted confs) $ toArg coeffs thetas

-- | Returns a (2^n)-dimensional identity matrix.
idGate :: Int -> CycMat
idGate n = Matrix.iden $ 2^n

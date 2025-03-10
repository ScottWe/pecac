-- | Problem-specific abstraction for OpenQASM 3 gates.

module Pecac.Analyzer.Gate
  ( GateConfigs (..)
  , GateSummary (..)
  , PlainName (..)
  , Polarity (..)
  , RotName (..)
  , createGlobalPhase
  , getConfigs
  , getPlainArity
  , getRotArity
  , isParameterized
  , plainNameToString
  , rotNameToString
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Pecac.Affine
  ( Affine
  , isConstant
  )
import Pecac.Analyzer.Revolution (Revolution)

-----------------------------------------------------------------------------------------
-- * Enumeration of Supported Gate Types.

-- | Supported named parameter-free gates.
data PlainName = GateX
               | GateY
               | GateZ
               | GateH
               | GateS
               | GateSdg
               | GateT
               | GateTdg
               | GateSX
               | GateCX
               | GateCY
               | GateCZ
               | GateCH
               | GateSwap
               | GateCCX
               | GateCSwap
               deriving (Show, Eq)

-- | Returns the default number of operands to a plain gate.
getPlainArity :: PlainName -> Int
getPlainArity GateX     = 1
getPlainArity GateY     = 1
getPlainArity GateZ     = 1
getPlainArity GateH     = 1
getPlainArity GateS     = 1
getPlainArity GateSdg   = 1
getPlainArity GateT     = 1
getPlainArity GateTdg   = 1
getPlainArity GateSX    = 1
getPlainArity GateCX    = 2
getPlainArity GateCY    = 2
getPlainArity GateCZ    = 2
getPlainArity GateCH    = 2
getPlainArity GateSwap  = 2
getPlainArity GateCCX   = 3
getPlainArity GateCSwap = 3

-- | Reverts a plain gate name to its string representation.
plainNameToString :: PlainName -> String
plainNameToString GateX     = "x"
plainNameToString GateY     = "y"
plainNameToString GateZ     = "z"
plainNameToString GateH     = "h"
plainNameToString GateS     = "s"
plainNameToString GateSdg   = "sdg"
plainNameToString GateT     = "t"
plainNameToString GateTdg   = "tdg"
plainNameToString GateSX    = "sx"
plainNameToString GateCX    = "cx"
plainNameToString GateCY    = "cy"
plainNameToString GateCZ    = "cz"
plainNameToString GateCH    = "ch"
plainNameToString GateSwap  = "swap"
plainNameToString GateCCX   = "ccx"
plainNameToString GateCSwap = "cswap"

-- | Supported named rotation gates.
data RotName = RotX
             | RotY
             | RotZ
             | RotCX
             | RotCY
             | RotCZ
             | GPhase
             | RotP
             | RotCP
             deriving (Show, Eq)

-- | Returns the default number of operands to a rotation gate.
getRotArity :: RotName -> Int
getRotArity RotX   = 1
getRotArity RotY   = 1
getRotArity RotZ   = 1
getRotArity RotCX  = 2
getRotArity RotCY  = 2
getRotArity RotCZ  = 2
getRotArity GPhase = 0
getRotArity RotP   = 1
getRotArity RotCP  = 2

-- | Reverts a rotation name to its string representation.
rotNameToString :: RotName -> String
rotNameToString RotX   = "rx"
rotNameToString RotY   = "ry"
rotNameToString RotZ   = "rz"
rotNameToString RotCX  = "crx"
rotNameToString RotCY  = "cry"
rotNameToString RotCZ  = "crz"
rotNameToString GPhase = "gphase"
rotNameToString RotP   = "p"
rotNameToString RotCP  = "cp"

-----------------------------------------------------------------------------------------
-- * Abstract Gate Description.

-- | Indicates whether is control is negative or positive.
data Polarity = Pos | Neg deriving (Show, Eq)

-- | The common configurations between plain gates and rotation gates. In order, these
-- are whether the gate is inverted, the list of controls for the gate, and the list of
-- operands to the gate.
data GateConfigs = GateConfigs { isInverted :: Bool
                               , controls   :: [Polarity]
                               , operands   :: [Int]
                               } deriving (Show, Eq)

-- | An abstract representation of a gate. For plain gates, this is simply the name of
-- the gate and its configurations. For rotation gates, this includes an additional
-- list of integers, which stores the coefficients of the paramteters theta_1 through to
-- theta_k (recall that the angle is a_1*theta_1 + ... + a_k*theta_k).
data GateSummary = PlainSummary PlainName GateConfigs
                 | RotSummary RotName (Affine Rational Revolution) GateConfigs
                 deriving (Show, Eq)

-- | Returns true if a summarized gate is parameterized. Note that a rotation gate with
-- a constant parameterization is not considered to be a parameterized gate.
isParameterized :: GateSummary -> Bool
isParameterized (PlainSummary _ _)      = False
isParameterized (RotSummary _ coeffs _) = not $ isConstant coeffs

-- | Extracts the configurations from a gate summary.
getConfigs :: GateSummary -> GateConfigs
getConfigs (PlainSummary _ confs) = confs
getConfigs (RotSummary _ _ confs) = confs

-- | Returns a global phase gate with the provided parameters.
createGlobalPhase :: Affine Rational Revolution -> GateSummary
createGlobalPhase params = RotSummary GPhase params configs
    where configs = GateConfigs False [] []

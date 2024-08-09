-- | Problem-specific abstraction for OpenQASM 3 gates.

module Pecac.Analyzer.Gate
  ( GateConfigs (..)
  , GateSummary (..)
  , PlainName (..)
  , Polarity (..)
  , RotName (..)
  , getPlainArity
  , getRotArity
  ) where

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

-- | Supported named rotation gates.
data RotName = RotX
             | RotY
             | RotZ
             | RotCX
             | RotCY
             | RotCZ
             deriving (Show, Eq)

-- | Returns the default number of operands to a rotation gate.
getRotArity :: RotName -> Int
getRotArity RotX  = 1
getRotArity RotY  = 1
getRotArity RotZ  = 1
getRotArity RotCX = 2
getRotArity RotCY = 2
getRotArity RotCZ = 2

-----------------------------------------------------------------------------------------
-- * Abstract Gate Description.

-- | Indicates whether is control is negative or positive.
data Polarity = Pos | Neg deriving (Show, Eq)

-- | The common configurations between plain gates and rotation gates. In order, these
-- are whether the gate is inverted, the list of controls for the gate, and the list of
-- operands to the gate.
data GateConfigs = GateConfigs Bool [Polarity] [Int] deriving (Show, Eq)

-- | An abstract representation of a gate. For plain gates, this is simply the name of
-- the gate and its configurations. For rotation gates, this includes an additional
-- list of integers, which stores the coefficients of the paramteters theta_1 through to
-- theta_k (recall that the angle is a_1*theta_1 + ... + a_k*theta_k).
data GateSummary = PlainSummary PlainName GateConfigs
                 | RotSummary RotName [Int] GateConfigs
                 deriving (Show, Eq)

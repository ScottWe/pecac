-- | Problem-specific abstraction for OpenQASM 3 gates.

module Pecac.Analyzer.Gate
  ( RotName (..)
  , PlainName (..)
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

-- | Supported named rotation gates.
data RotName = RotX
             | RotY
             | RotZ
             | RotCX
             | RotCY
             | RotCZ
             deriving (Show, Eq)

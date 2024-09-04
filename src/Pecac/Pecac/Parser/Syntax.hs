-- | Abstract syntax for a pecac-compliant OpenQASM 3 file.

module Pecac.Parser.Syntax
  ( BaseGate (..)
  , Expr (..)
  , Gate (..)
  , Operand (..)
  , ParamDecl (..)
  , QASMFile (..)
  , QubitDecl (..)
  , Stmt (..)
  ) where

-----------------------------------------------------------------------------------------
-- * QASM File Strucutre.

-- | The snytactic structure of an OpenQASM file which conforms to pecac.
data QASMFile = QASMFile String [String] [Stmt] deriving (Show, Eq)

-- | The supported statements in a pecac-compliant OpenQASM 3 file.
data Stmt = GateStmt Gate
          | ParamDeclStmt ParamDecl
          | QubitDeclStmt QubitDecl
          deriving (Show, Eq)

-----------------------------------------------------------------------------------------
-- * QASM Gates.

-- | The modifier-free gates supported in a pecac-compliant OpenQASM 3 file.
data BaseGate = PlainGate String [Operand]
              | RotGate String Expr [Operand]
              deriving (Show, Eq)

-- | The supported gates in a pecac-compliant OpenQASM 3 file.
data Gate = Gate BaseGate
          | CtrlMod Gate
          | NegCtrlMod Gate
          | InvMod Gate
          deriving (Show, Eq)

-- | Abstract gate operands.
data Operand = QVar String
             | QReg String Int
             deriving (Show, Eq)

-- | Abstract expressions of type angle.
data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Brack Expr
          | Negate Expr
          | VarId String
          | CellId String Int
          | ConstNat Integer
          deriving (Show, Eq)

-----------------------------------------------------------------------------------------
-- * QASM Declarations.

-- | Declaration types for parameters (input angles).
data ParamDecl = ParamVarDecl String
               | ParamArrDecl String Int
               deriving (Show, Eq)

-- | Declaration types for qubits.
data QubitDecl = QubitVarDecl String
               | QubitArrDecl String Int
               deriving (Show, Eq)

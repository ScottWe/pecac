-- | Conversion from AST statements to abstract parameterized circuits.

module Pecac.Parser.Problem
  ( CircErr (..)
  , StmtErr (..)
  , qasmToParamCirc
  , summarizeStmts
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Pecac.Analyzer.Gate (GateSummary)
import Pecac.Analyzer.Problem
  ( ParamArr (..)
  , ParamCirc (..)
  , QubitReg (..)
  )
import Pecac.Either
  ( branchRight
  , updateLeft
  , updateRight
  )
import Pecac.Parser.Gate
  ( GateErr (..)
  , summarizeGate
  )
import Pecac.Parser.Syntax
  ( Gate (..)
  , ParamDecl (..)
  , QASMFile (..)
  , QubitDecl (..)
  , Stmt (..)
  )

import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NonEmpty

-----------------------------------------------------------------------------------------
-- * Statement List Parsing.

-- | Explanations for circuit parsing failures.
data StmtErr = DuplicateName String
             | MissingParams
             | MissingQubits
             | UnexpectParamDecl ParamDecl
             | UnexpectQubitDecl QubitDecl
             | InvalidGate Gate GateErr
             | NonArrParamDecl
             | NonArrQubitDecl
             deriving (Show, Eq)

-- | Either summarizes a ParamArrDecl, or returns an error.
parseParamDecl :: ParamDecl -> Maybe ParamArr
parseParamDecl (ParamVarDecl _)       = Nothing
parseParamDecl (ParamArrDecl name sz) = Just $ ParamArr name sz

-- | Either summarizes a QubitArrDecl, or returns an error.
parseQubitDecl :: QubitDecl -> Maybe QubitReg
parseQubitDecl (QubitVarDecl _)       = Nothing
parseQubitDecl (QubitArrDecl name sz) = Just $ QubitReg name sz

-- | Takes as input a qubit register description, a parameter array description, and a
-- list of statements. If every statement in the list yields a valid GateSummary with
-- respect to the qubit register and the parameter array, then returns the corresponding
-- list of gate summaries. Otherwise, returns an error explaining why such a list cannot
-- be produced.
asGateList :: QubitReg -> ParamArr -> [Stmt] -> Either StmtErr [GateSummary]
asGateList _    _    []                   = Right []
asGateList qvar pvar (GateStmt gate:rest) =
    case summarizeGate qvar pvar gate of
        Left err   -> Left $ InvalidGate gate err
        Right summ -> updateRight (asGateList qvar pvar rest) (summ :)
asGateList _ _ (ParamDeclStmt decl:_) = Left $ UnexpectParamDecl decl
asGateList _ _ (QubitDeclStmt decl:_) = Left $ UnexpectQubitDecl decl

-- | Takes as input the arguments to a ParamCirc constructor. If the QubitReg and
-- ParamArr have distinct names, then returns the corresponding ParmaCirc. Otherwise,
-- returns an error indicating the ducplicated name error.
finalizeCirc :: QubitReg -> ParamArr -> [GateSummary] -> Either StmtErr ParamCirc
finalizeCirc qvar@(QubitReg qname _) pvar@(ParamArr pname _) gates =
    if qname == pname
    then Left $ DuplicateName qname
    else Right $ ParamCirc pvar qvar gates

-- | Takes as input a list of statements. For this list to be valid, it must consist of a
-- ParamDeclStmt, following by a QubitDeclStmt, followed by zero or more gate statements.
-- If the declarations are both array declarations, and if the gate statements all have
-- valid gate summaries with respect to these declarations, then a parameterized circuit
-- is returned which summarizes the gates and their parameters. Otherwise, an error is
-- returned to explain which fo these assumptions has been violated.
summarizeStmts :: [Stmt] -> Either StmtErr ParamCirc
summarizeStmts (ParamDeclStmt pdecl:QubitDeclStmt qdecl:stmts) =
    case parseParamDecl pdecl of
        Nothing   -> Left NonArrParamDecl
        Just pvar -> case parseQubitDecl qdecl of
            Nothing   -> Left NonArrQubitDecl
            Just qvar -> branchRight (asGateList qvar pvar stmts) (finalizeCirc qvar pvar)
summarizeStmts []                      = Left MissingParams
summarizeStmts (ParamDeclStmt _:stmts) = Left MissingQubits
summarizeStmts _                       = Left MissingParams

-----------------------------------------------------------------------------------------
-- * QASM File Parsing.

-- | The list of required include statements.
_REQ_INCLUDES :: Set.Set String
_REQ_INCLUDES = Set.fromList ["stdgates.inc"]

-- | Explanation for file parsing failures.
data CircErr = UnsupportedVersion String
             | MissingIncludes (NonEmpty.NonEmpty String)
             | UnsupportedIncludes (NonEmpty.NonEmpty String)
             | InvalidStmt StmtErr
             deriving (Show, Eq)

-- | Predicate to identify OpenQASM versions supported by pecac.
isVerSupported :: String -> Bool
isVerSupported "3"   = True
isVerSupported "3.0" = True
isVerSupported _     = False

-- | Helper method to commpute the non-empty difference between two sets.
getInclDiff :: Set.Set String -> Set.Set String -> Maybe (NonEmpty.NonEmpty String)
getInclDiff expt act = NonEmpty.nonEmpty $ Set.toList $ Set.difference expt act

-- | Takes as input a list of included files. If this list does not contain the entire
-- list of include statements required by pecac, then a non-empty list of the missing
-- files is returned. Otherwise, nothing is returned.
hasMissingIncludes :: [String] -> Maybe (NonEmpty.NonEmpty String)
hasMissingIncludes incl = getInclDiff _REQ_INCLUDES $ Set.fromList incl

-- | Takes as input a list of included files. If this list does contains items not on the
-- list of include statements required by pecac, then a non-empty list of the unexpected
-- files is returned. Otherwise, nothing is returned.
hasExtraIncludes :: [String] -> Maybe (NonEmpty.NonEmpty String)
hasExtraIncludes incl = getInclDiff (Set.fromList incl) _REQ_INCLUDES

-- | Takes as input the AST of a QASM file. If the file version and include statements
-- meet the requirements of pecac, then a ParamCirc is returned corresponding to the
-- statements in the file, or an error is returned explaining why this is not possible
-- (see summarizeStmts). Otherwise, an error is returned explaining why the file metadata
-- does not conform to the requirements of pecac. 
qasmToParamCirc :: QASMFile -> Either CircErr ParamCirc
qasmToParamCirc (QASMFile ver incls [] stmts) =
    if not $ isVerSupported ver
    then Left $ UnsupportedVersion ver
    else case hasMissingIncludes incls of
        Just diff -> Left $ MissingIncludes diff
        Nothing   -> case hasExtraIncludes incls of
            Just diff -> Left $ UnsupportedIncludes diff
            Nothing   -> updateLeft (summarizeStmts stmts) InvalidStmt
qasmToParamCirc _ = error "Procedure delcarations are not supported."

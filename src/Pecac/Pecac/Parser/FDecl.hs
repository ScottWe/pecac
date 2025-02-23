-- | Functions to parse function delcarations.

module Pecac.Parser.FDecl
  ( RegisterMap
  , allocate
  , createRegistry
  , getAsExpr
  , getAsOperand
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.List (foldl')
import Pecac.Maybe (maybeApply)
import Pecac.Either (updateRight)
import Pecac.Parser.Syntax
  ( Expr (CellId)
  , Operand (..)
  )

import qualified Data.Map.Strict as Map

-----------------------------------------------------------------------------------------
-- * Register Mappings.

-- | Maintains a mapping from local variables to the array of in-scope declarations.
data RegisterMap = RegisterMap String (Map.Map String Int) deriving (Show, Eq)

-- | Creates an empty registery map associated with the provided register variable name.
createRegistry :: String -> RegisterMap
createRegistry name = RegisterMap name Map.empty

-- | Helper function to populate the RegisterMap map with new declarations.
allocateImpl :: [String] -> Map.Map String Int -> Either String (Map.Map String Int)
allocateImpl []         map = Right map
allocateImpl (var:vars) map =
    if Map.member var map
    then Left var
    else allocateImpl vars $ Map.insert var sz map
    where sz = Map.size map

-- | Allocates space in the array of in-scope declarations for a list of local variables.
allocate :: RegisterMap -> [String] -> Either String RegisterMap
allocate (RegisterMap name entries) vars =
    updateRight (allocateImpl vars entries) (RegisterMap name)

-- | Helper function to retrieve a variable index from the map as a syntactic object.
getAs :: (String -> Int -> a) -> RegisterMap -> String -> Maybe a
getAs f (RegisterMap name entries) var =
    maybeApply (Map.lookup var entries) $ f name

-- | Function to resolve a variable in RegisterMap as an Operand. If the variable is not
-- allocated in the map, then nothing is returned.
getAsOperand :: RegisterMap -> String -> Maybe Operand
getAsOperand = getAs QReg

-- | Function to resolve a variable in RegisterMap as an expression. If the variable is
-- not allocated in the map, then nothing is returned.
getAsExpr :: RegisterMap -> String -> Maybe Expr
getAsExpr = getAs CellId

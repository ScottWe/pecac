-- | A wrapper to Data.Matrix, with minimal support for tensor algebras.

module Pecac.Verifier.Matrix
  ( Matrix
  , add
  , build
  , compose
  , directSum
  , findScalar
  , iden
  , kroneckerProduct
  , prettyShow
  , scale
  , size
  , swap
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Pecac.Maybe (branchJust)

import qualified Data.Matrix as DMatrix

-----------------------------------------------------------------------------------------
-- * Type Definitions and Basic Construtors.

-- | A datatype for standard two-dimensional matrices.
type Matrix a = DMatrix.Matrix a

-- | Takes as input a list of list of matrix entries. The inner lists are assumed to be
-- rows in the matrix.
build :: [[a]] -> Matrix a
build = DMatrix.fromLists

-- | Returns an nxn identity matrix, given a number n.
iden :: (Num a) => Int -> Matrix a
iden = DMatrix.identity

-- | Returns the size of a matrix in the form (rows, cols).
size :: Matrix a -> (Int, Int)
size mat = (DMatrix.nrows mat, DMatrix.ncols mat)

-- | Returns the 2-qubit swap matrix over an arbitrary ring a.
swap :: (Num a) => Matrix a
swap = DMatrix.permMatrix 4 2 3

-----------------------------------------------------------------------------------------
-- * Utilities.

-- | Computes the valid indices for a matrix.
toIndices :: Matrix a -> [(Int, Int)]
toIndices mat = [ (j, k) | j <- [1..n], k <- [1..m] ]
    where (n, m) = size mat

-----------------------------------------------------------------------------------------
-- * Matrix Operations.

-- | Returns the direct sum of two matrices. That is a block diagonal matrix whose first
-- block is mat1 and whose second block is mat2. Note that these blocks need not be
-- square for the direct sum to exist.
directSum :: (Num a) => Matrix a -> Matrix a -> Matrix a
directSum mat1 mat2 = DMatrix.joinBlocks (mat1, tr, bl, mat2)
    where (n, m) = size mat1
          (p, q) = size mat2
          tr     = DMatrix.zero n q
          bl     = DMatrix.zero p m

-- | Helper method to compute th entries in the Kronecker tensor product of two matrices.
-- The code uses the observation that j = j1*n + j2 and k = k1*m + k2. Note, however,
-- that the calculations are performed using zero-indexing, whereas (Matrix-a) uses
-- one-indexing. For this reason, (j - 1) and (k - 1) are used in the calculations,
-- whereas (j1 + 1), (j2 + 1), (k1 + 1), and (k2 + 1) are used in the entry lookups.
kronProdBuilder :: (Num a) => Matrix a -> Matrix a -> (Int, Int) -> a
kronProdBuilder mat1 mat2 (j, k) = entry1 * entry2
    where (n, m)   = size mat2
          (j1, j2) = divMod (j - 1) n
          (k1, k2) = divMod (k - 1) m
          entry1   = DMatrix.getElem (j1 + 1) (k1 + 1) mat1
          entry2   = DMatrix.getElem (j2 + 1) (k2 + 1) mat2

-- | Returns the Kronecker tensor product of two matrices.
kroneckerProduct :: (Num a) => Matrix a -> Matrix a -> Matrix a
kroneckerProduct mat1 mat2 = DMatrix.matrix (n * p) (m * q) $ kronProdBuilder mat1 mat2
    where (n, m) = size mat1
          (p, q) = size mat2

-- | Standard scale multiplication (viewing (Matrix a) as an a-vector space).
scale :: (Num a) => a -> Matrix a -> Matrix a
scale = DMatrix.scaleMatrix

-- | Standard matrix composition (viewing (Matrix a) as a ring).
compose :: (Num a) => Matrix a -> Matrix a -> Matrix a
compose mat1 mat2 = mat1 * mat2

-- | Standard matrix addition (viewing (Matrix a) as a ring).
add :: (Num a) => Matrix a -> Matrix a -> Matrix a
add mat1 mat2 = mat1 + mat2

-----------------------------------------------------------------------------------------
-- * Comparison.

-- | Implementation details for findScalar. This function takes as input the matrices,
-- together with a list of all indices into the matrices. If all of the entries are zero,
-- then nothing is returned. Likewise, if one of the entries is zero, and the other index
-- is nonzero, then nothing is returned. Otherwise, there exists an index (j, k) such
-- that mat1[j,k] and mat2[j,k] are nonzero. In this case, the ratio mat2[j,k]/mat1[j,k]
-- is returned.
findScalarImpl :: (Eq a, Fractional a) => [(Int, Int)] -> Matrix a -> Matrix a -> Maybe a
findScalarImpl []               _    _    = Nothing
findScalarImpl ((j, k):indices) mat1 mat2
    | v1 == 0 && v2 == 0 = findScalarImpl indices mat1 mat2
    | v1 == 0 || v2 == 0 = Nothing
    | otherwise          = Just $ v2 / v1
    where v1 = DMatrix.getElem j k mat1
          v2 = DMatrix.getElem j k mat2

-- | This function takes as input two matrices mat1 and mat2. If there exists a nonzero
-- multiplier s such that (scale s mat1 == mat2), then s is returned. Otherwise, nothing
-- is returned.
findScalar :: (Eq a, Fractional a) => Matrix a -> Matrix a -> Maybe a
findScalar mat1 mat2 =
    if size mat1 == size mat2
    then branchJust (findScalarImpl (toIndices mat1) mat1 mat2) $ \s ->
        if scale s mat1 == mat2
        then Just s
        else Nothing
    else Nothing

-----------------------------------------------------------------------------------------
-- * Matrix Accessors.

-- | Converts a matrix to a human-readable string. The human-readability may fail, as the
-- matrix (or its entries) become arbitrarily large.
prettyShow :: (Show a) => Matrix a -> String
prettyShow = DMatrix.prettyMatrix

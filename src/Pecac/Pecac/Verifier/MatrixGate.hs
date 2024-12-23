-- | A library for constructing matrices which represent qubit quantum gates.

module Pecac.Verifier.MatrixGate
  ( addCtrlToMatrix
  , addNegCtrlToMatrix
  , applyAt
  , applyMatrixBetween
  , swapAndApply
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import qualified Pecac.Verifier.Matrix as Matrix

-----------------------------------------------------------------------------------------
-- * Matrix Padding.

-- | A function which applies a qubit operator (mat) between n qubits on the left-hand
-- side and m qubits on the right-hand side. This function requires that both 0 <= n and
-- m <= 0, else an error is raised.
applyMatrixBetween :: (Num a) => Int -> Int -> Matrix.Matrix a -> Matrix.Matrix a
applyMatrixBetween n m mat
    | n < 0            = error $ emsg "n"
    | m < 0            = error $ emsg "m"
    | n == 0 && m == 0 = mat
    | n > 0 && m == 0  = Matrix.kroneckerProduct lhs mat
    | n == 0 && m > 0  = Matrix.kroneckerProduct mat rhs
    | n < m            = Matrix.kroneckerProduct (Matrix.kroneckerProduct lhs mat) rhs
    | otherwise        = Matrix.kroneckerProduct lhs $ Matrix.kroneckerProduct mat rhs
    where lhs    = Matrix.iden $ 2^n
          rhs    = Matrix.iden $ 2^m
          emsg x = "applyMatrixBetween requires non-negative index: " ++ x ++ "."

-----------------------------------------------------------------------------------------
-- * Permutations.

-- | Helper method to conjugate a matrix w by a matrix u, under the assumption that u is
-- a self-inverse matrix.
uwu :: (Num a) => Matrix.Matrix a -> Matrix.Matrix a -> Matrix.Matrix a
uwu u w = Matrix.compose u $ Matrix.compose w u

-- | Decomposes (swapAt k a b) into a sequence of transpositions on { 0, 1, ..., k - 1 }.
-- This corresponds to the ladders in: https://doi.org/10.1016/S0022-4049(03)00069-0.
swapAtImpl :: (Num a) => Int -> Int -> Int -> Matrix.Matrix a
swapAtImpl k a b =
    if a + 1 == b
    then adj
    else uwu adj $ swapAtImpl k (a + 1) b
    where adj = applyMatrixBetween a (k - a - 2) Matrix.swap

-- | Constructs a (2^k)x(2^k) matrix which swaps qubit a with qubit b. This function
-- requires that both 0 <= a <= k and 0 <= b <= k, else an error is raised.
swapAt :: (Num a) => Int -> Int -> Int -> Matrix.Matrix a
swapAt k a b
    | a == b    = Matrix.iden $ 2^k
    | b < a     = swapAt k b a
    | k < 0     = error $ emsg "k"
    | a < 0     = error $ emsg "a"
    | b < 0     = error $ emsg "b"
    | k < b     = error $ emsg "(k - b)"
    | otherwise = swapAtImpl k a b
    where emsg x = "swapAt requires non-negative index: " ++ x ++ "."

-- | Takes as input the number of qubits (k), a pair of indices to swap (a, b), and the
-- matrix of a qubit operator (mat). If mat is a square matrix of dimension (2^k), with
-- both 0 <= a <= k and 0 <= b <= k, then returns an instance of mat where input qubits a
-- and b have been swapped. Otherwise, an error is raised.
swapAndApply :: (Num a) => Int -> Int -> Int -> Matrix.Matrix a -> Matrix.Matrix a
swapAndApply k a b mat
    | n /= m    = error "swapAndApply requires a square matrix: mat."
    | 2^k /= n  = error "swapAndApply requires that mat have size 2^k."
    | a == b    = mat
    | otherwise = uwu (swapAt k a b) mat
    where (n, m) = Matrix.size mat

-----------------------------------------------------------------------------------------
-- * Gate Application.

-- | Helper method to reindex the operands of a matrix. This assumes that qubit y has
-- been rerouted to input x. Then all instances of index x should be replaced by y (i.e.,
-- a swap has occured). However, there is at most once instance of x due to no-cloning.
-- Then it suffices to replace only the first instance of x with y. The function takes as
-- input the list of qubit indices, the index of x, and the index of y.
opSwap :: [Int] -> Int -> Int -> [Int]
opSwap []     _ _ = []
opSwap (q:qs) x y = if q == x then y : qs else q : opSwap qs x y

-- | Recursive implementation of applyAt. The first argument, j, indicates the expected
-- input qubit. The second argument, qs, is the list of qubit operands. The third
-- argument, k, is the number of qubits. The fourth argument, mat, is the operator being
-- applied to qs.
permuteQs :: (Num a) => Int -> [Int] -> Int -> Matrix.Matrix a -> Matrix.Matrix a
permuteQs j []     k mat = applyMatrixBetween 0 (k - j) mat
permuteQs j (q:qs) k mat =
    if j == q
    then permuteQs nextj qs k mat
    else swapAndApply k j q $ permuteQs nextj (opSwap qs j q) k mat
    where nextj = j + 1

-- | Takes as input the number of qubits (k) and the operands (qubits) to a qubit
-- operator, as represented by a matrix (mat). Returns a (2^k)x(2^k) matrix, which
-- corresponds to applying mat to subspace defined by qubits. In particular, the subspace
-- will represent |qubit| qubits, where the j-th qubit in the subspace corresponds to the
-- (qubit[j])-th qubit in the total space. This function requires that qubits contains
-- only unique elements from { 0, 1, ..., k-1 }.
applyAt :: (Num a) => Int -> [Int] -> Matrix.Matrix a -> Matrix.Matrix a
applyAt k [] mat
    | n /= m    = error "applyAt requires a square matrix: mat."
    | n /= 1    = error "applyAt requires that dimension of mat matches qubit count."
    | otherwise = Matrix.kroneckerProduct mat $ Matrix.iden $ 2^k
    where (n, m) = Matrix.size mat
applyAt k qubits mat
    | n /= m        = error "applyAt requires a square matrix: mat."
    | n /= 2^qcount = error "applyAt requires that dimension of mat matches qubit count."
    | qcount > k    = error "applyAt requires qubits do not exceed system size (k)."
    | otherwise     = applyMatrixBetween minq (k - maxq) permuted
    where (n, m)   = Matrix.size mat
          qcount   = length qubits
          minq     = foldr min k qubits
          maxq     = 1 + foldr max 0 qubits
          local    = map (\x -> x - minq) qubits
          permuted = permuteQs 0 local (maxq - minq) mat

-----------------------------------------------------------------------------------------
-- * Control Modifiers.

-- | Returns the matrix which is obtained by adding a positive qubit control to the
-- specified matrix.
addCtrlToMatrix :: (Num a) => Matrix.Matrix a -> Matrix.Matrix a
addCtrlToMatrix mat = 
    if n == m
    then Matrix.directSum (Matrix.iden n) mat
    else error "addCtrlToMatrix may only be applied to square matrices."
    where (n, m) = Matrix.size mat

-- | Returns the matrix which is obtained by adding a negative qubit control to the
-- specified matrix.
addNegCtrlToMatrix :: (Num a) => Matrix.Matrix a -> Matrix.Matrix a
addNegCtrlToMatrix mat =
    if n == m
    then Matrix.directSum mat $ Matrix.iden n
    else error "addNegCtrlToMatrix may only be applied to square matrices."
    where (n, m) = Matrix.size mat

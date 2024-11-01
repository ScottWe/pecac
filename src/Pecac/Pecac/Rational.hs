-- | General-purpose Rational utilities not found in Prelude.

module Pecac.Rational
  ( qceil
  , qfloor
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.Ratio
  ( denominator
  , numerator
  )

-----------------------------------------------------------------------------------------
-- Rounding Rational Numbers.

-- | Takes as input a rational number a/b. Returns a tuple (x, y) containing the quotient
-- and remainder between a and b. That is, a/b = x + y/b with 0 <= y < b.
expand :: Rational -> (Integer, Integer)
expand x = (quot n d, rem n d)
    where n = numerator x
          d = denominator x

-- | Returns the floor of a rational number.
qfloor :: Rational -> Integer
qfloor = fst . expand

-- | Returns the ceiling of a natural number.
qceil :: Rational -> Integer
qceil x = if r == 0 then q else q + 1
    where (q, r) = expand x

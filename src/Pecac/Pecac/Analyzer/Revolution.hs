-- | Types and functions for working with rational revolutions (multiples of 2*pi).

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Pecac.Analyzer.Revolution
  ( Group (..)
  , Monoid (..)
  , Revolution
  , RMod (..)
  , Semigroup (..)
  , asRational
  , rationalToRev
  , ratioToRev
  , strToRev
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.Group
import Data.Ratio
  ( (%)
  , denominator
  , numerator
  )
import Pecac.Affine (RMod (..))
import Pecac.Maybe (branchJust)
import Pecac.Parser.Revolution (parseRevolution)

-----------------------------------------------------------------------------------------
-- * Revolution Constructors.

-- | This type represents an angle in rational revolutions (i.e. multiples of 2*pi). This
-- helps to ensure type-safety (e.g., conversion between rational values and angles must
-- be explicit now) and sound usage of data-types (e.g., rational numbers form a ring
-- whereas angles form a Q-module).
newtype Revolution = Revolution Rational deriving (Eq)

-- | Converts a rational number to a revolution.
rationalToRev :: Rational -> Revolution
rationalToRev = Revolution

-- | Converts a pair of numbers (a numerator and a denominator) to a revolution. If the
-- denominator is zero, then the function returns nothing, rather than raising an error.
ratioToRev :: Integer -> Integer -> Maybe Revolution
ratioToRev n d = if d == 0 then Nothing else Just $ rationalToRev $ n % d

-- | Converts a string to a revolution, according to the format in Parser.Revolution.
strToRev :: String -> Maybe Revolution
strToRev src = branchJust (parseRevolution src) (uncurry ratioToRev)

-----------------------------------------------------------------------------------------
-- * Displaying Revolution.

-- | Converts a revolution to a rational number. This is the inverse to rationalToRev.
asRational :: Revolution -> Rational
asRational (Revolution r) = r

-- | The default format is as rational multiples of pi.
instance Show Revolution where
    show rev = nstr ++ " / " ++ dstr ++ " * pi"
        where r    = 2 * asRational rev
              nstr = show $ numerator r
              dstr = show $ denominator r

-----------------------------------------------------------------------------------------
-- * Q-Module Structure.

-- | The group structure is by additional of angles.
instance Semigroup Revolution where
    (<>) rev1 rev2 = rationalToRev $ r1 + r2
        where r1 = asRational rev1
              r2 = asRational rev2

-- | The identity revolution is a rotation by 0-degrees.
instance Monoid Revolution where
    mempty = rationalToRev 0

-- | The inverse of a rotation by r revolutions is a rotation by -r revolutions.
instance Group Revolution where
    invert rev = rationalToRev $ negate $ asRational rev

-- | Rotation addition is clearly commutative.
instance Abelian Revolution

-- | Rotations can be scaled by rational numbers.
instance RMod Rational Revolution where
    scale s rev = rationalToRev $ s * asRational rev

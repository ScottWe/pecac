-- | General-purpose Either utilities not found in Prelude.

module Pecac.Either
  ( updateRight
  , branchRight
  ) where

-----------------------------------------------------------------------------------------
-- Either for Value Propogation.

-- | Consumes a function f from type b to type (Either a c) and a value of type
-- (Either a b). If the value is of type a, then it is unchanged and returned on the
-- left. If the value is equal to (Right r), then (f r) is returned. In other words, the
-- function branches on the right, to either a left or right value.
branchRight :: Either a b -> (b -> Either a c) -> Either a c
branchRight (Left l)  _ = Left l
branchRight (Right r) f = f r

-- | Consumes a function from type b to type c, and a value of type (Either a b). If the
-- value is of type a, then it is unchanged and returned on the left. If the value is of
-- type b, then f is applied and the resulting value is returned on the right.
updateRight :: Either a b -> (b -> c) -> Either a c
updateRight value f = branchRight value (Right . f)

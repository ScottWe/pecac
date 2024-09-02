-- | General-purpose Maybe utilities not found in Prelude.

module Pecac.Maybe
  ( branchJust
  , maybeApply
  ) where

-----------------------------------------------------------------------------------------
-- Maybe Value Manipulation.

-- | Consumes a (Maybe a) value and a function f from type a to type (Maybe b). If the
-- Maybe argument wraps a value x of type a, then (f x) is returned. Otherwise, nothing
-- is returned.
branchJust :: Maybe a -> (a -> Maybe b) -> Maybe b
branchJust Nothing  _ = Nothing
branchJust (Just x) f = f x

-- | Consumes a value of type (Maybe a) and function f from type a to type b. If the
-- Maybe argument wraps a value x of type a, then a (Maybe b) value wrapping (f x) is
-- returned. Otherwise, nothing is returned.
maybeApply :: Maybe a -> (a -> b) -> Maybe b
maybeApply maybe f = branchJust maybe (Just . f)

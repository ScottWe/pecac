-- | Command flags between command-line parsers.

{-# LANGUAGE DeriveDataTypeable #-}

module PecacExe.CmdLnFlags
  ( def
  , gphaseFlag
  , probFlags
  , radFlags
  , seedFlags
  , srcFlags
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import System.Console.CmdArgs
  ( Data
  , Typeable
  , (&=)
  , argPos
  , def
  , typ
  , help
  , typFile
  )

-------------------------------------------------------------------------------
-- * Input/Output Flags.

-- | Returns the flags for the --src argument. The default value is taken as an
-- argument, since flags are impure.
srcFlags :: String -> String
srcFlags x = x &= help "Input source (defaults to stdin)."
               &= typFile

-------------------------------------------------------------------------------
-- * Parameter Flags.

-- Returns the flags for the --prob argument. This default value is taken as an
-- argument, since flags are impure.
probFlags :: Rational -> Rational
probFlags x = x &= help "A bound on the probability of failure, as a rational."

-- | Returns the flags for the --angle argument. Each use of this flag adds an
-- addition angle (given as a rational multiple of pi) to the list of all
-- anlges. The defualt value (typically an empty list) is taken as an argument,
-- since flags are impure.
radFlags :: [String] -> [String]
radFlags x = x &= help "A rational multiple of 2*pi."

-- | Returns the flags for the --seed argument. The default value is no seed,
-- in which case the system should be used to generate a seed. This default
-- value is taken as an argument, since flags are impure.
seedFlags :: Maybe Int -> Maybe Int
seedFlags x = x &= help "A manual random seed for deterministic tests."

-- | Returns the flags for the --gphase argument. The default value is taken as
-- an argument, since flags are impure.
gphaseFlag :: Bool -> Bool
gphaseFlag x = x &= help "Compare circuits upto global phase."

-- | Command flags between command-line parsers.

{-# LANGUAGE DeriveDataTypeable #-}

module PecacExe.CmdLnFlags
  ( def
  , radFlags
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

-- | Returns the flags for the --angle argument. Each use of this flag adds an
-- addition angle (given as a rational multiple of pi) to the list of all
-- anlges. The defualt value (typically an empty list) is taken as an argument,
-- since flags are impure.
radFlags :: [String] -> [String]
radFlags x = x &= help "A rational multiple of pi."

-- | Command flags between command-line parsers.

{-# LANGUAGE DeriveDataTypeable #-}

module PecacExe.CmdLnFlags
  ( def
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

-- | Command-line handler for the pecac parser.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Parser.CmdLn
  ( ParseTool(..)
  , getToolArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import PecacExe.CmdLnFlags
  ( def
  , srcFlags
  )
import PecacExe.CmdLnParser
  ( Data
  , Typeable
  , addModeAnnotations
  , parseCmdLnArgs
  )

-------------------------------------------------------------------------------
-- * Argument Data Type.

data ParseTool = ParseMode { src :: String } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

parseMode :: ParseTool
parseMode = ParseMode { src = srcFlags def }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO ParseTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "PECAC Parser Interface"
          desc  = "A command-line interface to inspect to PECAC AST."
          ctors = [parseMode]

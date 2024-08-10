-- | Command-line handler for the pecac circuit viewer.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module CircViewer.CmdLn
  ( ViewTool(..)
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

data ViewTool = CircMode { src :: String } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

circMode :: ViewTool
circMode = CircMode { src = srcFlags def }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO ViewTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "PECAC Circuit Viewer"
          desc  = "A command-line tool to view pecac circuit representations."
          ctors = [circMode]

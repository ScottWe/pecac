-- | Command-line handler for the pecac cutoff viewer.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module CutoffViewer.CmdLn
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

data ViewTool = Inspect { src :: String } 
              | Compare { lhs :: String
                        , rhs :: String }
              deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

inspectMode :: ViewTool
inspectMode = Inspect { src = srcFlags def }

compareMode :: ViewTool
compareMode = Compare { lhs = srcFlags def
                      , rhs = srcFlags def }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO ViewTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "PECAC Curoff Viewer"
          desc  = "A command-line tool to view a circuit's cutoff values."
          ctors = [inspectMode, compareMode]

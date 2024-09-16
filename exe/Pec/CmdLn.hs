-- | Command-line handler for the pecac pec tool.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Pec.CmdLn
  ( PecTool(..)
  , getToolArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import PecacExe.CmdLnFlags
  ( def
  , gphaseFlag
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

data PecTool = PecTool { lhs    :: String
                       , rhs    :: String
                       , gphase :: Bool
                       } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

pecMode :: PecTool
pecMode = PecTool { lhs    = srcFlags def
                  , rhs    = srcFlags def
                  , gphase = gphaseFlag def
                  }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO PecTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "PECAC Equivalence Checker"
          desc  = "A command-line tool for parameterized equivalence checking."
          ctors = [pecMode]

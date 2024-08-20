-- | Command-line handler for the pecac cyclotomic instantiator.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module CycloInst.CmdLn
  ( CycloInstTool(..)
  , getToolArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import PecacExe.CmdLnFlags
  ( def
  , radFlags
  , srcFlags
  )
import PecacExe.CmdLnParser
  ( Data
  , Typeable
  , addModeAnnotations
  , parseCmdLnArgs
  )

-----------------------------------------------------------------------------------------
-- * Argument Data Type.

data CycloInstTool = Evaluate { src    :: String
                              , angles :: [String] }
                   | Compare { lhs    :: String
                             , rhs    :: String
                             , angles :: [String] }
                   deriving (Show, Eq, Data, Typeable)

-----------------------------------------------------------------------------------------
-- * Program Modes.

evalMode :: CycloInstTool
evalMode = Evaluate { src    = srcFlags def
                    , angles = radFlags def
                    }

compMode :: CycloInstTool
compMode = Compare { lhs    = srcFlags def
                   , rhs    = srcFlags def
                   , angles = radFlags def
                   }

-----------------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO CycloInstTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "PECAC Cyclotomic Circuit Instantiator"
          desc  = "A command-line tool to instantiate circuits with rational angles."
          ctors = [evalMode, compMode]

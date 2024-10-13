-- | Command-line handler for the pecac reparameterization tool.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Reparam.CmdLn
  ( ReparamTool (..)
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

data ReparamTool = FindLCD { src :: String }
                 | JoinLCD { src :: String
                           , otr :: String }
                 | Rescale { src :: String
                           , otr :: String
                           } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

findLcdMode :: ReparamTool
findLcdMode = FindLCD { src = srcFlags def }

joinLcdMode :: ReparamTool
joinLcdMode = JoinLCD { src = srcFlags def
                      , otr = srcFlags def
                      }

rescaleMode :: ReparamTool
rescaleMode = Rescale { src = srcFlags def
                      , otr = srcFlags def
                      }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO ReparamTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "PECAC Reparameterization Tool"
          desc  = "A command-line tool to find equivalent integral circuits."
          ctors = [findLcdMode, joinLcdMode, rescaleMode]

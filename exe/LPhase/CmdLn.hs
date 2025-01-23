-- | Command-line handler for the pecac global linear phase inference tool.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module LPhase.CmdLn
  ( LPhaseTool (..)
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

data LPhaseTool = Infer { src :: String
                        , otr :: String }
                | Encode { src :: String
                         , otr :: String
                         } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

inferMode :: LPhaseTool
inferMode = Infer { src = srcFlags def
                  , otr = srcFlags def
                  }

encodeMode :: LPhaseTool
encodeMode = Encode { src = srcFlags def
                    , otr = srcFlags def
                    }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO LPhaseTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "PECAC Linear Phase Estimation Tool"
          desc  = "A command-line tool to find infer linear global phase."
          ctors = [inferMode, encodeMode]

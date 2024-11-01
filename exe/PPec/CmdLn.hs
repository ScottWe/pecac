-- | Command-line handler for the pecac ppec tool.

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module PPec.CmdLn
  ( PPecTool(..)
  , getToolArgs
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.Ratio ((%))
import PecacExe.CmdLnFlags
  ( def
  , gphaseFlag
  , probFlags
  , seedFlags
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

data PPecTool = PPecTool { prob   :: Rational
                         , lhs    :: String
                         , rhs    :: String
                         , gphase :: Bool
                         , seed   :: Maybe Int
                         } deriving (Show, Eq, Data, Typeable)

-------------------------------------------------------------------------------
-- * Program Modes.

ppecMode :: PPecTool
ppecMode = PPecTool { prob   = probFlags $ 1 % 1
                    , lhs    = srcFlags def
                    , rhs    = srcFlags def
                    , gphase = gphaseFlag def
                    , seed   = seedFlags def
                    }

-------------------------------------------------------------------------------
-- * CmdArgs Mode Declaration.

getToolArgs :: IO PPecTool
getToolArgs = parseCmdLnArgs title desc ctors
    where title = "PECAC Probabilistic Equivalence Checker"
          desc  = "A command-line tool for probabilistic PEC."
          ctors = [ppecMode]

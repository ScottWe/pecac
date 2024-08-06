-- | Command-line interface to interact with the PECEC parser.

module Main where

-------------------------------------------------------------------------------
-- * Import Section.

import Parser.CmdLn
  ( ParseTool(..)
  , getToolArgs
  )
import PecacExe.IOUtils (readSrc)
import Pecac.Parser.Parser (parseQasm)
import Text.Pretty.Simple (pPrint)

-------------------------------------------------------------------------------
-- * Entry Point.

processArgs :: ParseTool -> IO ()
processArgs (ParseMode src) = do
    contents <- readSrc src
    case parseQasm src contents of
        Left err   -> putStrLn err
        Right prog -> pPrint prog

main :: IO ()
main = do
    args <- getToolArgs
    processArgs args

-- | Utility methods to read source files and output files.

module PecacExe.IOUtils
  ( readCirc
  , readSrc
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Pecac.Analyzer.Problem (ParamCirc)
import Pecac.Parser.Parser (parseQasm)
import Pecac.Parser.Problem (qasmToParamCirc)
import PecacExe.ErrorLogging (logCircErr)
import System.Exit (die)
import System.Directory (doesFileExist)

-------------------------------------------------------------------------------
-- * Basic Input and Output.

-- | Takes as input the the name of a file. If the name is empty, then the
-- input defaults to stdin and all contents pending on stdin are turned.
-- Otherwise, the file is read as and the contents are returned.
readSrc :: String -> IO String
readSrc "" = getContents
readSrc fp = do
    exists <- doesFileExist fp
    if exists
    then readFile fp
    else die $ "Unable to read source file: " ++ fp

-------------------------------------------------------------------------------
-- * Circuit Reading.

-- | Takes as input the name of an OpenQASM 3 file, and callback function. If
-- the name of the file is empty, then the input defaults to stdin as described
-- in readSrc. If the read is successful, then an attempt is made to parse the
-- file as a parameterized circuit. If this is successful, then the callback
-- function is called with the parameterized circuit, to perform an IO-action.
-- In all other cases, an error is logged to stdout.
readCirc :: String -> (ParamCirc -> IO ()) -> IO ()
readCirc src f = do
    contents <- readSrc src
    case parseQasm src contents of
        Left err   -> putStrLn err
        Right prog -> case qasmToParamCirc prog of
            Left err   -> putStrLn $ unlines $ logCircErr err
            Right circ -> f circ

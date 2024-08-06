-- | Utility methods to read source files and output files.

module PecacExe.IOUtils (readSrc) where

-------------------------------------------------------------------------------
-- * Import Section.

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

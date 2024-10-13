-- | Utility methods to read source files and output files.

module PecacExe.IOUtils
  ( logList
  , logShowableVect
  , logVect
  , readCirc
  , readRevs
  , readSrc
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Pecac.Analyzer.Problem (ParamCirc)
import Pecac.Analyzer.Revolution
  ( Revolution
  , strToRev
  )
import Pecac.Either (branchRight)
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

-------------------------------------------------------------------------------
-- * Revolution Reading.

-- | Takes as input a list of strings. If the strings can be interpreted as
-- revolutions, then the corresponding list of revolutions is returned.
-- Otherwise, nothing is returned.
readRevsImpl :: [String] -> Either String [Revolution]
readRevsImpl []         = Right []
readRevsImpl (str:strs) =
    case strToRev str of
        Nothing  -> Left str
        Just rev -> branchRight (readRevsImpl strs) $
            \revs -> Right $ rev : revs

-- | Takes as input a list of strings, and callback function. If the strings
-- can be interpreted as revolutions, then the callback function is called with
-- those revolutions, to perform an IO-action. Otherwise, an error is logged to
-- stdout.
readRevs :: [String] -> ([Revolution] -> IO ()) -> IO ()
readRevs strs f =
    case readRevsImpl strs of
        Left err   -> putStrLn $ "Failed to parse angle: " ++ err
        Right revs -> f revs

-------------------------------------------------------------------------------
-- * List Formatting.

-- | Recursive implementation details for logList. Takes, as a first argument,
-- the current index into the list.
logListImpl :: Int -> (Int -> a -> String) -> [a] -> IO ()
logListImpl _ _       []     = return ()
logListImpl n display (x:xs) = do
    putStrLn $ display n x
    logListImpl (n + 1) display xs

-- | Helper method to print the contents of a list. A formatting function is
-- taken as the first input. A list is taken as the second input, on which the
-- formatting function is used to print each element to stdout.
logList :: (Int -> a -> String) -> [a] -> IO ()
logList = logListImpl 0

-- | A specialization of logList, which formats each element of the list as an
-- index into an array, that is:
--      <var>[<index>]: <value>
-- The variable name is taken as the first input, and a function which converts
-- each element to a string is taken as the second input.
logVect :: String -> (a -> String) -> [a] -> IO ()
logVect var format = logList display
    where display n v = var ++ "[" ++ show n ++ "]: " ++ format v

-- | A specialization of logVect, in which each element of the list is showable
-- and the corresponding show function is used to stringify each element.
logShowableVect :: (Show a) => String -> [a] -> IO ()
logShowableVect var = logVect var show

#!/usr/bin/env runhaskell
{-# Options -Wall #-}

import qualified Data.ByteString as BS
import           Data.PSDF
import           System.Environment

{-
  given a filename as a command line argument, read from the file
  if no filename is given, read from standard input
-}

main :: IO ()
main = do
  argv <- getArgs
  if length argv == 0
    then do
      str <- BS.getContents
      print $ readPSDF str
    else do
      psdf <- readPSDFFile $ head argv
      print psdf

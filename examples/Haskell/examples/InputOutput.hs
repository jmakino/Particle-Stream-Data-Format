#!/usr/bin/env runhaskell
{-# Options -Wall #-}

import qualified Data.ByteString as BS
import           Data.PSDF
import           System.Environment

{-
  given two filenames as command line arguments, 
  read from the first file and write to second
  if no filename is given, read from standard input
-}

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    (inFn: outFn: _) -> do
      psdf <- decodePSDFFile inFn
      encodePSDFFile outFn psdf      
    _ -> do
      str <- BS.getContents
      let psdf = decodePSDF str
      BS.putStr $ encodePSDF psdf


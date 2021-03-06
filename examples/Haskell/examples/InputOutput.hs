#!/usr/bin/env runhaskell
{-# Options -Wall #-}

import qualified Data.ByteString as BS
import           Data.PSDF.Tensor
import           Data.Tensor.TypeLevel
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
      encodePSDFFile outFn (psdf :: PSDF Vec3)     
    _ -> do
      str <- BS.getContents
      let psdf = decodePSDF str
      BS.putStr $ encodePSDF (psdf :: PSDF Vec3)


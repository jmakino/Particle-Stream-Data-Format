#!/usr/bin/env runhaskell
{-# Options -Wall #-}

import qualified Data.ByteString as BS
import           Data.PSDF

main :: IO ()
main = do
  str <- BS.getContents
  print $ readPSDF str
  return ()
{-# LANGUAGE OverloadedStrings #-}

module Data.PSDF (
  PSDF(..),
  
  readPSDF
  ) where

import           Control.Monad
import qualified Data.ByteString      as BS
import qualified Data.ListLike ()
import qualified Data.ListLike.String as LL
import           Data.List 
import           Data.Maybe
import qualified Data.Object          as Y
import qualified Data.Object.Yaml     as Y
import qualified Data.Text            as T


data PSDF = PSDF
type MyYaml = Y.Object T.Text T.Text


readPSDF :: BS.ByteString -> [MyYaml]
readPSDF str = catMaybes $ map Y.decode strs
  where
    isHeader = BS.isPrefixOf "---" 
    cmp x y = (isHeader x == isHeader y)
    strs = 
      map LL.unlines $ 
      filter (not . isHeader . head) $ 
      groupBy cmp $ 
      LL.lines str
    


parse :: MyYaml -> PSDF
parse _ = PSDF

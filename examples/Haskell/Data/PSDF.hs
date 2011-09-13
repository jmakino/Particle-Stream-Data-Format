{-# LANGUAGE OverloadedStrings #-}
{-# Options -Wall #-}

module Data.PSDF (
  PSDF, Body(..),
  
  readPSDF, readPSDFFile
  ) where

import           Control.Applicative
--import           Control.Monad
import qualified Data.ByteString      as BS
import           Data.ListLike ()
import           Data.ListLike.Text ()
import qualified Data.ListLike.String as LL
import           Data.List 
import           Data.Maybe
import qualified Data.Object          as Y
import qualified Data.Object.Yaml     as Y
import qualified Data.Text            as T

type MyScalar = T.Text
type MyMapping = [(MyScalar, Y.Object MyScalar MyScalar)]
type MyYaml = Y.Object MyScalar MyScalar
type PSDF = [Body]

data Body 
  = Body
    { id       :: Int,
      time     :: Double,
      mass     :: Double,
      position :: [Double],
      velocity :: [Double]
    }
    deriving (Eq, Show)

readPSDFFile :: FilePath -> IO PSDF
readPSDFFile = fmap readPSDF . BS.readFile 

readPSDF :: BS.ByteString -> PSDF
readPSDF str = 
  catMaybes $ 
  map (>>= parseBody) $
  map (Y.decode) strs
  where
    isHeader = BS.isPrefixOf "---" 
    cmp x y = (isHeader x == isHeader y)
    strs = 
      map LL.unlines $ 
      filter (not . isHeader . head) $ 
      groupBy cmp $ 
      LL.lines str

parseBody :: MyYaml -> Maybe Body
parseBody yaml = case yaml of
  Y.Mapping xs -> parseMapping xs
  _            -> Nothing

parseMapping :: MyMapping -> Maybe Body
parseMapping mapping = Body <$> parse "id" <*> parse "t" <*> parse "m" <*> parseList "r" <*> parseList "v"
  where
    parse :: Read a => MyScalar -> Maybe a
    parse key = 
      fmap (read . LL.toString) $
      (>>= expectScalar) $
      lookup key mapping
      
    parseList :: Read a => MyScalar -> Maybe [a]
    parseList key = 
      fmap (map $ read . LL.toString) $
      (>>= expectList) $
      lookup key mapping
    
    expectScalar :: MyYaml -> Maybe MyScalar
    expectScalar x = case x of
      Y.Scalar ret -> Just ret
      _            -> Nothing
      
    expectList :: MyYaml -> Maybe [MyScalar]
    expectList x = case x of
      Y.Sequence xs -> sequence $ map expectScalar xs
      _             -> Nothing

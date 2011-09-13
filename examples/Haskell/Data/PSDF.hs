{-# LANGUAGE OverloadedStrings #-}
{-# Options -Wall #-}

module Data.PSDF (
  PSDF, Body(..),
  
  decodePSDF, decodePSDFFile,
  encodePSDF, encodePSDFFile
  ) where

import           Control.Applicative
import qualified Data.ByteString      as BS
import qualified Data.ListLike        as LL
import           Data.ListLike.Text ()
import qualified Data.ListLike.String as LL
import           Data.List 
import           Data.Maybe
import qualified Data.Object          as Y
import qualified Data.Object.Yaml     as Y
import qualified Data.Text            as T
import           Prelude              hiding (id)


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

decodePSDFFile :: FilePath -> IO PSDF
decodePSDFFile = fmap decodePSDF . BS.readFile 

decodePSDF :: BS.ByteString -> PSDF
decodePSDF str = 
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

encodePSDFFile :: FilePath -> PSDF -> IO ()
encodePSDFFile fn psdf = BS.writeFile fn $ encodePSDF psdf

encodePSDF :: PSDF -> BS.ByteString
encodePSDF = LL.unlines . map encodeBody 

encodeBody :: Body -> BS.ByteString
encodeBody body = 
  BS.filter (/= 39) $
  LL.append "--- !Particle\n" $
  (Y.encode :: MyYaml -> BS.ByteString) $
  Y.Mapping 
    [ ("id", encodeScalar $ id body),
      ("t" , encodeScalar $ time body),
      ("m" , encodeScalar $ mass body),
      ("r" , encodeScalar $ position body),
      ("v" , encodeScalar $ velocity body)
    ]
  where
    encodeScalar :: Show a => a -> MyYaml
    encodeScalar = Y.Scalar . LL.fromString . show


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

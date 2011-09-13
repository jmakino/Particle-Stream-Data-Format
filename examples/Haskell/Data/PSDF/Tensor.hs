{-# LANGUAGE FlexibleContexts, OverloadedStrings, StandaloneDeriving, UndecidableInstances #-}
{-# Options -Wall #-}

-- | An implementation of PSDF reader and writer based on typelevel-tensor

module Data.PSDF.Tensor (
  PSDF(..), Body(..),

  decodePSDF, decodePSDFFile,
  encodePSDF, encodePSDFFile
  ) where

import           Control.Applicative
import qualified Data.ByteString      as BS
import           Data.Foldable        
import qualified Data.ListLike        as LL
import           Data.ListLike.Text ()
import qualified Data.ListLike.String as LL
import           Data.List 
import           Data.Maybe
import qualified Data.Object          as Y
import qualified Data.Object.Yaml     as Y
import           Data.Tensor.TypeLevel
import qualified Data.Text            as T
import           Prelude              hiding (id)


type MyScalar = T.Text
type MyMapping = [(MyScalar, Y.Object MyScalar MyScalar)]
type MyYaml = Y.Object MyScalar MyScalar
type PSDF vec = [Body vec]

data Body vec
  = Body
    { id       :: Int,
      time     :: Double,
      mass     :: Double,
      position :: vec Double,
      velocity :: vec Double
    }

deriving instance (Eq (vec Double)) => Eq (Body vec)
deriving instance (Show (vec Double)) => Show (Body vec)



decodePSDFFile :: (Vector vec) => FilePath -> IO (PSDF vec)
decodePSDFFile = fmap decodePSDF . BS.readFile 

decodePSDF :: (Vector vec) => BS.ByteString -> (PSDF vec)
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

encodePSDFFile :: (Vector vec) => FilePath -> PSDF vec -> IO ()
encodePSDFFile fn psdf = BS.writeFile fn $ encodePSDF psdf

encodePSDF :: (Vector vec) => PSDF vec -> BS.ByteString
encodePSDF = LL.concat . map encodeBody 

encodeBody :: (Vector vec) => Body vec -> BS.ByteString
encodeBody body = 
  BS.filter (/= 39) $
  LL.append "--- !Particle\n" $
  (Y.encode :: MyYaml -> BS.ByteString) $
  Y.Mapping 
    [ ("id", encodeScalar $ id body),
      ("t" , encodeScalar $ time body),
      ("m" , encodeScalar $ mass body),
      ("r" , encodeScalar $ toList $ position body),
      ("v" , encodeScalar $ toList $ velocity body)
    ]
  where
    encodeScalar :: Show a => a -> MyYaml
    encodeScalar = Y.Scalar . LL.fromString . show


parseBody :: (Vector vec) => MyYaml -> Maybe (Body vec)
parseBody yaml = case yaml of
  Y.Mapping xs -> parseMapping xs
  _            -> Nothing

parseMapping :: (Vector vec) => MyMapping -> Maybe (Body vec)
parseMapping mapping = Body <$> parse "id" <*> parse "t" <*> parse "m" <*> parseVec "r" <*> parseVec "v"
  where
    parse :: Read a => MyScalar -> Maybe a
    parse key = 
      fmap (read . LL.toString) $
      (>>= expectScalar) $
      lookup key mapping

    parseVec key = 
      fmap (\xs -> compose (\i -> xs!!axisIndex i)) $
      parseList key     
    
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

{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, OverloadedStrings, StandaloneDeriving, UndecidableInstances #-}
{-# Options -Wall #-}

-- | An implementation of PSDF reader and writer based on typelevel-tensor

module Data.PSDF.Tensor (
  PSDF, Body(..),

  decodePSDF, decodePSDFFile,
  encodePSDF, encodePSDFFile
  ) where

import           Control.Applicative
import qualified Data.ByteString       as BS
import           Data.Foldable        
import           Data.List 
import           Data.Maybe
import qualified Data.PSDF.List        as ListPSDF
import           Data.Tensor.TypeLevel
import           NumericPrelude        hiding (id)

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


decodeBody :: (Vector vec) => ListPSDF.Body -> Maybe (Body vec)
decodeBody x = ret
  where
    ret =   
      Body (ListPSDF.id x) (ListPSDF.time x) (ListPSDF.mass x) <$> 
      decodeVec (ListPSDF.position x) <*> decodeVec (ListPSDF.velocity x) 
    
    z = compose $ const 0
    dim = dimension z
    
    decodeVec xs 
      | dim /= length xs = Nothing
      | otherwise        = Just $ compose $ \i -> xs !! axisIndex i
      | otherwise        = return z
      -- this last guard never matches, but needed to infer the type of z.
                           
encodeBody :: (Vector vec) => Body vec -> ListPSDF.Body 
encodeBody x = 
  ListPSDF.Body (id x) (time x) (mass x) (toList $ position x) (toList $ velocity x)

decodePSDFFile :: (Vector vec) => FilePath -> IO (PSDF vec)
decodePSDFFile = fmap (catMaybes . map decodeBody) . ListPSDF.decodePSDFFile 

decodePSDF :: (Vector vec) => BS.ByteString -> (PSDF vec)
decodePSDF = catMaybes . map decodeBody . ListPSDF.decodePSDF

encodePSDFFile :: (Vector vec) => FilePath -> PSDF vec -> IO ()
encodePSDFFile fn psdf = ListPSDF.encodePSDFFile fn $ map encodeBody psdf

encodePSDF :: (Vector vec) => PSDF vec -> BS.ByteString
encodePSDF = ListPSDF.encodePSDF . map encodeBody  

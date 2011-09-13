#!/usr/bin/env runhaskell
{-# LANGUAGE NoImplicitPrelude #-}

{-
  read from standard input and
  perform various coordinate transformations.
-}

import qualified Data.ByteString  as BS
import           Data.PSDF.Tensor 
import           Data.Tensor.TypeLevel
import           NumericPrelude hiding (id)
import           System.Environment



main :: IO ()
main = do
  psdf <- fmap decodePSDF BS.getContents 
  process psdf

      

process :: PSDF Vec3 -> IO ()      
process particles = do
  putStrLn "total mass:"
  print $ totalMass particles
    
  putStrLn "total energy:"    
  print $ totalEnergy particles
  
  putStrLn "total momentum:"  
  print $ totalMomentum particles
  
  putStrLn "total 2nd moment:"  
  print $ sum $ map moment2 $ particles

  putStrLn "total 3rd moment:"  
  print $ sum $ map moment3 $ particles

  where
    norm v = contract $ \i -> v!i * v!i

    momentum body = compose $ \i-> mass body * velocity body!i
    moment2 body  = compose $ \i-> compose $ \j -> mass body * velocity body!i * velocity body!j
    
    moment3 body  = compose $ \i-> compose $ \j -> compose $ \k -> 
      mass body * velocity body!i * velocity body!j * velocity body!k

    kineticEnergy body = 0.5 * mass body * norm (velocity body)
    potentialEnergy body1 body2 = 
      - (mass body1 * mass body2) / sqrt (norm (position body1 - position body2))

    energy body = 
      kineticEnergy body + 
      0.5 * sum[potentialEnergy i j| i <- particles, j <- particles, id i /= id j]


    totalMass = sum . map mass
    totalMomentum = sum . map momentum

    totalEnergy = sum . map energy 

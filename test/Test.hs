{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Main ( main ) where

import Control.Eff

import System.Random.Effect

import Control.Monad (void)
import Data.Word
import Data.Typeable

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Test.HUnit hiding (State)
import Test.QuickCheck

main :: IO ()
main = defaultMain tests

checkRange :: (Integer, Integer) -> Integer -> Bool
checkRange (low, high) x =
  x >= low && x <= high

testUniformRandom :: Integer -> Integer -> Word64 -> Bool
testUniformRandom a b seed =
  let low  = min a b
      high = max a b

   in checkRange (low, high) $ run $ runRandomState (mkRandom seed) $ do
        uniformIntDist a b

tests =
  [ testProperty "random range" testUniformRandom
  ]

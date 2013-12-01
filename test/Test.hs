{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Main ( main ) where

import Control.Eff
import Control.Eff.State

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

runWithSeed :: Word64 -> Eff (State Random :> ()) a -> a
runWithSeed seed = run . runRandomState (mkRandom seed)

checkRange :: (Integer, Integer) -> Integer -> Bool
checkRange (low, high) x =
  x >= low && x <= high

testUniformRandom :: Integer -> Integer -> Word64 -> Bool
testUniformRandom a b seed =
  let low  = min a b
      high = max a b

   in checkRange (low, high) . runWithSeed seed $ do
        uniformIntDist a b

testDiscreteDistributionInRange :: [Word64] -> Word64 -> Bool
testDiscreteDistributionInRange xs seed =
  let ddh = buildDDH xs
      minVal = 0
      maxVal = length xs - 1
   in length xs == 0 || sum xs == 0 ||
        ((\x -> x >= minVal && x <= maxVal) . runWithSeed seed $
          discreteDist ddh)

testNoZeroDiscreteDistributionPick :: [Word64] -> Word64 -> Bool
testNoZeroDiscreteDistributionPick xs seed =
  let ddh = buildDDH xs
      minVal = 0
      maxVal = length xs - 1
   in length xs == 0 || sum xs == 0 ||
        ((\x -> (xs !! x) /= 0) . runWithSeed seed $
          discreteDist ddh)

tests =
  [ testProperty "random range" testUniformRandom
  , testProperty "discrete dist range" testDiscreteDistributionInRange
  , testProperty "no non-zero discrete dist pick" testNoZeroDiscreteDistributionPick
  ]

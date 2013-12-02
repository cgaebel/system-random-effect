{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Main ( main ) where

import Control.Eff
import Control.Eff.State.Strict

import System.Random.Effect

import Control.Monad (void)
import Data.Vector ( Vector )
import qualified Data.Vector as V
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
   in sum xs == 0 ||
        ((\x -> x >= minVal && x <= maxVal) . runWithSeed seed $
          discreteDist ddh)

testNoZeroDiscreteDistributionPick :: [Word64] -> Word64 -> Bool
testNoZeroDiscreteDistributionPick xs seed =
  let ddh = buildDDH xs
   in sum xs == 0 ||
        ((\x -> (xs !! x) /= 0) . runWithSeed seed $
          discreteDist ddh)

testUnsafeThaw :: [Word64] -> Word64 -> Bool
testUnsafeThaw xs seed =
  let ddh = buildDDH xs
   in sum xs == 0 ||
        (runWithSeed seed $ do
          _ <- discreteDist ddh
          _ <- discreteDist ddh
          _ <- discreteDist ddh
          _ <- discreteDist ddh
          _ <- discreteDist ddh
          return True)

testUniformIntegralDist :: Integer -> Integer -> Word64 -> Bool
testUniformIntegralDist a b seed =
  let r1 = runWithSeed seed $ uniformIntDist      a b
      r2 = runWithSeed seed $ uniformIntegralDist a b
   in r1 == r2

testKnuthShuffle :: [Int] -> Word64 -> Bool
testKnuthShuffle xs' seed =
  let xs = V.fromList xs'
      countIf pred = V.length . V.filter pred
      shuffled = runWithSeed seed (knuthShuffle xs)
      sameCount v1 v2 = V.all id
                      $ V.map (\x -> countIf (== x) v1
                                  == countIf (== x) v2) v1
   in sameCount xs shuffled

tests =
  [ testProperty "random range" testUniformRandom
  , testProperty "discrete dist range" testDiscreteDistributionInRange
  , testProperty "no non-zero discrete dist pick" testNoZeroDiscreteDistributionPick
  , testProperty "unsafeThaw is okay to use" testUnsafeThaw
  , testProperty "testUniformIntegralDist == testUniformIntDist" testUniformIntegralDist
  , testProperty "knuth shuffle" testKnuthShuffle
  ]

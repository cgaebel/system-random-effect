{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Main ( main ) where

import Prelude hiding (all)

import Control.Eff as Eff
import Control.Eff.Lift
import Control.Eff.State.Strict

import System.Random.Effect

import Control.Applicative
import Control.Monad.ST
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.QuickCheck2

import Test.QuickCheck
import Test.QuickCheck.Monadic as Test
import Test.QuickCheck.Property ( morallyDubiousIOProperty )

main :: IO ()
main = defaultMain tests

runWithSeed :: Word64 -> Eff (State Random :> ()) a -> a
runWithSeed seed = Eff.run . runRandomState (mkRandom seed)

runIOWithSeed :: Word64 -> Eff (State Random :> Lift IO :> ()) a -> IO a
runIOWithSeed seed = runLift . runRandomState (mkRandom seed)

checkRange :: (Integer, Integer) -> Integer -> Bool
checkRange (low, high) x =
  x >= low && x <= high

testUniformRandom :: Integer -> Integer -> Word64 -> Bool
testUniformRandom a b seed =
  let low  = min a b
      high = max a b

   in checkRange (low, high) . runWithSeed seed $ do
        uniformIntDist a b

newtype DiscreteWeights = DW [Word64]
  deriving Show

instance Arbitrary DiscreteWeights where
  arbitrary      = DW <$> suchThat (listOf arbitrary) ((> 0) . sum)
  shrink (DW xs) = map DW (shrink xs)

testDiscreteDistributionInRange :: DiscreteWeights -> Word64 -> Bool
testDiscreteDistributionInRange (DW xs) seed =
  let ddh = buildDDH xs
      minVal = 0
      maxVal = length xs - 1
   in (\x -> x >= minVal && x <= maxVal) . runWithSeed seed $
          discreteDist ddh

testNoZeroDiscreteDistributionPick :: DiscreteWeights -> Word64 -> Bool
testNoZeroDiscreteDistributionPick (DW xs) seed =
  let ddh = buildDDH xs
   in (\x -> (xs !! x) /= 0) . runWithSeed seed $
          discreteDist ddh

testUnsafeThaw :: DiscreteWeights -> Word64 -> Bool
testUnsafeThaw (DW xs) seed =
  let ddh = buildDDH xs
   in runWithSeed seed $ do
          _ <- discreteDist ddh
          _ <- discreteDist ddh
          _ <- discreteDist ddh
          _ <- discreteDist ddh
          _ <- discreteDist ddh
          return True

testUniformIntegralDist :: Integer -> Integer -> Word64 -> Bool
testUniformIntegralDist a b seed =
  let r1 = runWithSeed seed $ uniformIntDist      a b
      r2 = runWithSeed seed $ uniformIntegralDist a b
   in r1 == r2

testKnuthShuffle :: [Int] -> Word64 -> Bool
testKnuthShuffle xs' seed =
  let xs = V.fromList xs'
      countIf f = V.length . V.filter f
      shuffled = runWithSeed seed (knuthShuffle xs)
      sameCount v1 v2 = V.all id
                      $ V.map (\x -> countIf (== x) v1
                                  == countIf (== x) v2) v1
   in sameCount xs shuffled

testKnuthShuffleM :: [Int] -> Word64 -> IO Bool
testKnuthShuffleM xs' seed = do
  let xs = V.fromList xs'
      countIf f = V.length . V.filter f
      shuffled = do
        vs <- V.thaw xs
        runIOWithSeed seed (knuthShuffleM vs)
        V.freeze vs
      sameCount v1 v2 = V.all id
                      $ V.map (\x -> countIf (== x) v1
                                  == countIf (== x) v2) v1
  shuf <- shuffled
  return (sameCount xs shuf)

testKnuthShuffleEquivalence :: [Int] -> Word64 -> IO Bool
testKnuthShuffleEquivalence xs seed = do
  let vs  = V.fromList xs
      ks1 = runWithSeed seed (knuthShuffle vs)

  xs' <- V.thaw vs
  runIOWithSeed seed (knuthShuffleM xs')
  ks2 <- V.freeze xs'

  return (ks1 == ks2)

testSecureRandom :: Integer -> Integer -> IO Bool
testSecureRandom a b = do
  let low  = min a b
      high = max a b

  runLift $ do
    rng <- mkSecureRandomIO
    return $ Eff.run $ runRandomState rng $
      checkRange (low, high) <$> uniformIntDist a b

(|>) :: b -> (b -> c) -> c
(|>) = flip ($)

histogram :: Vector Integer -> Vector Int
histogram v = runST $ do
  mv <- MV.replicate (fromIntegral (V.maximum v + 1)) 0
  V.forM_ v $ \i' -> do
    let i = (fromIntegral i') :: Int
    !x <- MV.read mv i
    MV.write mv i (x+1)
  V.unsafeFreeze mv

-- check if all uniformly distributed numbers are within 10% of the mean.
-- 10% was a number chosen arbitrarily.
simpleUniformIntDistTest :: Word64 -> Bool
simpleUniformIntDistTest seed =
  let nBuckets = 5 :: Int
      samplesPerBucket = 4000 :: Int
      nSamples = nBuckets * samplesPerBucket
      maxDelta = (fromIntegral samplesPerBucket) `div` 10
      nums     = uniformIntDist 0 (fromIntegral (nBuckets - 1))
              |> V.replicateM nSamples
              |> runWithSeed seed
      hist     = histogram nums
   in V.all (\x -> x >= samplesPerBucket - maxDelta
                && x <= samplesPerBucket + maxDelta) hist

runIOProperty ::
    Testable prop =>
    IO prop ->
    Property
runIOProperty prop
    = monadicIO $ Test.run prop >>= stop

tests :: [Test]
tests =
  [ testProperty "random range" testUniformRandom
  , testProperty "discrete dist range" testDiscreteDistributionInRange
  , testProperty "no non-zero discrete dist pick" testNoZeroDiscreteDistributionPick
  , testProperty "unsafeThaw is okay to use" testUnsafeThaw
  , testProperty "testUniformIntegralDist == testUniformIntDist" testUniformIntegralDist
  , testProperty "knuth shuffle" testKnuthShuffle
  , testProperty "knuth shuffle (monadic)" (\xs seed -> runIOProperty $ testKnuthShuffleM xs seed)
  , testProperty "knuth shuffle equivalence" (\xs seed -> runIOProperty $ testKnuthShuffleEquivalence xs seed)
  , testProperty "secure random" (\a b -> runIOProperty $ testSecureRandom a b)
  , testProperty "uniformIntDist is uniform-ish" simpleUniformIntDistTest
  ]

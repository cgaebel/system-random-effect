{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Main ( main ) where

import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict

import System.Random.Effect

import Control.Applicative
import Control.Monad (void)
import Control.Monad.ST
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Data.Word
import Data.Typeable

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Test.HUnit hiding (State)
import Test.QuickCheck
import Test.QuickCheck.Property ( morallyDubiousIOProperty )

main :: IO ()
main = defaultMain tests

runWithSeed :: Word64 -> Eff (State Random :> ()) a -> a
runWithSeed seed = run . runRandomState (mkRandom seed)

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
      countIf pred = V.length . V.filter pred
      shuffled = runWithSeed seed (knuthShuffle xs)
      sameCount v1 v2 = V.all id
                      $ V.map (\x -> countIf (== x) v1
                                  == countIf (== x) v2) v1
   in sameCount xs shuffled

testKnuthShuffleM :: [Int] -> Word64 -> IO Bool
testKnuthShuffleM xs' seed = do
  let xs = V.fromList xs'
      countIf pred = V.length . V.filter pred
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
    return $ run $ runRandomState rng $
      checkRange (low, high) <$> uniformIntDist a b

tests =
  [ testProperty "random range" testUniformRandom
  , testProperty "discrete dist range" testDiscreteDistributionInRange
  , testProperty "no non-zero discrete dist pick" testNoZeroDiscreteDistributionPick
  , testProperty "unsafeThaw is okay to use" testUnsafeThaw
  , testProperty "testUniformIntegralDist == testUniformIntDist" testUniformIntegralDist
  , testProperty "knuth shuffle" testKnuthShuffle
  , testProperty "knuth shuffle (monadic)" (\xs seed -> morallyDubiousIOProperty $ testKnuthShuffleM xs seed)
  , testProperty "knuth shuffle equivalence" (\xs seed -> morallyDubiousIOProperty $ testKnuthShuffleEquivalence xs seed)
  , testProperty "secure random" (\a b -> morallyDubiousIOProperty $ testSecureRandom a b)
  ]

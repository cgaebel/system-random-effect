{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Main ( main ) where

import Data.Word

import Control.Eff
import Control.Eff.State.Strict
import Control.Applicative
import Control.DeepSeq
import Control.Exception ( evaluate )
import Control.Monad

import Criterion
import Criterion.Config
import Criterion.Main

import System.Random.Effect

fastIntDist :: Member (State Random) r
            => Int
            -> Int
            -> Eff r Int
fastIntDist a' b' = do
  let a = min a' b'
      b = max a' b'
      range = b - a

  x <- randomInt
  return (x `rem` range + a)
{-# NOINLINE fastIntDist #-}

instance NFData a => Benchmarkable (Eff (State Random :> ()) a) where
  run eff n = do
    let res = Control.Eff.run $ runRandomState (mkRandom 0) $ do
                last <$> replicateM n eff

    _ <- evaluate (rnf res)
    return ()

main :: IO ()
main =
  defaultMainWith config (return ()) [
      bcompare [
        bench "fastIntDist"         (fastIntDist         1 1000 :: Eff (State Random :> ()) Int)
      , bench "uniformIntDist"      (uniformIntDist      1 1000 :: Eff (State Random :> ()) Integer)
      , bench "uniformIntegralDist" (uniformIntegralDist 1 1000 :: Eff (State Random :> ()) Word64)
      ]
    , bcompare [
        bench "normalDist"          (normalDist     0 10 :: Eff (State Random :> ()) Double)
      , bench "lognormalDist"       (lognormalDist  0 10 :: Eff (State Random :> ()) Double)
      ]
    , bcompare [
        bench "randomWord32"        (randomWord        :: Eff (State Random :> ()) Word)
      , bench "randomWord64"        (randomWord64      :: Eff (State Random :> ()) Word64)
      , bench "randomBits32"        (randomBits        :: Eff (State Random :> ()) Word32)
      , bench "randomBits64"        (randomBits        :: Eff (State Random :> ()) Word64)
      , bench "randomBitList64"     (randomBitList 64  :: Eff (State Random :> ()) [Bool])
      , bench "randomBitList128"    (randomBitList 128 :: Eff (State Random :> ()) [Bool])
      ]
    , bcompare [
        bench "randomDouble"        (randomDouble         :: Eff (State Random :> ()) Double)
      , bench "uniformRealDist"     (uniformRealDist 0 10 :: Eff (State Random :> ()) Double)
      , bench "linearRealDist"      (linearRealDist  0 10 :: Eff (State Random :> ()) Double)
      ]
    , bench "binomialDist"            (binomialDist 1000 0.5 :: Eff (State Random :> ()) Int)
    ]
  where
    config = defaultConfig {
    --  cfgPerformGC = ljust True
     cfgConfInterval = ljust 0.99
    }

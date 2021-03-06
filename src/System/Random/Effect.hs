{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
-- | A random number effect, using a pure mersenne twister under
--   the hood. This algorithm is not suitable for cryptography!
--
--   If you need cryptographically secure random numbers,
--   you MUST use 'mkSecureRandomIO'. Otherwise, 'mkRandom' and
--   'mkRandomIO' are much faster.
--
--   This effect should be plug-and-play with any application
--   making use of extensible effects.
--
--   Patches, even for the smallest of documentation bugs, are
--   always welcome!
module System.Random.Effect ( Random
                            -- * Seeding
                            , mkRandom
                            , mkRandomIO
                            , mkSecureRandomIO
                            -- * Running
                            , forRandEff
                            , runRandomState
                            -- * Uniform Distributions
                            , uniformIntDist
                            , uniformIntegralDist
                            , uniformRealDist
                            -- * Linear Distributions
                            , linearRealDist
                            -- * Bernoulli Distributions
                            , bernoulliDist
                            , binomialDist
                            , negativeBinomialDist
                            , geometricDist
                            -- * Poisson Distributions
                            , poissonDist
                            , exponentialDist
                            , gammaDist
                            , weibullDist
                            , extremeValueDist
                            -- * Normal Distributions
                            , normalDist
                            , lognormalDist
                            , chiSquaredDist
                            , cauchyDist
                            , fisherFDist
                            , studentTDist
                            -- * Sampling Distributions
                            , DiscreteDistributionHelper
                            , buildDDH
                            , discreteDist
                            , piecewiseConstantDist
                            , piecewiseLinearDist
                            -- * Shuffling
                            , knuthShuffle
                            , knuthShuffleM
                            -- * Raw Generators
                            , randomInt
                            , randomInt64
                            , randomWord
                            , randomWord64
                            , randomDouble
                            , randomBits
                            , randomBitList
                            ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.List
import Data.Ratio
import Data.Typeable
import Data.Vector.Algorithms.Search
import Data.Vector ( Vector, (//), (!) )
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as M
import Data.Word
import Statistics.Distribution

import qualified Statistics.Distribution.CauchyLorentz as DCL
import qualified Statistics.Distribution.ChiSquared    as DC
import qualified Statistics.Distribution.Exponential   as DE
import qualified Statistics.Distribution.FDistribution as DF
import qualified Statistics.Distribution.Gamma         as DG
import qualified Statistics.Distribution.Normal        as DN
--import qualified Statistics.Distribution.Poisson       as DP
import qualified Statistics.Distribution.StudentT      as DS

import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict

import System.Random.Effect.Raw

#if MIN_VERSION_base(4,7,0)
#else
#define Typeable Typeable1
#define FiniteBits Bits

finiteBitSize :: Bits a => a -> Int
finiteBitSize = bitSize
{-# INLINE finiteBitSize #-}
#endif

randomDouble :: Member (State Random) r
             => Eff r Double
randomDouble = do
  r <- randomWord64
  return (fromIntegral (r `div` 2048) / 9007199254740992)
{-# INLINE randomDouble #-}

-- | Yields a set of random from the internal generator,
--   using 'randomWord64' internally.
randomBits :: ( Member (State Random) r
              , FiniteBits x)
           => Eff r x
randomBits = do
  let z     = clearBit (bit 0) 0 -- zero, so we can get the number of bits
      nBits = finiteBitSize z

  -- we OR with zero to get the number of bits above.
  -- it shouldn't affect the output.
  (z .|.) . bitsToInteger <$> randomBitList nBits
{-# INLINE randomBits #-}

-- | Returns a list of bits which have been randomly generated.
randomBitList :: Member (State Random) r
              => Int -- ^ The number of bits to generate
              -> Eff r [Bool]
randomBitList k = do
  let iters = (k `div` 64) + 1

      breakBits w = map (testBit w) [0..(finiteBitSize w - 1)]

  word64s <- replicateM iters randomWord64

  return $ take k (concatMap breakBits word64s)

-- | Returns the maximum set bit in an integer.
maxBit :: Num a => Integer -> a
maxBit = let loop !k 0 = k
             loop !k x = loop (k+1) (x `unsafeShiftR` 1)
         in loop 0

-- | Repeat a computation until it succeeds a test.
loopUntil :: Monad m => (a -> Bool) -> m a -> m a
loopUntil f c = do
  x <- c
  case f x of
    True  -> return x
    False -> loopUntil f c

b2i :: Bits a => a -> Bool -> a
b2i x True  = setBit x 0
b2i x False = x
{-# INLINE b2i #-}

bitsToInteger :: Bits a => [Bool] -> a
bitsToInteger =
  let z = clearBit (bit 0) 0
   in foldl' (\x -> b2i (unsafeShiftL x 1)) z

-- | Implementation of 'uniformIntDist', with shared calculations
--   with other uniformIntDists passed as parameters. This lets us
--   share work with other calls to 'uniformIntDist' with the same
--   parameters.
--
--   Returns a number in the inclusive range [0, range].
--
--   'numBits' is the number of bits in 'range'.
uniformIntDist' :: Member (State Random) r
                => Integer -- ^ range
                -> Int     -- ^ numBits
                -> Eff r Integer
uniformIntDist' range nBits
  | range == 0 = return 0
  | otherwise  =
    loopUntil (<= range) $
      bitsToInteger <$> randomBitList nBits

-- | Generates a uniformly distributed random number in
--   the inclusive range [a, b].
uniformIntDist :: Member (State Random) r
               => Integer -- ^ a
               -> Integer -- ^ b
               -> Eff r Integer
uniformIntDist a' b' =
  let a     = min a' b'
      b     = max a' b'
      range = b - a
      maxB  = maxBit range
   in (a+) <$> uniformIntDist' range maxB
{-# INLINE uniformIntDist #-}

-- | Generates a uniformly distributed random number in
--   the inclusive range [a, b].
--
--   This function is more flexible than 'uniformIntDist'
--   since it relaxes type constraints, but passing in
--   constant bounds such as @uniformIntegralDist 0 10@
--   will warn with -Wall.
uniformIntegralDist :: ( Member (State Random) r
                      , Integral a )
                    => a -- ^ a
                    -> a -- ^ b
                    -> Eff r a
uniformIntegralDist a b =
  fromInteger <$> uniformIntDist (toInteger a)
                                 (toInteger b)
{-# INLINE uniformIntegralDist #-}

-- | The part of 'uniformRealDist' that does all the work.
--   We factor it out so we can inline 'uniformRealDist',
--   and possibly share as much work as possible.
uniformRealDist' :: Member (State Random) r
                 => Double -- ^ a
                 -> Double -- ^ range
                 -> Eff r Double
uniformRealDist' a range = do
  d <- randomDouble
  return (d * range + a)

-- | Generates a uniformly distributed random number in
--   the range [a, b).
--
--    NOTE: This code might not be correct, in that the
--          returned value may not be perfectly uniformly
--          distributed. If you know how to make one of
--          these a better way, PLEASE send me a pull request.
--          I just stole this implementation from the C++11
--          <random> header.
uniformRealDist :: Member (State Random) r
                => Double -- ^ a
                -> Double -- ^ b
                -> Eff r Double
uniformRealDist a' b' =
  let a     = min a' b'
      b     = max a' b'
      range = b - a

   in uniformRealDist' a range
{-# INLINE uniformRealDist #-}

-- | Generates a linearly-distributed random number in the range @[a, b)@;
-- @a@ with a probability of 0.
-- This code is not guaranteed to be correct.
linearRealDist :: Member (State Random) r
               => Double
               -> Double
               -> Eff r Double
linearRealDist a' b' = do
  let (a, b) = (min a' b', max a' b')
      range = b - a
  -- P(x) =
  --        { 0        x < a
  --        { k*(x-a)      a <= x < b
  --        { 0                     b <= x
  -- CDF(x) =
  --        { 0                x < a
  --        { k*(1/2)*(x-a)^2      a <= x < b
  --        { 1                             b <= x
  -- Choose k such that CDF is continuous:
  --    lim
  --   x->b-  CDF(x) = k*(1/2)*(b-a)^2 = 1
  -- k = 2/(b-a)^2 = 2/range^2
  -- Thus CDF(x, a<=x<b) = ((x-a)/range)^2
  --      CDF^-1 :: (A, 0<=A<1) -> (x, a<=x<b)
  --      CDF^1(A) = sqrt(A) * range + a
  -- Given a uniformly-distributed number in [0, 1),
  -- we can make it linearly-distributed by applying CDF^-1.
  r <- randomDouble
  return $ sqrt(r) * range + a

-- | Samples a continuous distribution.
--
--   Generates random numbers as if they were sampled by the given
--   distribution.
--
--   This is implemented with the inverse transform rule:
--   <http://en.wikipedia.org/wiki/Inverse_transform_sampling>.
sampleContDist :: ( Member (State Random) r
                  , ContDistr d )
               => d -- ^ The distribution to sample.
               -> Eff r Double
sampleContDist d =
  quantile d <$> randomDouble

-- | Produces random boolean values, according to a discrete probability.
--
--   k must be in the range [0, 1].
bernoulliDist :: Member (State Random) r
              => Rational -- ^ k: The fraction of results which should be true.
              -> Eff r Bool
bernoulliDist k
  | k < 0 || k > 1 =
    error $ "system-random-effect: bernoulliDist: fraction 'k' is out of range: " ++ show k
  | k == 0         = return False
  | otherwise      = do
    let (n, d) = (numerator k, denominator k)
    dist <- uniformIntDist 1 d
    return $ dist <= n
{-# INLINE bernoulliDist #-}

-- | The value obtained is the number of successes in a sequence of
--   t yes/no experiments, each of which succeeds with probability p.
--
--   t must be >= 0
--   p must be in the range [0, 1].
binomialDist :: Member (State Random) r
             => Int  -- ^ t
             -> Rational -- ^ p
             -> Eff r Int
binomialDist t p
  | p < 0 || p > 1 =
    error $ "system-random-effect: binomialDist: fraction 'p' is out of range: " ++ show p
  | t == 0 || p == 0 = return 0
  | p == 1           = return t
  | otherwise        = do
    trials <- V.replicateM t randomDouble

    let p'           = realToFrac p
        numSuccesses =
          V.foldl' (\s x -> if x <= p' then s+1 else s) 0 trials

    return numSuccesses

-- | The value represents the number of failures in a series of
--   independent yes/no trials (each succeeds with probability p),
--   before exactly k successes occur.
--
--   p must be in the range (0, 1]
--   k must be >= 0
--
--   Warning: NOT IMPLEMENTED!
negativeBinomialDist :: Member (State Random) r
                     => Rational -- ^ p
                     -> Integer  -- ^ k
                     -> Eff r Integer
negativeBinomialDist p k
  | p <= 0 || p > 1  =
    error $ "system-random-effect: negativeBinomialDist: fraction 'p' is out of range: " ++ show p
  | p == 1 || k == 0 = return 0
  | otherwise        =
    error "system-random-effect: negativeBinomialDist: TODO. Patches welcome!"

-- | The value represents the number of yes/no trials (each
--   succeeding with probability p) which are necessary to
--   obtain a single success.
--
--   'geometricDist' p is equivalent to negativeBinomialDist 1 p
--
--   p must be in the range (0, 1]
--
--   Warning: NOT IMPLEMENTED!
geometricDist :: Member (State Random) r
              => Rational -- ^ p
              -> Eff r Integer
geometricDist p
  | p <= 0 || p > 1 =
    error $ "system-random-effect: geometricDist: fraction 'p' is out of range: " ++ show p
  | p == 1          = return 0
  | otherwise       =
    error "system-random-effect: geometricDist: TODO: Patches welcome!"

-- | The value obtained is the probability of exactly i
--   occurrences of a random event if the expected, mean
--   number of its occurrence under the same conditions
--   (on the same time/space interval) is μ.
--
--   Warning: NOT IMPLEMENTED!
poissonDist :: Member (State Random) r
            => Double       -- ^ μ
            -> Eff r Double -- ^ i
poissonDist =
  error "system-random-effect: poissonDist: TODO: Patches welcome!"

-- | The value obtained is the time/distance until the next
--   random event if random events occur at constant rate λ
--   per unit of time/distance. For example, this distribution
--   describes the time between the clicks of a Geiger counter
--   or the distance between point mutations in a DNA strand.
--
--   This is the continuous counterpart of 'geometricDist'.
exponentialDist :: Member (State Random) r
                => Double -- ^ λ. Scale parameter.
                -> Eff r Double
exponentialDist lambda =
  sampleContDist (DE.exponential lambda)

-- | For floating-point α, the value obtained is the sum of α
--   independent exponentially distributed random variables,
--   each of which has a mean of β.
gammaDist :: Member (State Random) r
          => Double -- ^ α. The shape parameter.
          -> Double -- ^ β. The scale parameter.
          -> Eff r Double
gammaDist alpha beta =
  sampleContDist (DG.gammaDistr alpha beta)

-- | Generates random numbers as sampled from a Weibull
--   distribution. It was originally identified to describe
--   particle size distribution.
weibullDist :: Member (State Random) r
            => Double -- ^ α. The shape parameter.
            -> Double -- ^ β. The scale parameter.
            -> Eff r Double
weibullDist alpha beta = do -- lambda = beta, k = alpha
  r <- randomDouble
  return $ beta * (-log (1-r)) ** (1/alpha)

-- | ???
--
-- Warning: NOT IMPLEMENTED!
extremeValueDist :: Member (State Random) r
                 => Double -- ^ α. The shape parameter.
                 -> Double -- ^ β. The scale parameter.
                 -> Eff r Double
extremeValueDist =
  error "system-random-effect: extremeValueDist: TODO: Patches welcome!"


-- | Generates random numbers as sampled from the
--   normal distribution.
normalDist :: Member (State Random) r
           => Double -- ^ μ. The mean.
           -> Double -- ^ σ. The standard deviation.
           -> Eff r Double
normalDist mu sigma =
  sampleContDist (DN.normalDistr mu sigma)

-- | Generates a log-normally distributed random number.
--   This is based off of sampling the normal distribution,
--   and then following the instructions at
--   <http://en.wikipedia.org/wiki/Log-normal_distribution#Generating_log-normally_distributed_random_variates>.
lognormalDist :: Member (State Random) r
              => Double -- ^ μ. The mean.
              -> Double -- ^ σ. The standard deviation.
              -> Eff r Double
lognormalDist mu sigma = do
  z <- sampleContDist simpleND
  return $ exp (mu + sigma*z)

-- | Share this computation over every call of lognormalDist
simpleND :: DN.NormalDistribution
simpleND = DN.normalDistr 0 1
{-# NOINLINE simpleND #-}

-- | Produces random numbers according to a chi-squared distribution.
chiSquaredDist :: Member (State Random) r
               => Int -- ^ n. The number of degrees of freedom.
               -> Eff r Double
chiSquaredDist n
  | n <= 0    =
    error $ "system-random-effect: chiSquaredDist: invalid degrees of freedom: " ++ show n
  | otherwise =
    sampleContDist (DC.chiSquared n)

-- | Produced random numbers according to a Cauchy (or Lorentz) distribution.
cauchyDist :: Member (State Random) r
           => Double -- ^ Central point
           -> Double -- ^ Scale parameter (full width half maximum)
           -> Eff r Double
cauchyDist a b =
  sampleContDist (DCL.cauchyDistribution a b)

-- | Produces random numbers according to an F-distribution.
--
--   m and n are the degrees of freedom.
fisherFDist :: Member (State Random) r
            => Int -- ^ m
            -> Int -- ^ n
            -> Eff r Double
fisherFDist m n =
  sampleContDist (DF.fDistribution m n)

-- | This distribution is used when estimating the mean of an
--   unknown normally distributed value given n+1 independent
--   measurements, each with additive errors of unknown standard
--   deviation, as in physical measurements. Or, alternatively,
--   when estimating the unknown mean of a normal distribution
--   with unknown standard deviation, given n+1 samples.
studentTDist :: Member (State Random) r
             => Double -- ^ The number of degrees of freedom
             -> Eff r Double
studentTDist d =
  sampleContDist (DS.studentT d)

-- | Contains a sorted list of cumulative probabilities, so we
--   can do a sample by generating a uniformly distributed random
--   number in the range [0, 1), and binary searching the vector
--   for where to put it.
newtype DiscreteDistributionHelper =
  DDH (Vector Rational)

-- | Performs O(n) work building a table which we can later use
--   sample with 'discreteDist'.
buildDDH :: [Word64] -> DiscreteDistributionHelper
buildDDH xs =
  let vs  = V.fromList xs
      s   = fromIntegral (V.sum vs)
      vs' = V.map fromIntegral vs
      ns  = V.map (% s) vs' -- normalize the list
   in DDH (V.postscanl' (+) 0 ns)

-- | Given a pre-build 'DiscreteDistributionHelper' (use 'buildDDH'),
--   produces random integers on the interval [0, n), where the
--   probability of each individual integer i is defined as w_i/S,
--   that is the weight of the ith integer divided by the sum of all
--   n weights.
--
--   i.e. This function produces an integer with probability equal to
--   the weight given in its index into the parameter to 'buildDDH'.
discreteDist :: Member (State Random) r
             => DiscreteDistributionHelper
             -> Eff r Int
discreteDist (DDH xs) = do
  y <- realToFrac <$> randomDouble
  return $ runST $ do
    mv <- V.unsafeThaw xs
    binarySearch mv y

-- Select an interval at random from a collection of intervals and
-- their weights, then run a provided function between its endpoints.
piecewiseDist :: Member (State Random) r
              => (Double -> Double -> Eff r Double)
              -> [Double]
              -> DiscreteDistributionHelper
              -> Eff r Double
piecewiseDist f intervals weights@(DDH rs)
  | V.length rs + 1 /= length intervals =
    error $ "system-random-effect: piecewiseDist:"
      ++ " Incongruent parameter lengths."
      ++ " intervals=" ++ show intervals
      ++ " weights="   ++ show rs
  | otherwise = do
    idx <- discreteDist weights

    let vints = V.fromList intervals
        (l, r) = (vints V.! idx, vints V.! (idx+1))
    f l r
{-# INLINE piecewiseDist #-}

-- | This function produces random floating-point numbers, which
--   are uniformly distributed within each of the several subintervals
--   [b_i, b_(i+1)), each with its own weight w_i. The set of interval
--   boundaries and the set of weights are the parameters of this
--   distribution.
--
--   For example, @piecewiseConstantDist [ 0, 1, 10, 15 ]
--                             (buildDDH   [ 1, 0,  1 ])@
--   will produce values between 0 and 1 half the time, and values
--   between 10 and 15 the other half of the time.
piecewiseConstantDist :: Member (State Random) r
                      => [Double] -- ^ Intervals
                      -> DiscreteDistributionHelper -- ^ Weights
                      -> Eff r Double
piecewiseConstantDist = piecewiseDist uniformRealDist

-- | This function produces random floating-point numbers, which
--   are distributed with linearly-increasing probability within
--   each of the several subintervals [b_i, b_(i+1)), each with its own
--   weight w_i. The set of interval boundaries and the set of weights
--   are the parameters of this distribution.
--
--   For example, `piecewiseLinearDist [ 0, 1, 10, 15 ]
--                             (buildDDH   [ 1, 0,  1 ])`
--   will produce values between 0 and 1 half the time, and values
--   between 10 and 15 the other half of the time.
piecewiseLinearDist :: Member (State Random) r
                    => [Double] -- ^ Intervals
                    -> DiscreteDistributionHelper -- ^ Weights
                    -> Eff r Double
piecewiseLinearDist = piecewiseDist linearRealDist

-- | Shuffle a mutable vector.
knuthShuffleM :: ( PrimMonad m
                 , Applicative m
                 , Typeable m
                 , Member (State Random) r
                 , SetMember Lift (Lift m) r
                 )
              => MVector (PrimState m) a
              -> Eff r ()
knuthShuffleM v = forM_ [0..iLast]
                $ \i -> uniformIntegralDist 0 iLast
                    >>= lift . swap i
  where
    iLast = M.length v - 1

    swap i j = do
        [vi, vj] <- mapM (M.read v) [i, j]
        mapM_ (uncurry $ M.write v) [(i, vj), (j, vi)]

-- | Shuffle an immutable vector.
knuthShuffle :: Member (State Random) r
             => Vector a
             -> Eff r (Vector a)
knuthShuffle v0 = foldM swap v0 [0..iLast]
  where
    iLast = V.length v0 - 1

    swap :: Member (State Random) r => Vector a -> Int -> Eff r (Vector a)
    swap v i = do
        j <- uniformIntegralDist 0 iLast
        return $ v // zip [j, i] ((v !) <$> [i, j])

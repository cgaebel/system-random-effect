-- | Contains a random number generator for the deterministic,
--   fast, but NOT SECURE mt19937 random number generator. This
--   is a very versatile (and pure!) generator that works for
--   pretty much everything EXCEPT security.
--
--   Seriously. Don't use this for crypto.
--
--   This is the minimal definition for a random number generator.
--   new ones can be implemented by adding to the sum type 'Random'.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module System.Random.Effect.Raw ( Random
                                -- * Seeding
                                , mkRandom
                                , mkRandomIO
                                , mkSecureRandomIO
                                -- * Running
                                , runRandomState
                                -- * Raw Generators
                                , randomInt
                                , randomWord
                                , randomInt64
                                , randomWord64
                                ) where

import Control.Applicative
import Control.Arrow ( second )
import Data.Bits
import qualified Data.ByteString as B
import Data.Typeable
import Data.Int
import Data.Word

import qualified Crypto.Random as CR
import qualified System.Random.Mersenne.Pure64 as SR

import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict

-- | A random number generator. Either a fast, insecure mersenne
--   twister or a secure one, depending on which smart constructor
--   is used to construct this type.
data Random = FastRandom   {-# UNPACK #-} !SR.PureMT
            | SecureRandom {-# UNPACK #-} !CR.SystemRandom
  deriving Typeable

randomInt :: Member (State Random) r
          => Eff r Int
randomInt = randomF SR.randomInt srandomBits
{-# INLINE randomInt #-}

randomWord :: Member (State Random) r
           => Eff r Word
randomWord = randomF SR.randomWord srandomBits
{-# INLINE randomWord #-}

randomInt64 :: Member (State Random) r
            => Eff r Int64
randomInt64 = randomF SR.randomInt64 srandomBits
{-# INLINE randomInt64 #-}

randomWord64 :: Member (State Random) r
             => Eff r Word64
randomWord64 = randomF SR.randomWord64 srandomBits
{-# INLINE randomWord64 #-}

-- | Create a random number generator from a 'Word64' seed.
--   This uses the insecure (but fast) mersenne twister.
mkRandom :: Word64 -> Random
mkRandom = FastRandom . SR.pureMT
{-# INLINE mkRandom #-}

-- | Create a new random number generator, using the clocktime as the base for
--   the seed. This must be called from a computation with a lifted base effect
--   of 'IO'.
--
--   This is just a conveniently seeded mersenne twister.
mkRandomIO :: SetMember Lift (Lift IO) r
           => Eff r Random
mkRandomIO =
  FastRandom <$> lift SR.newPureMT
{-# INLINE mkRandomIO #-}

-- | Creates a new random number generator, using the system entropy source
--   as a seed. The random number generator returned from this function is
--   cryptographically secure, but not nearly as fast as the one returned
--   by 'mkRandom' and 'mkRandomIO'.
mkSecureRandomIO :: SetMember Lift (Lift IO) r
                 => Eff r Random
mkSecureRandomIO = do
  SecureRandom <$> lift CR.newGenIO
{-# INLINE mkSecureRandomIO #-}

-- | Runs an effectful random computation, returning the computation's result.
runRandomState :: Random
               -> Eff (State Random :> r) w
               -> Eff r w
runRandomState seed computation =
  snd <$> runState seed computation
{-# INLINE runRandomState #-}

foldBits :: (Bits a, Num a)
         => B.ByteString
         -> a
foldBits bs =
  let addByte byte bits =
        (bits `unsafeShiftL` 8) .|. fromIntegral byte
   in B.foldr' addByte 0 bs
{-# INLINE foldBits #-}

-- | Securely generate some random bits.
srandomBits :: ( Bits a
               , Num  a )
            => CR.SystemRandom
            -> (a, CR.SystemRandom)
srandomBits sr =
  let z      = clearBit (bit 0) 0
      nBytes = bitSize z `div` 8
   in case CR.genBytes nBytes sr of
        Left err -> error $ "system-random-effect: System.Random.Effect.Secure: genBytes: " ++ show err
        Right (bs, sr') -> (z .|. foldBits bs, sr')
{-# INLINE srandomBits #-}

-- | A generalized form of generating a random number of the correct type
--   from System.Random.Mersenne.Pure64.
randomF :: Member (State Random) r
        => (SR.PureMT       -> (a, SR.PureMT))
        -> (CR.SystemRandom -> (a, CR.SystemRandom))
        -> Eff r a
randomF f s = do
  old <- get
  let (val, new) = case old of
                     (FastRandom   r) -> second FastRandom   (f r)
                     (SecureRandom r) -> second SecureRandom (s r)
  put new
  return val
{-# INLINE randomF #-}

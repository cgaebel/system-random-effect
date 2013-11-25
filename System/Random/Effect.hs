{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module System.Random.Effect ( Random
                            , mkRandom
                            , mkRandomIO
                            , randomInt
                            , randomInt64
                            , randomWord
                            , randomWord64
                            , randomDouble
                            ) where

import Data.Int
import Data.Typeable
import Data.Word
import qualified System.Random.Mersenne.Pure64 as SR

import Control.Eff
import Control.Eff.State
import Control.Eff.Lift

-- | A pure mersenne twister pseudo-random number generator.
data Random = Random {-# UNPACK #-} !SR.PureMT
    deriving Typeable

-- | Create a random number generator from a 'Word64' seed.
mkRandom :: Word64 -> Random
mkRandom = Random . SR.pureMT
{-# INLINE mkRandom #-}

-- | Create a new random number generator, using the clocktime as the base for
--   the seed. This must be called from a computation with a lifted base effect
--   of 'IO'.
mkRandomIO :: Member (Lift IO) r
           => Eff r Random
mkRandomIO = lift (fmap Random SR.newPureMT)
{-# INLINE mkRandomIO #-}

-- | Runs an effectful random computation, returning the computation's result.
runRandomState :: Random
               -> Eff (State Random :> r) w
               -> Eff r w
runRandomState seed computation =
    fmap snd (runState seed computation)
{-# INLINE runRandomState #-}

-- | A generalized form of generating a random number of the correct type
--   from System.Random.Mersenne.Pure64.
randomF :: Member (State Random) r
        => (SR.PureMT -> (a, SR.PureMT))
        -> Eff r a
randomF f = do
    (Random old) <- getState
    let (val, new) = f old
    putState (Random new)
    return val
{-# INLINE randomF #-}

-- | Yield a new 'Int' value from the generator. The full 64 bits will be used
--   on a 64 bit machine.
randomInt :: Member (State Random) r => Eff r Int
randomInt = randomF SR.randomInt
{-# INLINE randomInt #-}

-- | Yield a new 'Word' value from the generator.
randomWord :: Member (State Random) r => Eff r Word
randomWord = randomF SR.randomWord
{-# INLINE randomWord #-}

-- | Yield a new 'Int64' value from the generator.
randomInt64 :: Member (State Random) r => Eff r Int64
randomInt64 = randomF SR.randomInt64
{-# INLINE randomInt64 #-}

-- | Yield a new 'Word64' value from the generator.
randomWord64 :: Member (State Random) r => Eff r Word64
randomWord64 = randomF SR.randomWord64
{-# INLINE randomWord64 #-}

-- | Yield a new 53-bit precise 'Double' value from the generator.
randomDouble :: Member (State Random) r => Eff r Double
randomDouble = randomF SR.randomDouble
{-# INLINE randomDouble #-}
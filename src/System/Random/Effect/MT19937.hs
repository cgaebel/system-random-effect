-- | Contains a random number generator for the deterministic,
--   fast, but NOT SECURE mt19937 random number generator. This
--   is a very versatile (and pure!) generator that works for
--   pretty much everything EXCEPT security.
--
--   Seriously. Don't use this for crypto.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- oh god why
module System.Random.Effect.MT19937 ( Random
                                    -- * Seeding
                                    , mkRandom
                                    , mkRandomIO
                                    -- * Running
                                    , runRandomState
                                    ) where

import Control.Applicative
import Data.Typeable
import Data.Word
import qualified System.Random.Mersenne.Pure64 as SR

import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict

import System.Random.Effect.Types

-- | A pure mersenne twister pseudo-random number generator.
newtype Random = Random SR.PureMT
  deriving Typeable

instance Member (State Random) r => RNG r where
  randomInt = randomF SR.randomInt
  {-# INLINE randomInt #-}

  randomWord = randomF SR.randomWord
  {-# INLINE randomWord #-}

  randomInt64 = randomF SR.randomInt64
  {-# INLINE randomInt64 #-}

  randomWord64 = randomF SR.randomWord64
  {-# INLINE randomWord64 #-}

  randomDouble = randomF SR.randomDouble
  {-# INLINE randomDouble #-}

-- | Create a random number generator from a 'Word64' seed.
mkRandom :: Word64 -> Random
mkRandom = Random . SR.pureMT
{-# INLINE mkRandom #-}

-- | Create a new random number generator, using the clocktime as the base for
--   the seed. This must be called from a computation with a lifted base effect
--   of 'IO'.
mkRandomIO :: SetMember Lift (Lift IO) r
           => Eff r Random
mkRandomIO = lift (fmap Random SR.newPureMT)
{-# INLINE mkRandomIO #-}

-- | Runs an effectful random computation, returning the computation's result.
runRandomState :: Random
               -> Eff (State Random :> r) w
               -> Eff r w
runRandomState seed computation =
  snd <$> runState seed computation
{-# INLINE runRandomState #-}

-- | A generalized form of generating a random number of the correct type
--   from System.Random.Mersenne.Pure64.
randomF :: Member (State Random) r
        => (SR.PureMT -> (a, SR.PureMT))
        -> Eff r a
randomF f = do
  (Random old) <- get
  let (val, new) = f old
  put (Random new)
  return val
{-# INLINE randomF #-}

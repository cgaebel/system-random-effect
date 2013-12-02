-- | Contains common types needed for the rest of
--   the library. These are re-exported in
--   'System.Random.Effect', so you probably don't
--   need to do anything with this module.
module System.Random.Effect.Types ( RNG(..) ) where

import Control.Eff
import Data.Int
import Data.Word

-- | Complete class for any random number generator. All other
--   random functions can parameterize over this minimal
--   definition of an RNG.
class RNG g where
  -- | Yield a new 'Int' value from the generator. The full 64 bits will be used
  --   on a 64 bit machine.
  randomInt    :: Eff g Int

  -- | Yield a new 'Word' value from the generator.
  randomInt64  :: Eff g Int64

  -- | Yield a new 'Int64' value from the generator.
  randomWord   :: Eff g Word

  -- | Yield a new 'Word64' value from the generator.
  randomWord64 :: Eff g Word64

  -- | Yield a new 53-bit precise 'Double' value from the generator.
  --   The returned number will be in the range [0, 1).
  randomDouble :: Eff g Double

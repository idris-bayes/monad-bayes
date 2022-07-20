module Control.Monad.Bayes.Sampler

import Control.Monad.Bayes.Interface
import System.Random

||| An 'IO' based random sampler.
public export
SamplerIO : Type -> Type
SamplerIO = IO

public export
MonadSample SamplerIO where
  random = System.Random.randomIO

||| Initialize a pseudo-random number generator using randomness supplied by the operating system. Providing the randomness, i.e. setting the random seed, is done automatically, so this is just `id`.
public export
sampleIO : SamplerIO a -> IO a
sampleIO = id

||| Like 'sampleIO' but with a custom seed.
public export
sampleIOwith : SamplerIO a -> Bits64 -> IO a
sampleIOwith m seed = srand seed >> m
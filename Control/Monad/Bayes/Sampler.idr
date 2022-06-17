module Control.Monad.Bayes.Sampler

import Control.Monad.Bayes.Interface
import System.Random

||| An 'IO' based random sampler (Note: the `ReaderT GenIO IO` pattern isn't necessary here, as there isn't infrastructure in Idris for providing, propagating, and updating a GenIO seed though a program, as seen in the Haskell System.Random.MWC library. A lot of functions in the Haskell Bayes.Sampler module are hence redundant.)
SamplerIO : Type -> Type
SamplerIO = IO

MonadSample SamplerIO where
  random = System.Random.randomIO

||| Initialize a pseudo-random number generator using randomness supplied by the operating system. Providing the randomness ,i.e. setting the random seed, is done automatically, so this is just `id`.
sampleIO : SamplerIO a -> IO a
sampleIO = id

||| Like 'sampleIO' but with a custom pseudo-random number generator.
sampleIOwith : SamplerIO a -> Bits64 -> IO a
sampleIOwith m seed = srand seed >> m

||| Like 'sampleIO', but with a fixed random seed; useful for reproducibility. Using `0` as the default seed.
sampleIOfixed : SamplerIO a -> IO a
sampleIOfixed = flip sampleIOwith 0

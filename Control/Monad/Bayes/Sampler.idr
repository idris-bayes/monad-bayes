module Control.Monad.Bayes.Sampler

import Control.Monad.Bayes.Interface
import System.Random

data GslRng : Type where
  MkGslRng : AnyPtr -> GslRng

%foreign "C:init_gsl_rng,libm"
init_rng : AnyPtr

initRng : GslRng
initRng = MkGslRng init_rng

%foreign "C:call_uniform,libm"
call_uniform : AnyPtr -> Double

test : (Double, Double)
test = let p = init_rng
           a = call_uniform p
           b = call_uniform p
       in (a, b)

||| An 'IO' based random sampler (I'm not sure if the `ReaderT GenIO IO` pattern is necessary, and for what particular reason we want to use it. The purpose of that pattern is to propagate and mutably update a GenIO seed though a program -- might need to talk to Ohad about this?)
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

||| Like 'sampleIO' but with a custom pseudo-random number generator.
public export
sampleIOwith : SamplerIO a -> Bits64 -> IO a
sampleIOwith m seed = srand seed >> m

||| Like 'sampleIO', but with a fixed random seed; useful for reproducibility. Using `0` as the default seed.
public export
sampleIOfixed : SamplerIO a -> IO a
sampleIOfixed = flip sampleIOwith 0

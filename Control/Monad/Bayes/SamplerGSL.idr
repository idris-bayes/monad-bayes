module Control.Monad.Bayes.SamplerGSL

import Control.Monad.Reader
import Statistics.Distribution.GSL
import Statistics.Distribution.Binomial
import Statistics.Distribution.Test
import Control.Monad.Bayes.Interface
import System.Random

--- | WARNING: Only works with local copy of the `my_gsl_lib.so` file, copied over from the `distributions` Idris package. Need to work out how to avoid requiring a local copy. 
--- | For now, use `Sampler.idr`

-- An 'IO' based random sampler
private
SamplerIO : Type -> Type
SamplerIO = ReaderT GslRng IO 

private
MonadSample SamplerIO where
  random          = MkReaderT (\seed => pure (uniform_gsl 0 1 seed))
  uniform min max = MkReaderT (\seed => pure (uniform_gsl min max seed))
  bernoulli p     = MkReaderT (\seed => pure (1 == binomial_gsl 1 p seed))
  binomial n p    = MkReaderT (\seed => pure (cast $ binomial_gsl n p seed))
  normal m s      = MkReaderT (\seed => pure (normal_gsl m s seed))

||| Like 'sampleIO', but with a fixed random seed; useful for reproducibility
private
sampleIOfixed : SamplerIO a -> IO a
sampleIOfixed m = do
  let rng_seed = init_rng
  runReaderT rng_seed m  

private
testSeed : IO () 
testSeed = print $ let seed = GSL.init_rng
                   in  (normal_gsl 17 0.5 seed, binomial_gsl 17 0.5 seed, uniform_gsl 0.5 17 seed)
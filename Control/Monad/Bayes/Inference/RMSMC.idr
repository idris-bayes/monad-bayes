module Control.Monad.Bayes.Inference.RMSMC

import Control.Monad.Bayes.Interface
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Weighted
-- import Control.Monad.Bayes.Traced.Basic as TrBas
-- import Control.Monad.Bayes.Traced.Dynamic as TrDyn
-- import Data.Monoid

-- -- | Resample-move Sequential Monte Carlo.
rmsmc :
  MonadSample m =>
  -- | number of timesteps
  Nat ->
  -- | number of particles
  Nat ->
  -- | number of Metropolis-Hastings transitions after each resampling
  Nat ->
  -- | model
  Sequential (Traced (Population m)) a ->
  Population m a
rmsmc k n t =
  marginal
    . sis (composeCopies t mhStep . hoistT resampleSystematic) k
    . Sequential.hoistFirst (hoistT (spawn {m} n >>))

-- -- | Resample-move Sequential Monte Carlo with a more efficient
-- -- tracing representation.
-- rmsmcBasic ::
--   MonadSample m =>
--   -- | number of timesteps
--   Int ->
--   -- | number of particles
--   Int ->
--   -- | number of Metropolis-Hastings transitions after each resampling
--   Int ->
--   -- | model
--   Sequential (TrBas.Traced (Population m)) a ->
--   Population m a
-- rmsmcBasic k n t =
--   TrBas.marginal
--     . sis (composeCopies t TrBas.mhStep . TrBas.hoistT resampleSystematic) k
--     . hoistS (TrBas.hoistT (spawn n >>))

-- -- | A variant of resample-move Sequential Monte Carlo
-- -- where only random variables since last resampling are considered
-- -- for rejuvenation.
-- rmsmcLocal ::
--   MonadSample m =>
--   -- | number of timesteps
--   Int ->
--   -- | number of particles
--   Int ->
--   -- | number of Metropolis-Hastings transitions after each resampling
--   Int ->
--   -- | model
--   Sequential (TrDyn.Traced (Population m)) a ->
--   Population m a
-- rmsmcLocal k n t =
--   TrDyn.marginal
--     . sis (TrDyn.freeze . composeCopies t TrDyn.mhStep . TrDyn.hoistT resampleSystematic) k
--     . hoistS (TrDyn.hoistT (spawn n >>))

-- -- | Apply a function a given number of times.
-- composeCopies :: Int -> (a -> a) -> (a -> a)
-- composeCopies k = withEndo (mconcat . replicate k)

-- withEndo :: (Endo a1 -> Endo a2) -> (a1 -> a1) -> a2 -> a2
-- withEndo f = appEndo . f . Endo

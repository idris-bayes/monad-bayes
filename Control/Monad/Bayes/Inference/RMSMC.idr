module Control.Monad.Bayes.Inference.RMSMC

import Control.Monad.Bayes.Interface
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Traced.Static
import Control.Monad.Bayes.Traced.Basic
import Control.Monad.Bayes.Traced.Dynamic

||| Resample-move Sequential Monte Carlo.
rmsmc :
  MonadSample m =>
  -- | number of timesteps
  Nat ->
  -- | number of particles
  Nat ->
  -- | number of Metropolis-Hastings transitions after each resampling
  Nat ->
  -- | model
  Sequential (Static.Traced (Population m)) a ->
  Population m a
rmsmc k n t =
  Static.marginal
    . sis (composeCopies t mhStep . hoistT resampleSystematic) k
    . Sequential.hoistFirst (hoistT (spawn {m} n >>))

||| Resample-move Sequential Monte Carlo with a more efficient
||| tracing representation.
rmsmcBasic :
  MonadSample m =>
  -- | number of timesteps
  Nat ->
  -- | number of particles
  Nat ->
  -- | number of Metropolis-Hastings transitions after each resampling
  Nat ->
  -- | model
  Sequential (Basic.Traced (Population m)) a ->
  Population m a
rmsmcBasic k n t =
  Basic.marginal
    . sis (composeCopies t Basic.mhStep . Basic.hoistT resampleSystematic) k
    . Sequential.hoistFirst (Basic.hoistT (spawn n >>))

||| A variant of resample-move Sequential Monte Carlo
||| where only random variables since last resampling are considered
||| for rejuvenation.
rmsmcLocal :
  MonadSample m =>
  -- | number of timesteps
  Nat ->
  -- | number of particles
  Nat ->
  -- | number of Metropolis-Hastings transitions after each resampling
  Nat ->
  -- | model
  Sequential (Dynamic.Traced (Population m)) a ->
  Population m a
rmsmcLocal k n t =
  Dynamic.marginal
    . sis (Dynamic.freeze . composeCopies t Dynamic.mhStep . Dynamic.hoistT resampleSystematic) k
    . Sequential.hoistFirst (Dynamic.hoistT (spawn n >>))

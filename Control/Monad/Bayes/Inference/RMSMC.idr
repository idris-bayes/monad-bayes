module Control.Monad.Bayes.Inference.RMSMC

import Control.Monad.Bayes.Interface
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Traced.Static
import Control.Monad.Bayes.Traced.Basic
import Control.Monad.Bayes.Traced.Dynamic

||| Resample-move Sequential Monte Carlo.
export
rmsmc :
  MonadSample m =>
  -- | number of timesteps
  (n_timesteps : Nat) ->
  -- | number of particles
  (n_particles : Nat) ->
  -- | number of Metropolis-Hastings transitions after each resampling
  (n_mhsteps : Nat) ->
  -- | model
  Sequential (Static.Traced (Population m)) a ->
  Population m a
rmsmc n_timesteps n_particles n_mhsteps =
  Static.marginal
    . sis (composeCopies n_mhsteps mhStep . hoistT resampleSystematic) n_timesteps
    . Sequential.hoistFirst (hoistT (spawn n_particles >>))

||| Resample-move Sequential Monte Carlo with a more efficient
||| tracing representation.
export
rmsmcBasic :
  MonadSample m =>
  -- | number of timesteps
  (n_timesteps : Nat) ->
  -- | number of particles
  (n_particles : Nat) ->
  -- | number of Metropolis-Hastings transitions after each resampling
  (n_mhsteps : Nat) ->
  -- | model
  Sequential (Basic.Traced (Population m)) a ->
  Population m a
rmsmcBasic n_timesteps n_particles n_mhsteps =
  Basic.marginal
    . sis (composeCopies n_mhsteps Basic.mhStep . Basic.hoistT resampleSystematic) n_timesteps
    . Sequential.hoistFirst (Basic.hoistT (spawn n_particles >>))

||| A variant of resample-move Sequential Monte Carlo
||| where only random variables since last resampling are considered
||| for rejuvenation.
export
rmsmcLocal :
  MonadSample m =>
  -- | number of timesteps
  (n_timesteps : Nat) ->
  -- | number of particles
  (n_particles : Nat) ->
  -- | number of Metropolis-Hastings transitions after each resampling
  (n_mhsteps : Nat) ->
  -- | model
  Sequential (Dynamic.Traced (Population m)) a ->
  Population m a
rmsmcLocal n_timesteps n_particles n_mhsteps =
  Dynamic.marginal
    . sis (Dynamic.freeze . composeCopies n_mhsteps Dynamic.mhStep . Dynamic.hoistT resampleSystematic) n_timesteps
    . Sequential.hoistFirst (Dynamic.hoistT (spawn n_particles >>))

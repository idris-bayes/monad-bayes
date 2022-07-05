module Control.Monad.Bayes.Inference.SMC

import Control.Monad.Bayes.Interface
import public Control.Monad.Bayes.Population
import public Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Weighted

||| Sequential importance resampling.
-- An SMC template that takes a custom resampler.
export
sir :
  (Monad m) =>
  -- | resampler
  (forall x. Population m x -> Population m x) ->
  -- | number of timesteps
  (n_timesteps : Nat) ->
  -- | population size
  (n_particles : Nat) ->
  -- | model
  Sequential (Population m) a ->
  Population m a
sir resampler n_timesteps n_particles = sis resampler n_timesteps . Sequential.hoistFirst (spawn n_particles >>)

||| Sequential Monte Carlo with multinomial resampling at each timestep.
-- Weights are not normalized.
export
smcMultinomial :
  MonadSample m =>
  -- | number of timesteps
  (n_timesteps : Nat) ->
  -- | population size
  (n_particles : Nat) ->
  -- | model
  Sequential (Population m) a ->
  Population m a
smcMultinomial = sir resampleMultinomial

||| Sequential Monte Carlo with systematic resampling at each timestep.
-- Weights are not normalized.
export
smcSystematic :
  MonadSample m =>
  -- | number of timesteps
  (n_timesteps : Nat) ->
  -- | population size
  (n_particles : Nat) ->
  -- | model
  Sequential (Population m) a ->
  Population m a
smcSystematic = sir resampleSystematic

||| Default synonym for smcSystematic
export
smc : MonadSample m => (n_timesteps : Nat) -> (n_particles : Nat) -> Sequential (Population m) a -> Population m a
smc = smcSystematic

||| Sequential Monte Carlo with multinomial resampling at each timestep.
-- Weights are normalized at each timestep and the total weight is pushed
-- as a score into the transformed monad.
export
smcMultinomialPush :
  MonadInfer m =>
  -- | number of timesteps
  (n_timesteps : Nat) ->
  -- | population size
  (n_particles : Nat) ->
  -- | model
  Sequential (Population m) a ->
  Population m a
smcMultinomialPush = sir (pushEvidence . resampleMultinomial)

||| Sequential Monte Carlo with systematic resampling at each timestep.
-- Weights are normalized at each timestep and the total weight is pushed
-- as a score into the transformed monad.
export
smcSystematicPush :
  MonadInfer m =>
  -- | number of timesteps
  (n_timesteps : Nat) ->
  -- | population size
  (n_particles : Nat) ->
  -- | model
  Sequential (Population m) a ->
  Population m a
smcSystematicPush = sir (pushEvidence . resampleSystematic)

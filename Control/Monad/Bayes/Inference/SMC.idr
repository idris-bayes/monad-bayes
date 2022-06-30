module Control.Monad.Bayes.Inference.SMC

import Control.Monad.Bayes.Interface
import public Control.Monad.Bayes.PopulationVect
import public Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Weighted

||| Sequential importance resampling.
-- An SMC template that takes a custom resampler.
export
sir : 
  (Monad m) =>
  -- | resampler
  (forall x. {k : Nat} -> Population k m x -> Population k m x) ->
  -- | number of timesteps
  (n_timesteps : Nat) ->
  -- | population size
  (n_particles : Nat) ->
  -- | model
  Sequential (Population n_particles m) a ->
  Population n_particles m a
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
  Sequential (Population n_particles m) a ->
  Population n_particles m a
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
  Sequential (Population n_particles m) a ->
  Population n_particles m a
smcSystematic = sir resampleSystematic

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
  Sequential (Population n_particles m) a ->
  Population n_particles m a
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
  Sequential (Population n_particles m) a ->
  Population n_particles m a
smcSystematicPush = sir (pushEvidence . resampleSystematic)

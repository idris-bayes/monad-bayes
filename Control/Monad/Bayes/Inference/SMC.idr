module Control.Monad.Bayes.Inference.SMC

import Control.Monad.Bayes.Interface
import public Control.Monad.Bayes.Population
import public Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Weighted

||| Sequential importance resampling.
-- An SMC template that takes a custom resampler.
export
sir :
  (isMonad : Monad m) =>
  -- | resampler
  (forall x. Population m x -> Population m x) ->
  -- | number of timesteps
  Nat ->
  -- | population size
  Nat ->
  -- | model
  Sequential (Population m) a ->
  Population m a
sir resampler k n = sis resampler k . Sequential.hoistFirst (spawn n >>)

||| Sequential Monte Carlo with multinomial resampling at each timestep.
-- Weights are not normalized.
export
smcMultinomial :
  MonadSample m =>
  -- | number of timesteps
  Nat ->
  -- | number of particles
  Nat ->
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
  Nat ->
  -- | number of particles
  Nat ->
  -- | model
  Sequential (Population m) a ->
  Population m a
smcSystematic = sir resampleSystematic

||| Sequential Monte Carlo with multinomial resampling at each timestep.
-- Weights are normalized at each timestep and the total weight is pushed
-- as a score into the transformed monad.
export
smcMultinomialPush :
  MonadInfer m =>
  -- | number of timesteps
  Nat ->
  -- | number of particles
  Nat ->
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
  Nat ->
  -- | number of particles
  Nat ->
  -- | model
  Sequential (Population m) a ->
  Population m a
smcSystematicPush = sir (pushEvidence . resampleSystematic)

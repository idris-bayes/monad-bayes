module Control.Monad.Bayes.Inference.PMMH

import Control.Monad.Bayes.Interface
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Traced.Static
import Control.Monad.Trans
import Numeric.Log

||| Particle Marginal Metropolis-Hastings sampling.
export
pmmh :
  MonadInfer m =>
  -- | number of Metropolis-Hastings steps
  (n_mhsteps : Nat) ->
  -- | number of time steps
  (n_timesteps : Nat) ->
  -- | number of particles
  (n_particles : Nat) ->
  -- | model parameters prior
  Traced m b ->
  -- | model
  (b -> Sequential (Population n_particles m) a) ->
  m (Vect (S n_mhsteps) (Vect n_particles (Log Double, a)))
pmmh n_mhsteps n_timesteps n_particles param model =
  mh n_mhsteps (param >>= runPopulation . pushEvidence . PopulationVect.hoist lift . smcSystematic n_timesteps n_particles . model)

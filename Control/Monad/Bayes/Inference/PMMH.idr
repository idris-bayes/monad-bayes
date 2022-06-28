module Control.Monad.Bayes.Inference.PMMH

import Control.Monad.Bayes.Interface
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Traced.Static
import Control.Monad.Trans
import Numeric.Log

||| Particle Marginal Metropolis-Hastings sampling.
pmmh :
  MonadInfer m =>
  -- | number of Metropolis-Hastings steps
  (t : Nat) ->
  -- | number of time steps
  Nat ->
  -- | number of particles
  Nat ->
  -- | model parameters prior
  Traced m b ->
  -- | model
  (b -> Sequential (Population m) a) ->
  m (Vect (S t) (List (Log Double, a)))
pmmh t k n param model =
  mh t (param >>= runPopulation . pushEvidence . Population.hoist lift . smcSystematic k n . model)

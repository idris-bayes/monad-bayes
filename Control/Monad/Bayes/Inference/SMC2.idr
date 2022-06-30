module Control.Monad.Bayes.Inference.SMC2

import Control.Monad.Bayes.Interface
import Control.Monad.Bayes.Inference.RMSMC
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Traced.Static
import Control.Monad.Bayes.Sequential
import Control.Monad.Trans
import Numeric.Log

||| Helper monad transformer for preprocessing the model for 'smc2'.
export
record SMC2 (n_particles : Nat) (m : Type -> Type) (a : Type) where
  constructor MkSMC2
  setup : Sequential (Traced (Population n_particles m)) a 

export
{n_particles : Nat} -> Monad m => Functor (SMC2 n_particles m) where
  map f (MkSMC2 mx) = MkSMC2 (map f mx) 

export
{n_particles : Nat} -> Monad m => Applicative (SMC2 n_particles m) where
  pure = MkSMC2 . pure 
  (MkSMC2 mf) <*> (MkSMC2 ma) = MkSMC2 (mf <*> ma)

export
{n_particles : Nat} -> Monad m => Monad (SMC2 n_particles m) where
  (MkSMC2 mx) >>= k = MkSMC2 ((assert_total (>>=)) mx (setup . k))

export
{n_particles : Nat} -> MonadTrans (SMC2 n_particles) where
  lift = MkSMC2 . lift . lift . lift

export
{n_particles : Nat} -> MonadSample m => MonadSample (SMC2 n_particles m) where
  random = lift random

export
{n_particles : Nat} -> Monad m => MonadCond (SMC2 n_particles m) where
  score w = MkSMC2 $ score w 

export
{n_particles : Nat} -> MonadSample m => MonadInfer (SMC2 n_particles m) where

||| Sequential Monte Carlo squared.
export
smc2 :
  MonadSample m =>
  -- | number of time steps
  (n_timesteps : Nat) ->
  -- | number of inner particles
  (n_inner_particles : Nat) ->
  -- | number of outer particles
  (n_outer_particles : Nat) ->
  -- | number of MH transitions
  (n_mhsteps : Nat) ->
  -- | model parameters
  Sequential (Traced (Population n_outer_particles m)) b ->
  -- | model
  (b -> Sequential (Population n_inner_particles (SMC2 n_outer_particles  m)) a) ->
  Population n_outer_particles m (Vect n_inner_particles (Log Double, a))
smc2 n_timesteps n_inner_particles n_outer_particles n_mhsteps param model =
  rmsmc n_timesteps n_outer_particles n_mhsteps 
    (param >>= setup . runPopulation . smcSystematicPush n_timesteps n_inner_particles . model)

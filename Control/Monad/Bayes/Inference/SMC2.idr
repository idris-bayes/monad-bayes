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
record SMC2 (m : Type -> Type) (a : Type) where
  constructor MkSMC2
  setup : Sequential (Traced (Population m)) a 

export
Monad m => Functor (SMC2 m) where
  map f (MkSMC2 mx) = MkSMC2 (map f mx) 

export
Monad m => Applicative (SMC2 m) where
  pure = MkSMC2 . pure 
  (MkSMC2 mf) <*> (MkSMC2 ma) = MkSMC2 (mf <*> ma)

export
Monad m => Monad (SMC2 m) where
  (MkSMC2 mx) >>= k = MkSMC2 ((assert_total (>>=)) mx (setup . k))

export
MonadTrans SMC2 where
  lift = MkSMC2 . lift . lift . lift

export
MonadSample m => MonadSample (SMC2 m) where
  random = lift random

export
Monad m => MonadCond (SMC2 m) where
  score w = MkSMC2 $ score w 

export
MonadSample m => MonadInfer (SMC2 m) where

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
  Sequential (Traced (Population m)) b ->
  -- | model
  (b -> Sequential (Population (SMC2 m)) a) ->
  Population m (List (Log Double, a))
smc2 n_timesteps n_inner_particles n_outer_particles n_mhsteps param model =
  rmsmc n_timesteps n_outer_particles n_mhsteps 
    (param >>= setup . runPopulation . smcSystematicPush n_timesteps n_inner_particles . model)

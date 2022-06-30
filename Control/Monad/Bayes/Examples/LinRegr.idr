module Control.Monad.Bayes.Examples.LinRegr

import Data.Maybe
import Data.List
import Control.Monad.Bayes.Interface
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Traced.Static
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.PMMH
import Statistics.Distribution.Normal
import Numeric.Log

record LinRegrParams where
  constructor MkLinRegrParams
  mean : Double   -- mean
  c : Double      -- intercept
  s : Double      -- standard deviation

Show LinRegrParams where
  show (MkLinRegrParams mv cv sv) = "(m : " ++ show mv ++ ")" -- ++ ", c : " ++ show cv ++ ", std : " ++ show sv ++ ")" 

linRegr_prior : MonadSample m => m LinRegrParams
linRegr_prior = do
  m'    <- normal 0 3
  c'    <- normal 0 5
  s'    <- uniform 1 3
  pure (MkLinRegrParams m' c' s')

linRegr_sim : MonadSample m => LinRegrParams -> List Double -> m (List Double)
linRegr_sim (MkLinRegrParams m0 c0 s0) xs  = do
  foldlM (\ys, x => do y <- normal (m0 * x + c0) s0
                       pure (y::ys)) [] xs

linRegr_inf : MonadInfer m => List (Double, Double) -> m LinRegrParams
linRegr_inf xys  = do
  m0    <- normal 0 3
  c0    <- normal 0 5
  s0    <- uniform 1 3
  _ <- sequence (map (\(x, y_obs) => let logprob : Log Double = toLogDomain (normal_pdf (m0 * x + c0) s0 y_obs )
                                     in  score logprob) xys)
  pure (MkLinRegrParams m0 c0 s0)

linRegr_noprior : MonadInfer m =>  List (Double, Double) -> LinRegrParams ->  m LinRegrParams
linRegr_noprior xys (MkLinRegrParams mv cv sv) = do
  _ <- sequence (map (\(x, y_obs) => let logprob : Log Double = toLogDomain (normal_pdf (mv * x + cv) sv y_obs )
                                     in  score logprob) xys)
  pure (MkLinRegrParams mv cv sv)

||| Simulate outputs `ys` from a linear regression model
simLinRegr : Nat -> IO (List Double)
simLinRegr n_datapoints = do
  ys <- sampleIO $ (linRegr_sim (MkLinRegrParams 3 0 1) (map cast [0 ..  n_datapoints]))
  print ys >> pure ys

||| Perform MH inference over linear regression model parameters, `m`, `c`, `s`
mhLinRegr : (n_samples : Nat) -> Nat -> IO (Vect (S n_samples) LinRegrParams)
mhLinRegr n_samples n_datapoints = do
  let linRegrData : Nat -> List (Double, Double)
      linRegrData n_datapoints = zip (map cast [0 ..  n_datapoints]) (map (*3) (map cast [0 ..  n_datapoints]))
  param_trace <- sampleIO $ prior $ mh n_samples (linRegr_inf (linRegrData n_datapoints))
  print param_trace >> pure param_trace

||| Perform SMC inference over linear regression model parameters, `m`, `c`, `s`
smcLinRegr : (n_timesteps : Nat) -> (n_samples : Nat) -> Nat -> IO (List (Log Double, LinRegrParams))
smcLinRegr n_timesteps n_samples n_datapoints = do
  let linRegrData : Nat -> List (Double, Double)
      linRegrData n_datapoints = zip (map cast [0 ..  n_datapoints]) (map (*3) (map cast [0 ..  n_datapoints]))
  param_trace <- sampleIO $ runPopulation $ smcSystematic n_timesteps n_samples  (linRegr_inf (linRegrData n_datapoints))
  print param_trace >> pure param_trace

||| Perform PMMH inference over linear regression model parameters, `m`, `c`, `s`
pmmhLinRegr : (n_mhsteps : Nat) -> (n_timesteps : Nat) -> (n_samples : Nat) -> Nat -> IO (Vect (S n_mhsteps) (List (Log Double, LinRegrParams)))
pmmhLinRegr n_mhsteps n_timesteps n_samples n_datapoints = do
  let linRegrData : Nat -> List (Double, Double)
      linRegrData n_datapoints = zip (map cast [0 ..  n_datapoints]) (map (*3) (map cast [0 ..  n_datapoints]))
  param_trace <- sampleIO $ prior $
                  pmmh n_mhsteps n_timesteps n_samples linRegr_prior (linRegr_noprior (linRegrData n_datapoints))
  print param_trace >> pure param_trace

{-
pack --with-ipkg monad-bayes.ipkg repl Control/Monad/Bayes/Examples/LinRegr.idr

:exec smcLinRegr 
-}


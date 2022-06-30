module Control.Monad.Bayes.Examples.LinRegr

import Data.Maybe
import Data.List
import Control.Monad.Bayes.Interface
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Traced.Static
import Control.Monad.Bayes.Inference.SMC
import Statistics.Distribution.Normal
import Numeric.Log

record LinRegrParams where
  constructor MkLinRegrParams
  mean : Double   -- mean
  c : Double      -- intercept
  s : Double      -- standard deviation

Show LinRegrParams where
  show (MkLinRegrParams mv cv sv) = "(m : " ++ show mv ++ ")" -- ++ ", c : " ++ show cv ++ ", std : " ++ show sv ++ ")" 

linRegr_prior : MonadSample m => Maybe Double -> Maybe Double -> Maybe Double -> m LinRegrParams
linRegr_prior m0 c0 s0 = do
  mean <- normal 0 3
  c    <- normal 0 5
  s    <- uniform 1 3
  let m' = fromMaybe mean m0
      c' = fromMaybe c c0
      s' = fromMaybe s s0
  pure (MkLinRegrParams m' c' s')

linRegr_sim : MonadSample m => Maybe Double -> Maybe Double -> Maybe Double -> List Double -> m (List Double)
linRegr_sim m0 c0 s0 xs  = do
  MkLinRegrParams mean c s <- linRegr_prior m0 c0 s0
  foldlM (\ys, x => do y <- normal (mean * x + c) s
                       pure (y::ys)) [] xs

linRegr_inf : MonadInfer m => Maybe Double -> Maybe Double -> Maybe Double -> List (Double, Double) -> m LinRegrParams
linRegr_inf m0 c0 s0 xys  = do
  MkLinRegrParams mean c s <- linRegr_prior m0 c0 s0
  
  _ <- sequence (map (\(x, y_obs) => let logprob : Log Double = Exp (log $ normal_pdf (mean * x + c) s y_obs )
                                     in  score logprob) xys)
  pure (MkLinRegrParams mean c s)

||| Simulate outputs `ys` from a linear regression model
simLinRegr : Nat -> IO (List Double)
simLinRegr n_datapoints = do
  ys <- sampleIO $ (linRegr_sim (Just 3) (Just 0) (Just 1) (map cast [0 ..  n_datapoints]))
  print ys >> pure ys

||| Perform MH inference over linear regression model parameters, `m`, `c`, `s`
mhLinRegr : (n_samples : Nat) -> Nat -> IO (Vect (S n_samples) LinRegrParams)
mhLinRegr n_samples n_datapoints = do
  let linRegrData : Nat -> List (Double, Double)
      linRegrData n_datapoints = zip (map cast [0 ..  n_datapoints]) (map (*3) (map cast [0 ..  n_datapoints]))
  param_trace <- sampleIO $ prior $ mh n_samples (linRegr_inf Nothing Nothing Nothing (linRegrData n_datapoints))
  print param_trace >> pure param_trace

||| Perform SMC inference over linear regression model parameters, `m`, `c`, `s`
smcLinRegr : (n_timesteps : Nat) -> (n_samples : Nat) -> Nat -> IO (List (Log Double, LinRegrParams))
smcLinRegr n_timesteps n_samples n_datapoints = do
  let linRegrData : Nat -> List (Double, Double)
      linRegrData n_datapoints = zip (map cast [0 ..  n_datapoints]) (map (*3) (map cast [0 ..  n_datapoints]))
  param_trace <- sampleIO $ runPopulation $ smcSystematic n_timesteps n_samples  (linRegr_inf Nothing Nothing Nothing (linRegrData n_datapoints))
  print param_trace >> pure param_trace

{-
pack --with-ipkg monad-bayes.ipkg repl Control/Monad/Bayes/Examples/LinRegr.idr

:exec smcLinRegr 
-}
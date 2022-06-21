module Control.Monad.Bayes.Examples.LinRegr

import Data.Maybe
import Control.Monad.Bayes.Interface

record LinRegrParams where
  constructor MkLinRegrParams
  m : Double
  c : Double
  σ : Double


linRegrPrior : MonadSample m => Maybe Double -> Maybe Double -> Maybe Double -> m LinRegrParams
linRegrPrior m0 c0 s0 = do
  m <- normal 0 3
  c <- normal 0 5
  s <- uniform 1 3
  let m' = fromMaybe m m0
      c' = fromMaybe c c0
      s' = fromMaybe s s0
  pure (MkLinRegrParams m' c' s')

simLinRegr : MonadSample m => Maybe Double -> Maybe Double -> Maybe Double -> List Double -> m (List Double)
simLinRegr m0 c0 s0 xs  = do
  MkLinRegrParams m c s <- linRegrPrior m0 c0 s0
  foldlM (\ys, x => do y <- normal (m * x + c) s
                       pure (y::ys)) [] xs

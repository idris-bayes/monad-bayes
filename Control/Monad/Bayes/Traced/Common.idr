module Control.Monad.Bayes.Traced.Common

import Numeric.Log
import Control.Monad.Writer
import Control.Monad.Bayes.Interface
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Free

||| Collection of random variables sampled during the program's execution.
public export
record Trace (a : Type) where
  constructor MkTrace 
  -- | Sequence of random variables sampled during the program's execution.
  variables : List Double 
  -- |
  output    : a
  -- | The probability of observing this particular sequence.
  density   : Log Double     

export
Functor Trace where
  map f t = { output $= f } t

export
Applicative Trace where
  pure x    = MkTrace { variables = [], output = x, density = 1 }
  tf <*> tx =
    MkTrace
      { variables = variables tf ++ variables tx,
        output    = output tf (output tx),
        density   = density tf * density tx
      }

export
Monad Trace where
  t >>= f = 
    let t' = f (t.output)
    in  {variables := variables t ++ variables t', density := density t * density t'} t' 

export
singleton : Double -> Trace Double
singleton u = MkTrace {variables = [u], output = u, density = 1}

export
scored : Log Double
      -> Trace ()
scored w = MkTrace {variables = [], output = (), density = w}

export
bind : Monad m => m (Trace a) -> (a -> m (Trace b)) -> m (Trace b)
bind dx f = do
  t1 <- dx
  t2 <- f (output t1)
  pure $ {variables := variables t1 ++ variables t2, density := density t1 * density t2} t2

||| A single Metropolis-corrected transition of single-site Trace MCMC.
export
mhTrans : MonadSample m => Weighted (FreeSampler m) a -> Trace a -> m (Trace a)
mhTrans mw t@(MkTrace {variables = us, output = x, density = p}) = do
  let n = length us -- need to ensure n >= 1
  us' <- do
    i <- discreteUniform n 
    u' <- random
    case splitAt i us of
      (xs, _ :: ys) => pure $ xs ++ (u' :: ys)
      _             => ?error_impossible
  ((q, b), vs) <- runWriterT $ runWeighted $ Weighted.hoist (writerT . withPartialRandomness us') mw
  let ratio : Double = (exp . ln) $ min 1 (q * (cast n) / (p * cast (length vs)))
  accept <- bernoulli ratio
  pure $ if accept then MkTrace vs b q else t


||| A variant of 'mhTrans' with an external sampling monad.
export
mhTrans' : MonadSample m => Weighted (FreeSampler Identity) a -> Trace a -> m (Trace a)
mhTrans' m = mhTrans (Weighted.hoist (Free.hoist (pure . runIdentity)) m)

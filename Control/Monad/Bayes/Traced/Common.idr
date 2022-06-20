module Control.Monad.Bayes.Traced.Common

import Numeric.Log

record Trace (a : Type) where
  constructor MkTrace 
  -- | Sequence of random variables sampled during the program's execution.
  variables : List Double 
  -- |
  output    : a
  -- | The probability of observing this particular sequence.
  density   : Log Double     

Functor Trace where
  map f t = { output $= f } t

Applicative Trace where
  pure x    = MkTrace { variables = [], output = x, density = 1 }
  tf <*> tx =
    MkTrace
      { variables = variables tf ++ variables tx,
        output    = output tf (output tx),
        density   = density tf * density tx
      }

Monad Trace where
  t >>= f = 
    let t' = f (t.output)
    in  {variables := variables t ++ variables t', density := density t * density t'} t' 

singleton : Double -> Trace Double
singleton u = MkTrace {variables = [u], output = u, density = 1}

scored : Log Double
      -> Trace ()
scored w = MkTrace {variables = [], output = (), density = w}

bind : Monad m => m (Trace a) -> (a -> m (Trace b)) -> m (Trace b)
bind dx f = do
  t1 <- dx
  t2 <- f (output t1)
  pure $ {variables := variables t1 ++ variables t2, density := density t1 * density t2} t2

-- mhTrans : MonadSample m => Weighted (FreeSampler m) a -> Trace a -> m (Trace a)
-- mhTrans m t = do
--   let MkTrace {variables = us, density = p} = t
--       n = length us
  -- us' <- do
  --   i <- discrete $ discreteUniformAB 0 (n - 1)
  --   u' <- random
  --   case splitAt i us of
  --     (xs, _ : ys) -> return $ xs ++ (u' : ys)
  --     _ -> error "impossible"
  -- ((b, q), vs) <- runWriterT $ runWeighted $ Weighted.hoist (WriterT . withPartialRandomness us') m
  -- let ratio = (exp . ln) $ min 1 (q * fromIntegral n / (p * fromIntegral (length vs)))
  -- accept <- bernoulli ratio
  -- return $ if accept then Trace vs b q else t
  -- ?hole
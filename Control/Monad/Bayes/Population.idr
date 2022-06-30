module Control.Monad.Bayes.Population

import public Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Interface
import Control.Monad.Bayes.Sampler
import Control.Monad.Trans
import Numeric.Log
import Data.List
import Debug.Trace

-- | pack --with-ipkg monad-bayes.ipkg repl Control/Monad/Bayes/Population.idr

||| List transformer
public export
record ListT (m : Type -> Type) (a : Type) where
  constructor MkListT 
  runListT : m (List a)

mapListT : (m (List a) -> n (List b)) -> ListT m a -> ListT n b
mapListT f m = MkListT $ f (runListT m)

export
Functor m => Functor (ListT m) where
  map f  = mapListT $ map $ map f 

export
Applicative m => Applicative (ListT m) where
  pure x  = MkListT (pure [x])
  f <*> v = MkListT $ (<*>) <$> runListT f <*> runListT v

export
Monad m => Monad (ListT m) where
  m >>= k  = MkListT $ do
    a <- runListT m
    b <- (sequence . map (runListT . k))  a
    pure (join b)

export
MonadTrans ListT where
  lift = MkListT . map List.singleton

export
MonadSample m => MonadSample (ListT m) where
  random = lift random
  bernoulli = lift . bernoulli
  categorical = lift . categorical

export
MonadCond m => MonadCond (ListT m) where
  score = lift . score

export
MonadInfer m => MonadInfer (ListT m) where

||| A collection of weighted samples, or particles.
public export
record Population (m : Type -> Type) (a : Type) where
  constructor MkPopulation
  runPopulation' : Weighted (ListT m) a 

export
Functor m => Functor (Population m) where
  map f (MkPopulation mx) = MkPopulation (map f mx) 

export
Monad m => Applicative (Population m) where
  pure = MkPopulation . pure 
  (MkPopulation mf) <*> (MkPopulation ma) = MkPopulation (mf <*> ma)

export
Monad m => Monad (Population m) where
  (MkPopulation mx) >>= k = MkPopulation (mx >>= (runPopulation' . k))

export
MonadTrans Population where
  lift = MkPopulation . lift . lift

export
MonadSample m => MonadSample (Population m) where
  random = lift random

export
Monad m => MonadCond (Population m) where
  score w = MkPopulation $ score w -- Call score from Weighted

export
MonadSample m => MonadInfer (Population m) where

||| Explicit representation of the weighted sample with weights in the log domain.
export
runPopulation : Population m a -> m (List (Log Double, a))
runPopulation (MkPopulation m) = (runListT . runWeighted) m

||| Explicit representation of the weighted sample.
export
explicitPopulation : Functor m => Population m a -> m (List (Double, a))
explicitPopulation = map (map (\(log_w, a) => (fromLogDomain log_w, a))) . runPopulation

||| Initialize 'Population' with a concrete weighted sample.
export
fromWeightedList : Monad m => m (List (Log Double, a)) -> Population m a
fromWeightedList = MkPopulation . withWeight . MkListT

||| Applies a transformation to the inner monad.
export
hoist :
  Monad m2 =>
  (forall x. m1 x -> m2 x) ->
  Population m1 a ->
  Population m2 a
hoist f = fromWeightedList . f . runPopulation

||| Increase the sample size by a given factor.
||| The weights are adjusted such that their sum is preserved. It is therefore 
||| safe to use 'spawn' in arbitrary places in the program without introducing bias.
export
spawn : (isMonad : Monad m) => Nat -> Population m ()
spawn n = fromWeightedList $ pure $ replicate n (toLogDomain (1.0 / cast n), ()) 

export
resampleGeneric :
  MonadSample m => 
  -- | resampler
  ({k : Nat} -> Vect k Double -> m (List (Fin k))) ->
  Population m a ->
  Population m a
resampleGeneric resampler pop = fromWeightedList $ do
  particles <- runPopulation pop
  let (log_ws, xs)  : (Vect (length particles) (Log Double), Vect (length particles) a) 
                    = unzip (fromList particles)
      z : Log Double = Numeric.Log.sum log_ws 

  if isPositive z  
    then do
            let weights    : Vect (length particles) Double   
                            = map (exp . ln . (/ z)) log_ws
            ancestors <- resampler weights
            let offsprings : List a
                            = map (\idx => index idx xs) ancestors
            pure $ map (z / (toLogDomain $ length particles), ) offsprings
    else
            pure particles

||| Systematic sampler.
export
systematic : {n : Nat} -> Double -> Vect n Double -> List (Fin n)
systematic {n = Z}   u Nil = Nil
systematic {n = S k} u ws =
  let     
          prob : Maybe (Fin (S k)) -> Double
          prob (Just idx) = index idx ws
          prob  Nothing   = index last ws

          inc : Double
          inc = 1 / cast (S k)

          f : Nat -> Double -> Nat -> Double -> List Nat -> List Nat
          f i v j q acc = 
            if i == S k then acc else
            if v < q
              then f (S i) (v + inc) j    q                             ((minus j 1) :: acc)
              else f  i    v        (S j) (q + prob (natToFin j (S k))) acc
          
          particle_idxs : List (Fin (S k))
          particle_idxs = map (\nat => fromMaybe FZ (natToFin nat (S k))) 
                              (f Z (u / cast (S k)) Z 0.0 [])

  in      particle_idxs
-- :exec print $ systematic 0.77 ([0.1, 0.25, 0.2, 0.05,0.15,0.05,0.1,0.05,0.05])
-- = [8, 6, 5, 4, 2, 2, 1, 1, 0]


||| Resample the population using the underlying monad and a systematic resampling scheme.
||| The total weight is preserved.
export
resampleSystematic :
  (MonadSample m) =>
  Population m a ->
  Population m a
resampleSystematic = resampleGeneric (\ws => (`systematic` ws) <$> random)

||| Multinomial sampler.  Sample from \(0, \ldots, n - 1\) \(n\)
||| times drawn at random according to the weights where \(n\) is the
||| length of vector of weights.
export
multinomial : MonadSample m => {n : Nat} -> Vect n Double -> m (List (Fin n))
multinomial ws = sequence $ replicate n (categorical ws)

||| Resample the population using the underlying monad and a multinomial resampling scheme.
||| The total weight is preserved.
export
resampleMultinomial :
  (MonadSample m) =>
  Population m a ->
  Population m a
resampleMultinomial = resampleGeneric multinomial

||| Separate the sum of weights into the 'Weighted' transformer.
||| Weights are normalized after this operation.
export
extractEvidence :
  Monad m =>
  Population m a ->
  Population (Weighted m) a
extractEvidence pop = fromWeightedList $ do
  particles <- lift $ runPopulation pop

  let (log_ws, xs) = unzip particles

  let z      : Log Double
             = Numeric.Log.sum log_ws

  let normalized_log_ws : List (Log Double) 
             = map (if isPositive z
                      then (/ z) 
                      else const (toLogDomain (1.0 / cast (length log_ws)))) log_ws
  score z

  pure (zip normalized_log_ws xs)

||| Push the evidence estimator as a score to the transformed monad.
||| Weights are normalized after this operation.
export
pushEvidence :
  MonadCond m =>
  Population m a ->
  Population m a
pushEvidence = hoist applyWeight . extractEvidence

||| A properly weighted single sample, that is one picked at random according
||| to the weights, with the sum of all weights.
export
proper :
  (MonadSample m) =>
  Population m a ->
  Weighted m a
proper pop = do
  particles <- runPopulation $ extractEvidence pop
  let (log_ws_vec, xs_vec) = unzip (fromList particles)
  idx <- the (Weighted m (Fin (length particles))) (logCategorical log_ws_vec)
  pure (index idx xs_vec)

||| Model evidence estimator, also known as pseudo-marginal likelihood.
export
evidence : (Monad m) => Population m a -> m (Log Double)
evidence = extractWeight . runPopulation . extractEvidence

||| Picks one point from the population and uses model evidence as a 'score' in the transformed monad.
||| This way a single sample can be selected from a population without introducing bias.
export
collapse :
  (MonadInfer m) =>
  Population m a ->
  m a
collapse = applyWeight . proper
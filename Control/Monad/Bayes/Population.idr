module Control.Monad.Bayes.Population

import public Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Interface
import Control.Monad.Trans
import Data.List
import Debug.Trace

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
explicitPopulation = map (map (\(log_p, a) => (exp (ln log_p), a))) . runPopulation

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
spawn n = fromWeightedList $ pure $ replicate n (Exp (1.0 / the (Double) (cast n)), ()) 

export
resampleGeneric :
  MonadSample m => 
  -- | resampler
  ({k : Nat} -> Vect k Double -> m (List (Fin k))) ->
  Population m a ->
  Population m a
resampleGeneric resampler pop = fromWeightedList $ do
  particles <- runPopulation pop
  Trace.trace ("resampleGeneric: number of particles is " ++ show (length particles)) (pure ())
  let (log_ps, xs) : (Vect (length particles) (Log Double), Vect (length particles) a) = unzip (fromList particles)
      n = length xs
      z = Numeric.Log.sum log_ps
  if z > 0
    then do
      let weights = (map (exp . ln . (/ z)) log_ps)
      
      ancestors <- resampler weights
      let offsprings = map (\idx => index idx xs) ancestors
          k = map (z / cast n, ) offsprings
      Trace.trace ("resampleGeneric: number of offsprings is " ++ show (length k)) (pure k)
    else
      (pure particles) 

||| Systematic sampler.
export
systematic : {n : Nat} -> Double -> Vect n Double -> List (Fin n)
systematic {n = Z}   u Nil = Nil
systematic {n = S k} u (p :: ps) =
  let     w = Trace.trace ("systematic resampler: initial particle weights are " ++ show (p :: ps)) 5
          prob : Maybe (Fin (S k)) -> Double
          prob (Just idx) = index idx (p :: ps)
          prob  Nothing   = index last (p :: ps)

          inc : Double
          inc = 1 / cast (S k)

          f : Nat -> Double -> Nat -> Double -> List Nat -> List Nat
          f i v j q acc = 
            if i == S k then acc else
            if v < q
              then f (1 + i) (v + inc) j q ((minus j 1) :: acc)
              else f  i v (1 + j) (q + prob (natToFin j (S k))) acc
          
          particle_idxs : List (Fin (S k))
          particle_idxs = map (\nat => fromMaybe FZ (natToFin nat (S k))) 
                              (f Z (u / cast (S k)) Z 0.0 [])
          -- h =  (the Nat 5)
  in      Trace.trace ("systematic resampler: resampled particle indexes are: " ++ show particle_idxs) particle_idxs

||| Resample the population using the underlying monad and a systematic resampling scheme.
||| The total weight is preserved.
export
resampleSystematic :
  (MonadSample m) =>
  Population m a ->
  Population m a
resampleSystematic = resampleGeneric (\ps => (`systematic` ps) <$> random)

||| Multinomial sampler.  Sample from \(0, \ldots, n - 1\) \(n\)
||| times drawn at random according to the weights where \(n\) is the
||| length of vector of weights.
export
multinomial : MonadSample m => {n : Nat} -> Vect n Double -> m (List (Fin n))
multinomial ps = Trace.trace ("ps are : " ++ show ps) (sequence $ replicate n (categorical ps))

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
  particles <- lift $ runPopulation pop -- List (Log Double, a)
  let (log_ps, xs) = unzip particles
      z = Numeric.Log.sum log_ps
      ws = map (if z > 0 then (/ z) else const (Exp $ 1.0 / cast (length log_ps))) log_ps
  score z
  pure $ zip ws xs 

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
  let (log_ps_vec, xs_vec) = unzip (fromList particles)
  idx <- the (Weighted m (Fin (length particles))) (logCategorical log_ps_vec)
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

||| Applies a random transformation to a population.
export
mapPopulation :
  (Monad m) =>
  (List (Log Double, a) -> m (List (Log Double, a))) ->
  Population m a ->
  Population m a
mapPopulation f m = fromWeightedList $ runPopulation m >>= f

||| Population average of a function, computed using unnormalized weights.
export
popAvg : (Monad m) => (a -> Double) -> Population m a -> m Double
popAvg f p = do
  xs <- explicitPopulation p
  let ys = map (\(w, x) => f x * w) xs
  pure (Prelude.sum ys)

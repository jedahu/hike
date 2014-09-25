{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Hike.Data.HikeF
( HikeF(..)
, Cached
, cached
, call
, maybeCall
, (*|*)
, Task
, task
, runTask
)
where

import Data.Dynamic
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Ord

import Control.Applicative
import Control.Concurrent.Spawn hiding (Result)
import Control.Lens
import Control.Monad.Free
import Control.Monad.State

import System.IO

import Hike.Data.Result


data HikeF m k a
    = HikeCache k Dynamic a
    | HikeRead k (Maybe Dynamic -> a)
    | HikeLift (m a)
    deriving (Functor)

hikeCache :: (Ord k, Functor m) => k -> Dynamic -> Free (HikeF m k) ()
hikeCache k v = liftF (HikeCache k v ())

hikeRead :: (Ord k, Functor m) => k -> Free (HikeF m k) (Maybe Dynamic)
hikeRead k = liftF (HikeRead k id)

hikeLift :: (Functor m) => m a -> Free (HikeF m k) a
hikeLift m = liftF (HikeLift m)

cache
    :: (Ord k, Typeable a, Functor m)
    => Free (HikeF m k) a -> k -> Free (HikeF m k) a
cache m k = do
    d <- hikeRead k
    maybe go return (d >>= fromDynamic)
  where
    go = do
        a <- m
        hikeCache k (toDyn a)
        return a

newtype Cached m k a
    = Cached (Free (HikeF m k) (Result a))
    deriving (Functor)

instance (Functor m) => Applicative (Cached m k) where
    pure = Cached . pure . pure

    (<*>) (Cached f) (Cached a) = Cached (liftA2 (<*>) f a)

(*|*) :: Cached IO k (a -> b) -> Cached IO k a -> Cached IO k b
(*|*) (Cached u) (Cached v) = Cached (go u v)
  where
    go (Pure f) (Pure a) = return (f <*> a)
    go (Free (HikeLift f)) (Free (HikeLift a)) =
        join $ hikeLift $ join
        $ (liftA2 . liftA2 . liftA2) (<*>) (return f) (spawn a)
    go f@(Free (HikeLift _)) a = go f (a >>= hikeLift . return)
    go f a@(Free (HikeLift _)) = go (f >>= hikeLift . return) a
    go f a = liftA2 (<*>) f a

cached
    :: (Typeable a, Ord k, Functor m)
    => Free (HikeF m k) (Result a) -> k -> Cached m k a
cached m k = Cached (cache m k)

call :: (Functor m) => (a -> m b) -> Cached m k a -> Cached m k b
call f (Cached a) = Cached (a >>= hikeLift . fmap diff . f . (^.value))

maybeCall
    :: (Functor m)
    => (a -> m b) -> Cached m k a -> Cached m k b -> Cached m k b
maybeCall f a b = Cached $ do
    Result s (a', b') <- ab
    case s of
      Same -> return (same b')
      Diff -> (hikeLift . fmap diff . f) a'
  where
    (Cached ab) = (,) <$> a <*> b

data Task m k a b
    = Task k (Cached m k a) (a -> m b) (Free (HikeF m k) (Result b))

task
    :: k
    -> (Cached m k a)
    -> (a -> m b)
    -> (Free (HikeF m k) (Result b))
    -> Task m k a b
task = Task

runTask :: (Ord k, Typeable b, Functor m) => Task m k a b -> Cached m k b
runTask (Task k a f b) = maybeCall f a (cached b k)

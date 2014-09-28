{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Hike.Data.HikeF
( Key
, HikeF(..)
, Cached
, cached
, call
, maybeCall
, (*|*)
, Task
, task
, cachedTask
)
where

import Data.Dynamic
import Data.Function
import Data.Functor
import Data.Maybe

import Control.Applicative
import Control.Concurrent.Spawn hiding (Result)
import Control.Lens
import Control.Monad.Free
import Control.Monad.State

import System.IO

import Hike.Data.Result
import Hike.Control.Monad.Unique


type Key = UID

data HikeF m a
    = HikeCache Key Dynamic a
    | HikeRead Key (Maybe Dynamic -> a)
    | HikeLift (m a)
    deriving (Functor)

hikeCache :: (Functor m) => Key -> Dynamic -> Free (HikeF m) ()
hikeCache k v = liftF (HikeCache k v ())

hikeRead :: (Functor m) => Key -> Free (HikeF m) (Maybe Dynamic)
hikeRead k = liftF (HikeRead k id)

hikeLift :: (Functor m) => m a -> Free (HikeF m) a
hikeLift m = liftF (HikeLift m)

cache
    :: (Typeable a, Functor m)
    => Free (HikeF m) a -> Key -> Free (HikeF m) a
cache m k = do
    d <- hikeRead k
    maybe go return (d >>= fromDynamic)
  where
    go = do
        a <- m
        hikeCache k (toDyn a)
        return a

newtype Cached m a
    = Cached (Free (HikeF m) (Result a))
    deriving (Functor)

instance (Functor m) => Applicative (Cached m) where
    pure = Cached . pure . pure

    (<*>) (Cached f) (Cached a) = Cached (liftA2 (<*>) f a)

(*|*) :: Cached IO (a -> b) -> Cached IO a -> Cached IO b
(*|*) (Cached u) (Cached v) = Cached (go u v)
  where
    go (Pure f) (Pure a) = return (f <*> a)
    go (Free (HikeLift f)) (Free (HikeLift a)) =
        join $ hikeLift $ join
        $ (liftA2 . liftA2 . liftA2) (<*>) (return f) (spawn a)
    go f@(Free (HikeLift _)) a = go f (a >>= hikeLift . return)
    go f a@(Free (HikeLift _)) = go (f >>= hikeLift . return) a
    go f a = liftA2 (<*>) f a

mkCached
    :: (Typeable a, Functor m)
    => Free (HikeF m) (Result a) -> Key -> Cached m a
mkCached m = Cached . cache m

cached
    :: (Typeable a, Functor m)
    => Free (HikeF m) (Result a) -> Unique (Cached m a)
cached m = mkCached m <$> fresh

call :: (Functor m) => (a -> m b) -> Cached m a -> Cached m b
call f (Cached a) = Cached (a >>= hikeLift . fmap diff . f . (^.value))

maybeCall
    :: (Functor m)
    => (a -> m b) -> Cached m a -> Cached m b -> Cached m b
maybeCall f a b = Cached $ do
    Result s (a', b') <- ab
    case s of
      Same -> return (same b')
      Diff -> (hikeLift . fmap diff . f) a'
  where
    (Cached ab) = (,) <$> a <*> b

data Task m a b
    = Task (Cached m a) (a -> m b) (Free (HikeF m) (Result b))

task
    :: (Cached m a)
    -> (a -> m b)
    -> (Free (HikeF m) (Result b))
    -> Task m a b
task = Task

cachedTask :: (Typeable b, Functor m) => Task m a b -> Unique (Cached m b)
cachedTask (Task a f b) = maybeCall f a <$> cached b

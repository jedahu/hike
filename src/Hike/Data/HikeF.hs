{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
module Hike.Data.HikeF
( HikeF(..)
)
where

import Data.Dynamic
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Ord

import Control.Monad.Free
import Control.Monad.State


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
    = Cached (Free (HikeF m k) a)
    deriving (Functor)

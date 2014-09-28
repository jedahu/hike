{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Hike.Control.Monad.Cache
( CacheT
, Cache
, MonadCache
, cached
, evalCacheT
, evalCache
)
where

import Data.Dynamic
import Data.Function
import Data.Map as Map
import Data.Maybe
import Data.Ord

import Control.Lens
import Control.Monad
import Control.Monad.State


newtype CacheT k m a
    = CacheT (StateT (Map k Dynamic) m a)
    deriving (Functor, Monad, MonadTrans, MonadIO)

type Cache k = CacheT k Identity

class (Ord k, Monad m, Monad n) => MonadCache m k n where
    cached :: (Typeable a) => n a -> k -> m a

instance (Ord k, Monad m) => MonadCache (CacheT k m) k m where
    cached m k = CacheT $ do
        d <- use (at k)
        maybe go return (d >>= fromDynamic)
      where
        go = do
            v <- lift m
            at k ?= (toDyn v)
            return v

evalCacheT :: (Monad m) => CacheT k m a -> m a
evalCacheT (CacheT s) = evalStateT s Map.empty

evalCache :: Cache k a -> a
evalCache = runIdentity . evalCacheT

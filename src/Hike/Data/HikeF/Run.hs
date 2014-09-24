{-# LANGUAGE NoImplicitPrelude #-}
module Hike.Data.HikeF.Run
( runM
, evalM
)
where

import Data.Dynamic
import Data.Function
import Data.Functor
import Data.Map as Map
import Data.Ord

import Control.Lens
import Control.Monad
import Control.Monad.Free
import Control.Monad.State

import Hike.Data.HikeF


type CacheT k = StateT (Map k Dynamic)

runF :: (Ord k, Functor m, Monad m) => HikeF m k (CacheT k m a) -> CacheT k m a
runF (HikeCache k v a) = at k ?= v >> a
runF (HikeRead k f)    = use (at k) >>= f
runF (HikeLift m)      = lift m >>= id

runM :: (Ord k, Functor m, Monad m) => Free (HikeF m k) a -> CacheT k m a
runM = iterM runF

evalM :: (Ord k, Functor m, Monad m) => Free (HikeF m k) a -> m a
evalM m = evalStateT (runM m) Map.empty

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module Hike.Data.HikeF.Run
( runM
, evalM
, runL
, execL
)
where

import Data.Dynamic
import Data.Function
import Data.Functor
import Data.Map as Map
import Data.Ord
import Data.DList as DList
import Data.String
import Data.Tuple

import Text.Show

import Control.Lens
import Control.Monad
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Writer

import Hike.Data.HikeF


type CacheT k = StateT (Map k Dynamic)

stepM :: (Ord k, Functor m, Monad m) => HikeF m k (CacheT k m a) -> CacheT k m a
stepM (HikeCache k v a) = at k ?= v >> a
stepM (HikeRead k f)    = use (at k) >>= f
stepM (HikeLift m)      = lift m >>= id

runM :: (Ord k, Functor m, Monad m) => Free (HikeF m k) a -> CacheT k m a
runM = iterM stepM

evalM :: (Ord k, Functor m, Monad m) => Free (HikeF m k) a -> m a
evalM m = evalStateT (runM m) Map.empty

type SeqT k = CacheT k (Writer (DList (String, String)))

tell' :: (Show a, MonadWriter (DList (String, String)) m) => String -> a -> m ()
tell' s a = tell (DList.singleton (s, show a))

stepL
    :: (Ord k, Show k, Functor m, Monad m)
    => HikeF m k (SeqT k a) -> SeqT k ()
stepL (HikeCache k v a) = tell' "cache" k >> at k ?= v  >> a  >> return ()
stepL (HikeRead k f)    = tell' "read"  k >> use (at k) >>= f >> return ()
stepL (HikeLift _)      = tell' "act"   ""                    >> return ()

runL :: (Ord k, Show k, Functor m, Monad m) => Free (HikeF m k) a -> SeqT k ()
runL = iterM stepL . (>> return ())

execL
    :: (Ord k, Show k, Functor m, Monad m)
    => Free (HikeF m k) a -> DList (String, String)
execL m = execWriter (evalStateT (runL m) Map.empty)


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
import Data.DList as DList
import Data.String

import Text.Show

import Control.Lens
import Control.Monad
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Writer

import Hike.Data.HikeF


type CacheT = StateT (Map Key Dynamic)

stepM :: (Functor m, Monad m) => HikeF m (CacheT m a) -> CacheT m a
stepM (HikeCache k v a) = at k ?= v  >>  a
stepM (HikeRead k f)    = use (at k) >>= f
stepM (HikeLift m)      = lift m     >>= id

runM :: (Functor m, Monad m) => Free (HikeF m) a -> CacheT m a
runM = iterM stepM

evalM :: (Functor m, Monad m) => Free (HikeF m) a -> m a
evalM m = evalStateT (runM m) Map.empty

type SeqT = CacheT (Writer (DList (String, String)))

tell' :: (Show a, MonadWriter (DList (String, String)) m) => String -> a -> m ()
tell' s a = tell (DList.singleton (s, show a))

stepL :: (Functor m, Monad m) => HikeF m (SeqT a) -> SeqT ()
stepL (HikeCache k v a) = tell' "cache" k >> at k ?= v  >> a  >> return ()
stepL (HikeRead k f)    = tell' "read"  k >> use (at k) >>= f >> return ()
stepL (HikeLift _)      = tell' "act"   ""                    >> return ()

runL :: (Functor m, Monad m) => Free (HikeF m) a -> SeqT ()
runL = iterM stepL . (>> return ())

execL :: (Functor m, Monad m) => Free (HikeF m) a -> DList (String, String)
execL m = execWriter (evalStateT (runL m) Map.empty)

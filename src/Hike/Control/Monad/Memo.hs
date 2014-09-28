{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Hike.Control.Monad.Memo
( memo
)
where

import Data.Dynamic
import Data.Functor

import Hike.Control.Monad.Unique
import Hike.Control.Monad.Cache


memo
    :: (Typeable a, Functor m, MonadCache m UID n, MonadUnique m)
    => n a -> m (m a)
memo m = cached m <$> fresh


-- newtype MemoT m a
--     = MemoT (CacheT UID (UniqueT m) a)
--     deriving (Functor, Monad, MonadIO)
-- 
-- instance MonadTrans MemoT where
--     lift = MemoT . lift . lift
-- 
-- type Memo = MemoT Identity
-- 
-- class (Monad m) => MonadMemo m where
--     memo :: (Typeable a) => m a -> m a
-- 
-- instance (Monad m) => MonadMemo (MemoT m) where
--     memo (MemoT m) = MemoT (lift fresh >>= cached m)
-- 
-- memoM :: (Typeable a, Monad m) => m a -> MemoT m a
-- memoM m = memo (lift m)
-- 
-- evalMemoT :: (Monad m) => MemoT m a -> m a
-- evalMemoT (MemoT c) = evalUniqueT (evalCacheT c)
-- 
-- evalMemo :: Memo a -> a
-- evalMemo = runIdentity . evalMemoT

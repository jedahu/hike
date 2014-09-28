{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hike.Control.Monad.Unique
( UID
, UniqueT
, Unique
, MonadUnique
, fresh
, evalUniqueT
, evalUnique
, uidInteger
)
where

import Prelude (Integer, succ)

import Data.Eq
import Data.Function
import Data.Ord

import Text.Show

import Control.Monad
import Control.Monad.State
import Control.Monad.Identity


newtype UID
    = UID Integer
    deriving (Eq, Ord, Show)

uidInteger :: UID -> Integer
uidInteger (UID x) = x

newtype UniqueT m a
    = UniqueT (StateT Integer m a)
    deriving (Functor, Monad, MonadTrans, MonadIO)

type Unique = UniqueT Identity

class (Monad m) => MonadUnique m where
    fresh :: m UID

instance (Monad m) => MonadUnique (UniqueT m) where
    fresh = UniqueT $ do
        n <- get
        put (succ n)
        return (UID n)

evalUniqueT :: (Monad m) => UniqueT m a -> m a
evalUniqueT (UniqueT s) = evalStateT s 0

evalUnique :: Unique a -> a
evalUnique = runIdentity . evalUniqueT

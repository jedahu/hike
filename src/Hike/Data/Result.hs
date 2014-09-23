{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Hike.Data.Result where

import Data.Typeable
import Data.Eq
import Data.Functor
import Data.Monoid

import Text.Show

import Control.Applicative
import Control.Monad


data ResultState
    = Same
    | Diff
    deriving (Show, Eq, Typeable)

instance Monoid ResultState where
    mempty = Same

    mappend Same Same = Same
    mappend _    _    = Diff


data Result a
    = Result ResultState a
    deriving (Show, Eq, Functor, Typeable)

resultState
    :: (Functor f) => (ResultState -> f ResultState) -> Result a -> f (Result a)
resultState f (Result s a) = fmap (\s' -> Result s' a) (f s)

resultValue :: (Functor f) => (a -> f b) -> Result a -> f (Result b)
resultValue f (Result s a) = fmap (Result s) (f a)

resultFold :: (a -> b) -> (a -> b) -> Result a -> b
resultFold f _ (Result Same a) = f a
resultFold _ g (Result Diff a) = g a

instance Applicative Result where
    pure = Result Same

    Result fs f <*> Result as a = Result (fs <> as) (f a)

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

same :: a -> Result a
same = Result Same

diff :: a -> Result a
diff = Result Diff

state
    :: (Functor f)
    => (ResultState -> f ResultState) -> Result a -> f (Result a)
state f (Result s a) = fmap (\s' -> Result s' a) (f s)

value :: (Functor f) => (a -> f b) -> Result a -> f (Result b)
value f (Result s a) = fmap (Result s) (f a)

instance Applicative Result where
    pure = Result Same

    Result fs f <*> Result as a = Result (fs <> as) (f a)

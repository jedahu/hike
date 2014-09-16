{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Hike.Control.HikeT
where

import Data.Functor
import Data.Text
import qualified Data.Text as Text
import Data.Map
import qualified Data.Map as Map
import Data.DList
import qualified Data.DList as DList
import Data.Dynamic
import Data.Typeable
import Control.Applicative
import Control.Concurrent.Spawn
import Control.Lens hiding (liftAct)
import Control.Monad.Error
import Control.Monad.RWS
import Control.Monad.Trans.Free
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
-- import Control.Monad.Trans.Either

data DState a
    = Same a
    | Diff a
    deriving (Functor, Typeable)

dstate :: (a -> b) -> (a -> b) -> DState a -> b
dstate same _ (Same a) = same a
dstate _ diff (Diff a) = diff a

unDState :: DState a -> a
unDState (Same a) = a
unDState (Diff a) = a

instance Applicative DState where
    pure = Same

    (Same f) <*> (Same x) = Same (f x)
    (Same f) <*> (Diff x) = Diff (f x)
    (Diff f) <*> (Same x) = Diff (f x)
    (Diff f) <*> (Diff x) = Diff (f x)

data CacheF a
    = CacheWrite Text Dynamic a
    | CacheRead Text (Maybe Dynamic -> a)
    deriving (Functor)

type Cache = Map Text Dynamic

type Act r e m = ReaderT r (ErrorT e m)
type Require r s e m = FreeT CacheF (ReaderT r (StateT (s, Cache) (ErrorT e m)))

newtype Condition r s e m a =
    Condition (Require r s e m (DState a))
    deriving (Functor)

newtype Check r s e m a =
    Check (Require r s e m (DState a))
    deriving (Functor)

data Task r s e m a =
    Task (Require r s e m (DState a)) (Check r s e m a) Text
    deriving (Functor)

cacheWrite :: (Monad m) => Text -> Dynamic -> FreeT CacheF m ()
cacheWrite k v = liftF (CacheWrite k v ())

cacheRead :: (Monad m) => Text -> FreeT CacheF m (Maybe Dynamic)
cacheRead k = liftF (CacheRead k id)

liftAct :: (Error e, Functor m, Monad m) => Act r e m a -> Require r s e m a
liftAct = addStateF . FreeT . fmap Pure
  where
    addState e = StateT $ \s -> fmap (, s) e
    addStateF  = hoistFreeT (mapReaderT addState)

cache :: (Typeable a, Monad m) => FreeT CacheF m a -> Text -> FreeT CacheF m a
cache m k = do
    mdyn <- cacheRead k
    maybe run return (mdyn >>= fromDynamic)
  where
    run = do
        x <- m
        cacheWrite k (toDyn x)
        return x

precond :: (Typeable a, Error e, Monad m)
        => Require r s e m (DState a) -> Text -> Condition r s e m a
precond m k = Condition (cache m k)

postcond :: (Typeable a, Error e, Monad m)
         => Require r s e m (DState a) -> Text -> Check r s e m a
postcond m k = Check (cache m k)

actRequire :: (Error e, Functor m, Monad m)
           => Require r s e m (DState a)
           -> (a -> Act r e m b)
           -> Require r s e m (DState b)
           -> Require r s e m (DState b)
actRequire before m after = do
    pre  <- before
    post <- after
    let both = (,) <$> pre <*> post
    case (,) <$> pre <*> post of
      Same (_, b) -> return (Same b)
      Diff (a, _) -> Diff <$> liftAct (m a)

taskRequire :: (Typeable a, Error e, Monad m)
            => Task r s e m a -> Require r s e m (DState a)
taskRequire (Task m (Check post) k) = cache cond k
  where
    cond = post >>= dstate (return . Same) (const m)

task :: (Typeable a, Error e, Functor m, Monad m)
     => Condition r s e m a
     -> (a -> Act r e m b)
     -> Check r s e m b
     -> Text
     -> Task r s e m b
task (Condition dep) m check@(Check post) k =
    Task (actRequire dep m post) check k

runM :: (MonadState (s, Cache) m) => FreeT CacheF m a -> m a
runM fa = runFreeT fa >>= go
  where
    go (Pure a)                  =
        return a
    go (Free (CacheWrite k v a)) =
        modify (\(x, y) -> (x, Map.insert k v y)) >> runM a
    go (Free (CacheRead k f))    =
        gets (Map.lookup k . snd) >>= runM . f

puts :: (MonadIO m, Show a) => FreeT CacheF m a -> m a
puts fa = runFreeT fa >>= go
  where
    go (Pure a) = do
        liftIO (putStrLn ("done " ++ show a))
        return a
    go (Free (CacheWrite k v a)) = do
        liftIO (putStrLn ("write " ++ unpack k))
        puts a
    go (Free (CacheRead k f)) = do
        liftIO (putStrLn ("read " ++ unpack k))
        puts (f Nothing)

record :: (MonadWriter (DList (Text, Text)) m, Show a)
       => FreeT CacheF m a -> m a
record fa = runFreeT fa >>= go
  where
    go (Pure a) = do
        tell (DList.singleton ("done", pack (show a)))
        return a
    go (Free (CacheWrite k v a)) = do
        tell (DList.singleton ("write", k))
        record a
    go (Free (CacheRead k f)) = do
        tell (DList.singleton ("read", k))
        record (f Nothing)

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Hike.Control.HikeT
where

import Data.Functor
import Data.Text as Text
import Data.Map
import qualified Data.Map as Map
import Data.DList
import qualified Data.DList as DList
import Data.Dynamic
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

data CacheF m a
    = CacheWrite Text Dynamic a
    | CacheRead Text (Maybe Dynamic -> a)
    | CacheAct (m a)
    deriving (Functor)

type Cache = Map Text Dynamic

type Act r e m = ReaderT r (ErrorT e m)
type RunM r s e m = ReaderT r (StateT (s, Cache) (ErrorT e m))
type Require r s e m = FreeT (CacheF m) (RunM r s e Identity)

newtype Condition r s e m a =
    Condition (FreeT (CacheF m) (RunM r s e m) (DState a))
    deriving (Functor)

newtype Check r s e m a =
    Check (FreeT (CacheF m) (RunM r s e m) (DState a))
    deriving (Functor)

data Task r s e m a =
    Task (FreeT (CacheF m) (RunM r s e m) (DState a)) (Check r s e m a) Text
    deriving (Functor)

instance (Error e, Applicative m, Monad m)
         => Applicative (Condition r s e m) where
    pure = Condition . pure . pure

    (Condition f) <*> (Condition a) = Condition (liftA2 (<*>) f a)

papply :: IO (a -> b) -> IO a -> IO b
papply f = join . liftA2 (<*>) (return f) . spawn

plift2 :: (a -> b -> c) -> IO a -> IO b -> IO c
plift2 f a b = return f `papply` a `papply` b

(*|*) :: (Error e)
      => Condition r s e IO (a -> b)
      -> Condition r s e IO a
      -> Condition r s e IO b
(*|*) = undefined

cacheWrite :: (Error e, Functor m, Monad m, Monad n)
           => Text -> Dynamic -> FreeT (CacheF m) (RunM r s e n) ()
cacheWrite k v = liftF (CacheWrite k v ())

cacheRead :: (Error e, Functor m, Monad m, Monad n)
          => Text -> FreeT (CacheF m) (RunM r s e n) (Maybe Dynamic)
cacheRead k = liftF (CacheRead k id)

cacheAct :: (Error e, Functor m, Monad m, Monad n)
         => m a -> FreeT (CacheF m) (RunM r s e n) a
cacheAct io = liftF (CacheAct io)

liftActF :: (Error e, Functor m, Monad m)
         => Act r e m a -> Act r e (Require r s e m) a
liftActF = mapReaderT (mapErrorT cacheAct)

liftAct :: (Error e, Functor m, Monad m) => Act r e m a -> Require r s e m a
liftAct fa = ask
    >>= runErrorT . runReaderT (liftActF fa)
    >>= either throwError return

cache :: (Error e, Typeable a, Functor m, Monad m)
      => FreeT (CacheF m) (RunM r s e m) (DState a)
      -> Text
      -> FreeT (CacheF m) (RunM r s e m) (DState a)
cache m k = do
    mdyn <- cacheRead k
    maybe run return (mdyn >>= fromDynamic)
  where
    run = do
        x <- m
        cacheWrite k (toDyn x)
        return x

precond :: (Error e, Typeable a, Functor m, Monad m)
        => Require r s e m (DState a) -> Text -> Condition r s e m a
precond m k = Condition (cache (idToM' m) k)

postcond :: (Error e, Typeable a, Functor m, Monad m)
         => Require r s e m (DState a) -> Text -> Check r s e m a
postcond m k = Check (cache (idToM' m) k)

actRequire :: (Error e, Functor m, Monad m)
           => FreeT (CacheF m) (RunM r s e m) (DState a)
           -> (a -> Act r e m b)
           -> FreeT (CacheF m) (RunM r s e m) (DState b)
           -> FreeT (CacheF m) (RunM r s e m) (DState b)
actRequire before m after = do
    pre  <- before
    post <- after
    case (,) <$> pre <*> post of
      Same (_, b) -> return (Same b)
      Diff (a, _) -> Diff <$> idToM' (liftAct (m a))

taskRequire :: (Error e, Typeable a, Functor m, Monad m)
            => Task r s e m a -> FreeT (CacheF m) (RunM r s e m) (DState a)
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

idToM :: (Error e, Monad m) => RunM r s e Identity a -> RunM r s e m a
idToM = mapReaderT (mapStateT (mapErrorT idM))
  where
    idM (Identity a) = return a

idToM' :: (Error e, Functor m, Monad m, Monad n)
      => Require r s e m a -> FreeT (CacheF m) (RunM r s e n) a
idToM' = hoistFreeT idToM

runM :: (Error e, Functor m, Monad m) => Require r s e m a -> RunM r s e m a
runM fa = runM' (idToM' fa)
  where
    runM' fa' = runFreeT fa' >>= go
    go (Pure a)                  =
        return a
    go (Free (CacheWrite k v a)) =
        modify (\(x, y) -> (x, Map.insert k v y)) >> runM' a
    go (Free (CacheRead k f))    =
        gets (Map.lookup k . snd) >>= runM' . f
    go (Free (CacheAct m))       =
        (lift . lift . lift) m >>= runM'

record :: (Error e, Show a, Functor m, Monad m)
       => Require r s e m a -> RunM r s e (Writer (DList (Text, Text))) ()
record fa = record' (idToM' fa >> return ())
  where
    record' fa' = runFreeT fa' >>= go
    go (Pure a) = do
        tell (DList.singleton ("done", pack (show a)))
        return a
    go (Free (CacheWrite k _ a)) = do
        tell (DList.singleton ("write", k))
        record' a
    go (Free (CacheRead k f)) = do
        tell (DList.singleton ("read", k))
        record' (f Nothing)
    go (Free (CacheAct _)) = do
        tell (DList.singleton ("act", ""))
        record' (return ())

-- puts :: (Error e, MonadIO m, Show a) => Require r s e m a -> RunM r s e IO a
-- puts fa = runFreeT fa >>= go
--   where
--     go (Pure a) = do
--         liftIO (putStrLn ("done " ++ show a))
--         return a
--     go (Free (CacheWrite k v a)) = do
--         liftIO (putStrLn ("write " ++ unpack k))
--         puts a
--     go (Free (CacheRead k f)) = do
--         liftIO (putStrLn ("read " ++ unpack k))
--         puts (f Nothing)
-- 

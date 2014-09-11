{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
module Hike.Control.Hike
where

import Data.Functor
import Data.Text
import Data.Map
import qualified Data.Map as Map
import Control.Applicative
import Control.Concurrent.Spawn
import Control.Lens
import Control.Monad.Error
import Control.Monad.RWS
import Control.Monad.Free
import Control.Monad.Writer
-- import Control.Monad.Trans.Either
import Data.Validation
import Data.Semigroup

data DState a
    = Same a
    | Diff a
    deriving (Functor)

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

data HActionF r e a
    = HAsk (r -> a)
    | HAct (IO (Either e a))
    | HErr e
    deriving (Functor)

data HCondF s a
    = HGet (s -> a)
    | HPut s a
    deriving (Functor)

data HikeF r s e a = HA (HActionF r e a) | HC (HCondF s a) deriving (Functor)

type HAction r e = Free (HActionF r e)

type HCond s = Free (HCondF s)

type Hike r s e = Free (HikeF r s e)

flipE :: Either a b -> Either b a
flipE = either Right Left

fmapL :: (a -> b) -> Either a x -> Either b x
fmapL f = either (Left . f) Right

mapHActionErrorF :: (e -> e') -> HActionF r e (HAction r e' a) -> HAction r e' a
mapHActionErrorF f (HAsk g)  = Free (HAsk g)
mapHActionErrorF f (HAct io) = Free (HAct $ fmap (fmapL f) io)
mapHActionErrorF f (HErr e)  = Free (HErr $ f e)

mapHActionError :: (e -> e') -> HAction r e a -> HAction r e' a
mapHActionError = iterM . mapHActionErrorF

mapHikeErrorF :: (e -> e') -> HikeF r s e (Hike r s e' a) -> Hike r s e' a
mapHikeErrorF f (HA (HAsk g))  = Free (HA (HAsk g))
mapHikeErrorF f (HA (HAct io)) = Free (HA (HAct $ fmap (fmapL f) io))
mapHikeErrorF f (HA (HErr e))  = Free (HA (HErr $ f e))
mapHikeErrorF _ (HC x)         = Free (HC x)

mapHikeError :: (e -> e') -> Hike r s e a -> Hike r s e' a
mapHikeError = iterM . mapHikeErrorF

mapHActionReaderF :: (r' -> r)
                  -> HActionF r e (HAction r' e a)
                  -> HAction r' e a
mapHActionReaderF f (HAsk g)  = Free (HAsk $ g . f)
mapHActionReaderF _ (HAct io) = Free (HAct io)
mapHActionReaderF _ (HErr e)  = Free (HErr e)

mapHikeReaderF :: (r' -> r)
               -> HikeF r s e (Hike r' s e a)
               -> Hike r' s e a
mapHikeReaderF f (HA (HAsk g))  = Free (HA (HAsk $ g . f))
mapHikeReaderF _ (HA (HAct io)) = Free (HA (HAct io))
mapHikeReaderF _ (HA (HErr e))  = Free (HA (HErr e))
mapHikeReaderF _ (HC x)         = Free (HC x)

liftHA :: HAction r e a -> Hike r s e a
liftHA = iterM (Free . HA)

instance MonadReader r (HAction r e) where
    ask = liftF (HAsk id)
    local = iterM . mapHActionReaderF

instance MonadReader r (Hike r s e) where
    ask = liftF (HA (HAsk id))
    local = iterM . mapHikeReaderF

instance MonadError e (HAction r e) where
    throwError e = liftF (HErr e)

    catchError (Free (HErr e)) f = f e
    catchError m               _ = m

instance MonadError e (Hike r s e) where
    throwError e = liftF (HA (HErr e))

    catchError (Free (HA (HErr e))) f = f e
    catchError m                    _ = m

instance MonadIO (HAction r e) where
    liftIO io = liftF (HAct $ Right <$> io)

instance MonadIO (Hike r s e) where
    liftIO io = liftF (HA (HAct $ Right <$> io))

instance MonadState s (Hike r s e) where
    get = liftF (HC (HGet id))
    put s = liftF (HC (HPut s ()))

type Run r s e = RWST r [String] s (ErrorT e IO)

runHActionF :: (Error e) => HActionF r e (Run r s e a) -> Run r s e a
runHActionF (HAsk f)  = ask >>= f
runHActionF (HAct io) = liftIO io >>= either throwError id
runHActionF (HErr e)  = throwError e

runHAction :: (Error e) => HAction r e a -> Run r s e a
runHAction = iterM runHActionF

runHCondF :: (Error e) => HCondF s (Run r s e a) -> Run r s e a
runHCondF (HGet f)   = get >>= f
runHCondF (HPut s a) = put s >> a

runHCond :: (Error e) => HCond s a -> Run r s e a
runHCond = iterM runHCondF

runHikeF :: (Error e) => HikeF r s e (Run r s e a) -> Run r s e a
runHikeF (HA x) = runHActionF x
runHikeF (HC x) = runHCondF x

runHike :: (Error e) => Hike r s e a -> Run r s e a
runHike = iterM runHikeF

type Print = Writer [String]

printHActionF :: (Error e) => r -> HActionF r e (Print ()) -> (Print ())
printHActionF r (HAsk f) = tell ["ask"] >> return r >>= f
printHActionF r (HAct a) = tell ["act"] >>= return
printHActionF r (HErr e) = tell ["error"]

printHAction :: (Error e) => r -> HAction r e () -> Print ()
printHAction = iterM . printHActionF


type Condition  r s e a = Hike r s e (DState a)
type TaskAction r s e a = Hike r s e (DState a)

data Task r s e a = Task (TaskAction r s e a) (Condition r s e a)
                    deriving (Functor)

data Dependency r s e a
    = PreCondition (Condition r s e a)
    | PreTask (Task r s e a)
    deriving (Functor)

callTask :: Task r s e a -> Condition r s e a
callTask (Task m post) =
    post >>= dstate (return . Same) (const m)

maybeTask :: Dependency r s e a
          -> (a -> HAction r e b)
          -> Condition r s e b
          -> Condition r s e b
maybeTask before m after = do
    pre  <- condition before
    post <- after
    let both = (,) <$> pre <*> post
    case (,) <$> pre <*> post of
      Same (_, b) -> return (Same b)
      Diff (a, _) -> Diff <$> liftHA (m a)

condition :: Dependency r s e a -> Condition r s e a
condition (PreCondition x) = x
condition (PreTask td)     = callTask td

-- instance Applicative (Dependency r s e) where
--     pure = PreCondition . pure . pure
--
--     f <*> a = PreCondition (liftA2 (<*>) (condition f) (condition a))

instance Applicative (Dependency r s e) where
    pure = PreCondition . pure . pure

    f <*> a = PreCondition cond
      where
        cond = case (condition f, condition a) of
            (Free (HA (HAct iof)), Free (HA (HAct ioa))) ->
                Free (HA (HAct (join $ liftA5 (<*>) (return iof) (spawn ioa))))
            (cf, ca) -> liftA2 (<*>) cf ca
        liftA5 = liftA2 . liftA2 . liftA2 . liftA2

task :: Dependency r s e a
     -> (a -> HAction r e b)
     -> Condition r s e b
     -> Task r s e b
task dep m post = Task (maybeTask dep m post) post


data BasicConfig = BasicConfig
    { _sourceFiles :: [String]
    }

compiler :: Dependency BasicConfig s Text String
compiler = PreCondition $ return (Same "cc")

sources :: Dependency BasicConfig s Text [String]
sources = PreCondition $ asks _sourceFiles >>= return . Diff

objects :: Condition BasicConfig s Text [String]
objects = asks _sourceFiles >>= return . Same . fmap (++ ".o")

buildAction :: (String, [String]) -> HAction r Text [String]
buildAction (cc, files) = do
    liftIO $ putStrLn "building..."
    return $ (++ ".o") <$> files

build :: Task BasicConfig s Text [String]
build = task ((,) <$> compiler <*> sources) buildAction objects

-- h1 :: HAction Int String Int
-- h1 = hErr "asdf"
-- 
-- h2 :: HAction Int String Int
-- h2 = return 99
-- 
-- h3 :: String -> HAction Int String Int
-- h3 = return . (+ 1) . Prelude.length
-- 
-- c3 :: HAction Int String (DState Int)
-- c3 = return (Same 6)
-- 
-- c3' :: HAction Int String (DState Int)
-- c3' = return (Diff 9)
-- 
-- p3 :: Dependency Int String String
-- p3 = PreCondition $ return (Same "asdf")
-- 
-- p3' :: Dependency Int String String
-- p3' = PreCondition $ return (Diff "asdfghi")
-- 
-- t3 :: Task Int String Int
-- t3 = task p3 h3 c3
-- 
-- t3' :: Task Int String Int
-- t3' = task p3' h3 c3'

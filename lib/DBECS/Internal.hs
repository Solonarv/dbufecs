{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module DBECS.Internal where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce

import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State
import Data.IntMap (IntMap)
import Data.Vinyl (Rec)
import Data.Vinyl.Functor (Compose(..), (:.))
import Data.Vinyl.Lens (type (∈))
import qualified Data.Vinyl as Vinyl
import qualified Data.Vinyl.TypeLevel as Vinyl

data World m xs = World (Ref m (Rec StoreOf xs)) (Ref m (Rec StoreOf xs))

newtype TickCounter = TickCounter Int
  deriving newtype (Eq, Ord, Show)
newtype EntityCounter = EntityCounter Int
  deriving newtype (Eq, Ord, Show)
newtype Entity = Entity Int
  deriving newtype (Eq, Ord)

class Component a where
  type Store a

newtype StoreOf a = StoreOf (Store a)

class Init m s where
  gInit :: m s

class Get s a | s -> a where
  gGet :: s -> Int -> Maybe a
  gHas :: s -> Int -> Bool

class Set s a | s -> a where
  gSet :: s -> Int -> a -> s

newStores :: forall xs m. (MonadRef m, Vinyl.RPureConstrained (StoreInit m) xs) => m (Rec StoreOf xs)
newStores = Vinyl.rtraverse getCompose $ Vinyl.rpureConstrained @(StoreInit m) (Compose (fmap StoreOf gInit))

class Init m (Store a) => StoreInit m a 
instance Init m (Store a) => StoreInit m a

newWorld :: (MonadRef m, Vinyl.RPureConstrained (StoreInit m) xs) => m (World m xs)
newWorld = liftA2 World (newRef =<< newStores) (newRef =<< newStores)

newtype Step xs m a = Step (Rec StoreOf xs -> Rec StoreOf xs -> m (a, Rec StoreOf xs))
  deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)
    via (ReaderT (Rec StoreOf xs) (StateT (Rec StoreOf xs) m))

deriving via (ReaderT (Rec StoreOf xs) (StateT (Rec StoreOf xs) m) a)
  instance Semigroup (ReaderT (Rec StoreOf xs) (StateT (Rec StoreOf xs) m) a)
  => Semigroup (Step xs m a)
deriving via (ReaderT (Rec StoreOf xs) (StateT (Rec StoreOf xs) m) a)
  instance Monoid (ReaderT (Rec StoreOf xs) (StateT (Rec StoreOf xs) m) a)
  => Monoid (Step xs m a)

instance MonadTrans (Step xs) where
  lift :: forall m a. Monad m => m a -> Step xs m a
  lift = coerce (lift @(ReaderT (Rec StoreOf xs)) . lift @(StateT(Rec StoreOf xs)) @m @a)

runStep :: (MonadRef m, TickCounter ∈ xs) => World m xs -> Step xs m a -> m a
runStep (World oldRef curRef) (Step k) = do
  old <- readRef oldRef
  cur <- readRef curRef
  (a, new) <- k old cur
  writeRef oldRef cur
  writeRef curRef (oldRef & Vinyl.rlens @TickCounter %~ coerce ((+1) @Int)
  pure a

class Monad m => MonadStep xs m where
  getC :: forall x a. (Get (Store x) a, x ∈ xs) => Entity -> Step xs m (Maybe a)

instance Monad m => MonadStep xs (Step xs m) where
  getC (Entity i) = Step \_ cur -> let store = rget cur in pure (cur, gGet store i)
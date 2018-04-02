{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses  #-}
module Language.Tiger.Gensym
  ( Fresh                       -- do NOT export genzero and gennext
  , Gensym(..)
  , GensymT(..)
  , runGensymT
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.State.Class

class Fresh s where
  genzero :: s
  gennext :: s -> s

instance Fresh Int where
  genzero = 0
  gennext = (+1)

newtype GensymT s m a = GensymT { unGensymT :: StateT s m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance Monad m => MonadState s (GensymT s m) where
  state = GensymT . state

class Gensym m where
  gensym :: (MonadState s m, Fresh s) => m s

instance Gensym (GensymT s m) where
  gensym = do
    s <- get
    modify gennext
    return s

runGensymT :: (Fresh s, Monad m) => GensymT s m a -> m a
runGensymT = flip evalStateT genzero . unGensymT

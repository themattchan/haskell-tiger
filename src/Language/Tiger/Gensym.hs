module Language.Tiger.Gensym
  ( Fresh                       -- do NOT export genzero and gennext
  , Gensym(..)
  , GensymT(..)
  , runGensymT
  ) where

import Control.Monad.Trans.State

class Fresh s where
  genzero :: s
  gennext :: s -> s

instance Fresh Int where
  genzero = 0
  gennext = (+1)

newtype GensymT s m a = GensymT { unGensymT :: StateT s m a }
  deriving (Functor, Applicatve, Monad, MonadTrans, MonadIO)

class Monad m => Gensym m where
  gensym :: Fresh s => m s

instance Monad m => Gensym (GensymT s m) where
  gensym = do
    s <- get
    modify gennext
    return s

runGensymT :: (Fresh s, Monad m) => GensymT s m a -> m a
runGensymT = flip evalStateT genzero . unGensymT

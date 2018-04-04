module Language.Tiger.Utils where

import Control.Monad.State.Lazy
import Control.Monad

mapAccumLM :: (Monad m) => (a -> s -> m (b, s)) -> s -> [a] -> m ([b], s)
mapAccumLM f a xs = runStateT (mapM (StateT . f) xs) a

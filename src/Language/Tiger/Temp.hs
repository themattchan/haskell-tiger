{-# LANGUAGE OverloadedStrings #-}
module Language.Tiger.Temp where

import Language.Tiger.Gensym
import qualified Language.Tiger.Types as Types (Symbol(..))


type Label = Types.Symbol

newLabel :: Gensym m => m Label
newLabel = do
  s <- gensym
  return $ "L" ++ show s

namedLabel :: String -> Label
namedLabel = fromString

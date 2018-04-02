{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Language.Tiger.Temp where

import Language.Tiger.Gensym
import qualified Language.Tiger.Types as Types (Symbol(..))
import Data.String (IsString(..))

type Temp = Int

newTemp :: Gensym Temp m => m Temp
newTemp = gensym

makeString :: Temp -> String
makeString i = "t" ++ show i

type Label = Types.Symbol

newLabel :: (Gensym Temp m) => m Label
newLabel = do
  s <- gensym
  return $ fromString $ "L" ++ show s

namedLabel :: String -> Label
namedLabel = fromString

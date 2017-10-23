-- | Chapter 6: Activation Records

module Language.Tiger.Translate where

import qualified Language.Tiger.Temp as Temp
import qualified Language.Tiger.Access as Access

data Level = Outermost
           | Level { parent :: Level
                   , name :: Temp.Label
                   , formals :: [Bool]
                   }

data Access = Access Level Frame.Access

allocLocal :: Level -> Bool -> Access
allocLocal

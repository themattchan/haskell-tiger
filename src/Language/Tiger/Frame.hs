-- | Chapter 6: Activation Records
module Language.Tiger.Frame where

import qualified Language.Tiger.Temp as Temp

data Access = InFrame Int
            | InReg Temp.Temp
              deriving (Eq, Show)

class Frame frame where
  newFrame :: Temp.Label -> [Bool] -> frame
  name :: frame -> Temp.Label
  formals :: frame -> [Access]
  allocLocal :: frame -> Bool -> Access

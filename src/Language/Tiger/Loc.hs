module Language.Tiger.Loc where

data Loc a = Loc { locPosn :: AlexPosn, locData :: a }

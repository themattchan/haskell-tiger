{-# LANGUAGE TemplateHaskell #-}

-- | Chapter 6: Activation Records
module Language.Tiger.Frame where
import Control.Lens.TH (makeLenses)

import qualified Language.Tiger.IR as IR
import qualified Language.Tiger.Temp as Temp

data Access = InFrame Int       -- offset from frame pointer
            | InReg Temp.Temp
              deriving (Eq, Show)

data Frame frame = Frame
  { _newFrame   :: Temp.Label -> [Bool] -> frame
  , _name       :: frame -> Temp.Label
  , _formals    :: frame -> [Access]
  , _allocLocal :: frame -> Bool -> Access
  , _fp         :: Temp.Temp
  , _wordSize   :: Int
  , _exp        :: Access -> IR.Exp -> IR.Exp
  }

makeLenses ''Frame

{-# LANGUAGE OverloadedStrings #-}

-- | Types in the Tiger language.
module Language.Tiger.Types
  ( Ty(..)
  ) where

import Language.Tiger.AST (Symbol)

type Unique = Int

data Ty
  = NilTy
  | UnitTy
  | IntTy
  | StringTy
  -- ^ base types
  | NameTy Symbol Symbol -- (Maybe Symbol)
  | RecordTy [(Symbol, Ty)] Unique
  | Array Ty Unique
  -- ^ user defined types
  deriving (Show, Eq)

data TyC = NameTyC | RecordTyC | ArrayTyC
  deriving (Eq)

instance Show TyC where
  show NameTyC = "Name"
  show RecordTyC = "Record"
  show ArrayTyC = "Array"

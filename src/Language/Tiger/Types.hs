{-# LANGUAGE OverloadedStrings #-}

-- | Types in the Tiger language.
-- For convenience, this reuses the type machinery from Language.Tiger.AST.
-- Translation from AST.Ty to Types.Ty is done in Semant.transTy
module Language.Tiger.Types
  ( Ty(..)
--  , AST.Ty(..)
  , AST.Field(..)
  , AST.Unique
  , AST.Symbol(..)
  ) where

import qualified Language.Tiger.AST as AST

data Ty
  = StringTy
  | UnitTy
  | IntTy
  | NilTy
  | TyTy (AST.Ty ())
  deriving Eq

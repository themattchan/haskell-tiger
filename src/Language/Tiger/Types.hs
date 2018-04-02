{-# LANGUAGE OverloadedStrings #-}

-- | Types in the Tiger language.
-- For convenience, this reuses the type machinery from Language.Tiger.AST.
module Language.Tiger.Types
  ( Ty(..)
--  , AST.Ty(..)
  , AST.Field(..)
  , AST.Unique
  , AST.Symbol(..)
  , coerce
  ) where
import Control.Monad
import qualified Language.Tiger.AST as AST

data Ty
  = StringTy
  | UnitTy
  | IntTy
  | NilTy
  | TyTy (AST.Ty ())
  deriving Eq

coerce :: AST.Ty a -> Ty
coerce (AST.NameTy s _ _)
  | s == "nil"    = NilTy
  | s == "unit"   = UnitTy
  | s == "string" = StringTy
  | s == "int"    = IntTy
coerce x = TyTy (void x)

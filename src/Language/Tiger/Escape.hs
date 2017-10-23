-- | Chapter 6: Activation Records
-- Escape analysis

module Language.Tiger.Escape where

import qualified Language.Tiger.Symtab as Symtab
import Language.Tiger.Types


type Depth = Int
type Escape = Bool

findEscape :: Exp a -> Exp (Bool, a)
findEscape = goE Symtab.empty 0 where

  goV :: Symtab (Depth, Escape) -> Depth -> Var a -> Var (Bool, a)
  goV env d v = case v of
    SimpleVar s a
      ->


  goE :: Symtab (Depth, Escape) -> Depth -> Exp a -> Exp (Bool, a)
  goE env d exp = case exp of

    Var v _ = goV env d v

-- | Symbol table / Typing environments
module Language.Tiger.Symtab
  ( Symtab
  , Symbol(..)
  , empty
  , insert
  , lookup
  ) where

import qualified Data.HashMap.Strict as HM

import Language.Tiger.Types (Type(..))

type Symtab a = HM.HashMap Symbol (Type a)

data Symbol = Sym String Int
  deriving (Show, Eq)

empty :: Symtab a
empty = HM.empty

insert :: Symbol -> a -> Symtab a -> Symtab a
insert = HM.insert

lookup :: Symbol -> Symtab a -> Maybe a
lookup = HM.lookup

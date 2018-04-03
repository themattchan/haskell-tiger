-- | Symbol table / Typing environments
module Language.Tiger.Symtab
  ( Symtab
  , Symbol(..)
  , empty
  , insert
  , lookup
  , fromList
  ) where

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as HM

import Language.Tiger.Types (Ty(..), Symbol(..))

type Symtab a = HM.HashMap Symbol a
-- newtype Symbol = Sym B.ByteString
--   deriving (Show, Eq)

empty :: Symtab a
empty = HM.empty

insert :: Symbol -> a -> Symtab a -> Symtab a
insert = HM.insert

inserts :: Foldable t => t (Symbol, a) -> Symtab a -> Symtab a
inserts = flip (foldr (uncurry insert))

lookup :: Symbol -> Symtab a -> Maybe a
lookup = HM.lookup

fromList :: [(Symbol, a)] -> Symtab a
fromList = HM.fromList

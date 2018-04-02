{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Tiger.Types where

import Control.Lens.TH (makeLenses, makePrisms)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Hashable
import Control.Comonad
import GHC.Generics
import Data.String

newtype Symbol = Sym { symbol :: B.ByteString }
  deriving (Show, Eq, Generic, Hashable)

instance IsString Symbol where
  fromString = Sym . B.pack

data Var a
  = SimpleVar Symbol                  a
  | FieldVar (Var a) Symbol           a
  | SubscriptVar (Var a) (Exp a)      a
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Op
  = Plus | Minus | Times | Divide
  | Eq | Neq
  | Lt | Le | Gt | Ge
  | And | Or
  deriving (Show, Eq, Enum, Bounded)

data Exp a
  -- lvalue
  = Var (Var a)                                     a
  | Nil                                             a
  | Int Int                                         a
  | String B.ByteString                             a
  -- app
  | Call Symbol [Exp a]                             a
  -- cmpexp, mathexp, boolexp
  | Op (Exp a) Op (Exp a)                           a
  -- record
  | Record [(Symbol, Exp a, a)] Symbol              a
  -- sequence
  | Seq [Exp a]                                     a
  -- assign
  | Assign (Var a) (Exp a)                          a
  -- control
  | If (Exp a) (Exp a) (Maybe (Exp a))              a
  | While (Exp a) (Exp a)                           a
  | For Symbol (Exp a) (Exp a) (Exp a)              a
  | Break                                           a
  | Let [Decl a] (Exp a)                            a
  -- array
  | Array Symbol (Exp a) (Exp a)                    a
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Decl a
  = FunctionDecl [Function a] a
  | VarDecl { name   :: Symbol
            , escape :: Bool
            , typ    :: Maybe (Symbol,a)
            , init   :: Exp a
            , meta   :: a
            }
  | TypeDecl [(Symbol, Ty a, a)] a
  deriving (Show, Eq, Functor, Foldable, Traversable)

type Unique = Int

data Ty a
  = NameTy Symbol (Maybe (Ty a))  a
  | RecordTy [Field a] Unique     a
  | ArrayTy Symbol Unique         a
  | NilTy                         a
  | IntTy                         a
  | StringTy                      a
  | UnitTy                        a
  deriving (Show, Functor, Foldable, Traversable)

instance Eq (Ty a) where
  UnitTy _ == UnitTy _ = True
  StringTy _ == StringTy _ = True
  IntTy _ == IntTy _  = True
  NilTy _  == NilTy _ = True
  ArrayTy _ u _ == ArrayTy _ w _ = u == w
  RecordTy _ u _ == RecordTy _ w _ = u == w
  NameTy s t _ == NameTy s' t' _ = s == s' && t == t'
  _ == _ = False

data Field a
  = Field
    { fieldName   :: Symbol
--    , fieldEscape :: Bool
    , fieldType   :: Symbol
    , fieldAnnot  :: a
    } deriving (Show, Eq, Functor, Foldable, Traversable)

data Function a
  = Function
    { funName   :: Symbol
    , funParams :: [Field a]
    , funResult :: Maybe (Symbol,a)
    , funBody   :: Exp a
    , funAnnot  :: a
    } deriving (Show, Eq, Functor, Foldable, Traversable)



-- * Lenses

makeLenses ''Symbol
makePrisms ''Var
makePrisms ''Op
makePrisms ''Exp
makePrisms ''Decl
makePrisms ''Ty
makeLenses ''Field
makeLenses ''Function

-- * Annotations

class Functor f => Ann f where
  ann :: f a -> a

instance Ann Var where
  ann (SimpleVar _      a) = a
  ann (FieldVar _ _     a) = a
  ann (SubscriptVar _ _ a) = a

instance Ann Exp where
  ann (Var _       a) = a
  ann (Nil         a) = a
  ann (Int _       a) = a
  ann (String _    a) = a
  ann (Call _ _    a) = a
  ann (Op _ _ _    a) = a
  ann (Record _ _  a) = a
  ann (Seq _       a) = a
  ann (Assign _ _  a) = a
  ann (If _ _ _    a) = a
  ann (While _ _   a) = a
  ann (For _ _ _ _ a) = a
  ann (Break       a) = a
  ann (Let _ _     a) = a
  ann (Array _ _ _ a) = a

instance Ann Decl where
  ann (FunctionDecl _ a) = a
  ann (VarDecl {meta=a}) = a
  ann (TypeDecl _ a)     = a

instance Ann Ty where
  ann (NameTy _ _   a) = a
  ann (RecordTy _ _ a) = a
  ann (ArrayTy _ _  a) = a
  ann (NilTy        a) = a
  ann (IntTy        a) = a
  ann (StringTy     a) = a
  ann (UnitTy       a) = a

instance Ann Field where ann = fieldAnnot

instance Ann Function where ann = funAnnot

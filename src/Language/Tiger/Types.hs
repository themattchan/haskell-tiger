{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}

module Language.Tiger.Types where

import Control.Lens.TH (makeLenses, makePrisms)
import qualified Data.ByteString.Strict as B
import Data.Hashable
import Control.Comonad

newtype Symbol = Sym { symbol :: B.ByteString }
  deriving (Show, Eq, Hashable)

makeLenses ''Symbol

instance IsString Symbol where
  fromString = Sym . B.pack

data Var a
  = SimpleVar Symbol                  a
  | FieldVar (Var a) Symbol           a
  | SubscriptVar (Var a) (Exp a)      a
  deriving (Show, Functor, Foldable, Traversable)

makePrisms ''Var

data Op
  = Plus | Minus | Times | Divide
  | Eq | Neq
  | Lt | Le | Gt | Ge
  deriving (Show, Eq, Enum, Bounded)

makePrisms ''Op

data Exp a
  = Var (Var a)                                     a
  | Nil                                             a
  | Int Int                                         a
  | String String                                   a
  | Call Symbol [Exp a]                             a
  | Op (Exp a) Op (Exp a)                           a
  | Record [(Symbol, Exp a, a)] Symbol              a
  | Seq [(Exp a, a)]                                a
  | Assign (Var a) (Exp a)                          a
  | If (Exp a) (Exp a) (Maybe (Exp a))              a
  | While (Exp a) (Exp a)                           a
  | For Symbol (Exp a) (Exp a) (Exp a)              a
  | Break                                           a
  | Let [Decl a] (Exp a)                            a
  | Array Symbol (Exp a) (Exp a)                    a
  deriving (Show, Eq, Functor, Foldable, Traversable, Comonad)

makePrisms ''Exp

data Decl a
  = FunctionDecl [Function a]
  | VarDecl { name   :: Symbol
            , escape :: Bool
            , typ    :: Maybe (Symbol,a)
            , init   :: Exp
            , meta   :: a
            }
  | TypeDecl [(Symbol, Ty, m)]
  deriving (Show, Eq, Functor, Foldable, Traversable)

makePrisms ''Decl

type Unique = Int

data Ty a
  = NameTy Symbol (Maybe (Ty a))  a
  | RecordTy [Field a] Unique
  | ArrayTy Symbol Unique         a
  | NilTy
  | IntTy
  | StringTy
  | UnitTy
  deriving (Show, Functor, Foldable, Traversable)

instance Eq (Ty a) where
  UnitTy == UnitTy = True
  StringTy == StringTy = True
  IntTy == IntTy = True
  NilTy == NilTy = True
  ArrayTy _ u _ == ArrayTy _ w _ = u == w
  RecordTy _ u == RecordTy _ w = u == w
  NameTy s t == NameTy s' t' = s == s' && t == t'
  _ == _ = False

makePrisms ''Ty

data Field a
  = Field
    { fieldName   :: Symbol
--    , fieldEscape :: Bool
    , fieldType   :: Symbol
    , fieldAnnot  :: a
    } deriving (Show, Functor, Foldable, Traversable)

makeLenses ''Field

data Function a
  = Function
    { funName   :: Symbol
    , funParams :: [Field a]
    , funResult :: Maybe (Symbol,a)
    , funBody   :: Exp a
    , funAnnot  :: a
    } deriving (Show, Functor, Foldable, Traversable)

makeLenses ''Function

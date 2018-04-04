{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Tiger.AST where

import Control.Lens.TH (makeLenses, makePrisms)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Hashable
import Control.Comonad
import GHC.Generics
import Data.String
import Language.Tiger.Loc

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
  deriving (Show, Eq, Enum, Bounded)

data Exp a
  = Var (Var a)                                     a -- ^ lvalue
  | Nil                                             a -- ^ exp (leaf)
  | Int Int                                         a -- ^ exp (leaf)
  | String B.ByteString                             a -- ^ exp (leaf)
  | Call Symbol [Exp a]                             a -- ^ app
  | Op (Exp a) Op (Exp a)                           a -- ^ cmpexp, mathexp, boolexp
  | Record [(Symbol, Exp a, a)] Symbol              a -- ^ record
  | Seq [Exp a]                                     a -- ^ sequence
  | Assign (Var a) (Exp a)                          a -- ^ assign
  | If (Exp a) (Exp a) (Maybe (Exp a))              a -- ^ control
  | While (Exp a) (Exp a)                           a -- ^ control
  | For Symbol (Exp a) (Exp a) (Exp a)              a -- ^ control
  | Break                                           a -- ^ control
  | Let [Decl a] (Exp a)                            a -- ^ control
  | Array Symbol (Exp a) (Exp a)                    a -- ^ array
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Decl a
  = FunctionDecl [Function a] a
  | VarDecl { name   :: Symbol
            , escape :: Bool
            , typ    :: Maybe (Symbol, a)
            , init   :: Exp a
            , meta   :: a
            }
  | TypeDecl [(Symbol, Ty a, a)] a
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Ty a
  = NameTy Symbol  a
  | RecordTy [Field a] a
  | ArrayTy Symbol a
  deriving (Show, Eq, Functor, Foldable, Traversable)

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

class Ann f where
  ann :: f a -> a
instance Comonad f => Ann f where
  ann = extract

instance Comonad Var where
  extract (SimpleVar _      a) = a
  extract (FieldVar _ _     a) = a
  extract (SubscriptVar _ _ a) = a

instance Comonad Exp where
  extract (Var _       a) = a
  extract (Nil         a) = a
  extract (Int _       a) = a
  extract (String _    a) = a
  extract (Call _ _    a) = a
  extract (Op _ _ _    a) = a
  extract (Record _ _  a) = a
  extract (Seq _       a) = a
  extract (Assign _ _  a) = a
  extract (If _ _ _    a) = a
  extract (While _ _   a) = a
  extract (For _ _ _ _ a) = a
  extract (Break       a) = a
  extract (Let _ _     a) = a
  extract (Array _ _ _ a) = a

instance Comonad Decl where
  extract (FunctionDecl _ a) = a
  extract (VarDecl {meta=a}) = a
  extract (TypeDecl _ a)     = a

instance Comonad Ty where
  extract (NameTy _ a)   = a
  extract (RecordTy _ a) = a
  extract (ArrayTy _ a)  = a

instance Comonad Field where
  extract = fieldAnnot

instance Comonad Function where
  extract = funAnnot

class HasSrcSpan a where
  sp :: a -> SrcSpan

instance HasSrcSpan SrcSpan where
  sp = id

instance HasSrcSpan SrcPosn where
  sp = posnToSpan

instance HasSrcSpan (Loc a) where
  sp = posnToSpan . locPosn

-- instance (Ann f, HasSrcSpan a) => HasSrcSpan (f a) where
--   sp = sp . ann
instance (Comonad f) => HasSrcSpan (f SrcSpan) where
  sp = sp . extract

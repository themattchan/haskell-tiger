{-# LANGUAGE RecordWildCards, TupleSections #-}

-- | Type checking and translation to intermediate code
module Language.Tiger.Semant where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import Language.Tiger.Types
import qualified Language.Tiger.Symtab as Symtab

-- data TransState = TransSt
--   { typeEnv ::
--   , varEnv  :: Symtab Env
--   , uniqSource :: Int
--   }

data TransError = TypeMismatch | TypeUndefined

-- type TransMC m = (MonadState TransState m, MonadWriter [TransError] m)



transVar :: MonadError TransError m
         => Symtab.Symtab Env -> Symtab.Symtab Ty
         -> Var a -> m (Exp (Ty (), a))
transVar venv tenv var = case var of
  SimpleVar symbol _ -> undefined
  FieldVar var1 symbol _ -> undefined
  SubscriptVar var1 exp1 _ -> undefined


annotTy :: Ty () -> Exp a -> m (Exp (Ty (), a))
annotTy t e = pure $ (t, ) <$> e

getTy :: Exp (Ty (), a) -> m (Ty ())
getTy = pure . fst . extract

checkOp :: MonadError TransError m
        => Op -> Ty () -> Ty () -> m (Ty ())
checkOp op tl tr
  | isArith = do
  , IntTy == tl
  , IntTy == tr
  = pure IntTy
  | isComp =  undefined
  | isEq = undefined
  where
    isArith = any (op ==) [Plus, Minus, Times, Divide]
    isComp  = any (op ==) [Lt, Le, Gt, Ge]
    isEq    = any (op ==) [Eq, Neq]

transExp :: MonadError TransError m
         => Symtab.Symtab Env -> Symtab.Symtab (Ty ())
         -> Exp a -> m (Exp (Ty (), a))
transExp venv tenv exp = case exp of
  Var v
    -> transVar venv tenv v

  Nil _
    -> annotTy NilTy exp

  Int _
    -> annotTy IntTy exp

  String s
    -> annotTy StringTy exp

  Call symb args _
    -> undefined

  Op l o r _
    -> do
    tl <- getTy l
    tr <- getTy r
    to <- checkOp o tl tr
    annotTy to exp

  Record fields recTy _
    -> case Symtab.lookup tenv recTy of
         Just actualRecTy@(RecordTy fieldTys uid)
           | length fields == length fieldTys ->
               do { zipWithM checkFieldTy fields fieldTys
                  ; annotTy actualRecTy exp
                  }
         Just _ -> throwError TypeMismatch
         Nothing -> throwError TypeUndefined
    where
      checkFieldTy fe fty = do
        fe' <- goTE fe
        ty <- getTy fe'
        guard $ ty == fty
        return fe'

  Seq seqs
    | null seqs
      -> annotTy UnitTy exp
    | otherwise
      -> mapM goTE seqs >>= flip annotTy exp . last

  Assign v e _
    -> undefined
  If c t me _
    -> undefined
  While ce be _
    -> undefined
  For s b e1 e2 e3 _
    -> undefined
  Break _
    -> undefined
  Let binds e _
    -> undefined
  Array symb e1 e2 _
    -> undefined

  where
    goTE = transExp venv tenv

transDec :: MonadError TransError m
         => Symtab.Symtab Env -> Symtab.Symtab (Ty a)
         -> Dec a -> m (Symtab.Symtab Env, Symtab.Symtab (Ty a))
transDec venv tenv dec = case dec of
  FunctionDecl fs
     -> undefined
  VarDecl{..}
     -> undefined
  TypeDecl ds
     -> undefined


transTy :: MonadError TransError m
        => Symtab.Symtab Ty
        -> Ty a -> m (Ty a)
transTy tenv ty = case ty of
  NameTy symb maybeTy _
     -> undefined
  RecordTy fields uniq
     -> undefined
  ArrayTy symv uniq _
     -> undefined
  NilTy
     -> undefined
  IntTy
     -> undefined
  StringTy
     -> undefined
  UnitTy
     -> undefined

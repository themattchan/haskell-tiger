{-# LANGUAGE RecordWildCards, TupleSections, OverloadedStrings #-}

-- | Type checking and translation to intermediate code
module Language.Tiger.Semant where

import Data.List

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import Language.Tiger.Types
import Language.Tiger.Gensym
import qualified Language.Tiger.Env as Env
import qualified Language.Tiger.Symtab as Symtab

data TransError = TypeMismatch | TypeUndefined | NoHOF | UnboundVariable
                | NoSuchField

transVar :: MonadError TransError m
         => Symtab.Symtab Env.Env -> Symtab.Symtab (Ty ())
         -> Var a -> m (Exp (Ty (), a))
transVar venv tenv var = case var of
  SimpleVar varName a
    -> case Symtab.lookup venv varName of
         Just (VarTy ty) -> pure (Var var (ty, a))
         Just _ -> throwError NoHOF
         Nothing -> throwError UnboundVariable

  FieldVar recVar fieldName a
    -> do
      Var _ (ty, _)  <- transVar venv tenv recVar
      case ty of
        RecordTy fields _ _ -> case lookup fieldName fields of
          Just fieldTy -> undefined
          Nothing -> throwError NoSuchField
        _ -> throwError TypeUndefined

  SubscriptVar var1 exp1 _
    -> do
      Var _ (ty, _)  <- transVar venv tenv recVar
      undefined

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
         => Symtab.Symtab Env.Env -> Symtab.Symtab (Ty ())
         -> Exp a -> m (Exp (Ty (), a))
transExp venv tenv exp = case exp of
  Var v _
    -> transVar venv tenv v

  Nil _
    -> annotTy NilTy exp

  Int _
    -> annotTy IntTy exp

  String s
    -> annotTy StringTy exp

  Call funTy args _
    -> case Symtab.lookup venv

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
               do { zipWithM checkTy fields fieldTys
                  ; annotTy actualRecTy exp
                  }
         Just _ -> throwError TypeMismatch
         Nothing -> throwError TypeUndefined

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
    -> do (venv', tenv') <- foldM (uncurry transDec) (venv,tenv) binds
          ty <- transExp venv' tenv' e
          annotTy ty exp

  Array arrTy sizeE initE _
    -> case Symtab.lookup tenv arrTy of
         Just actualArrTy@(ArrayTy initTy _ _) -> do
           checkTy sizeE IntTy
           checkTy initE initTy
           annotTy actualArrTy exp
         Just _ -> throwError TypeMismatch
         Nothing -> throwError TypeUndefined
  where
    goTE = transExp venv tenv
    checkFieldTy fe fty = do
      fe' <- goTE fe
      ty <- getTy fe'
      guard $ ty == fty
      return fe'

transDec :: MonadError TransError m
         => Symtab.Symtab Env.Env -> Symtab.Symtab (Ty a)
         -> Dec a -> m (Symtab.Symtab Env.Env, Symtab.Symtab (Ty a))
transDec venv tenv dec = case dec of
  FunctionDecl fs
     -> foldM (uncurry goFun) (venv,tenv) fs
  VarDecl{..}
     -> undefined
  TypeDecl ds
     -> undefined
  where
    goFun venv tenv Function{..} = undefined
--      case Symtab.lookup funName venv of


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

  baseTy -> return baseTy

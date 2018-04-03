{-# LANGUAGE RecordWildCards, TupleSections, OverloadedStrings, LambdaCase #-}

-- | Chapter 5: Semantic analysis
-- Type checking and translation to intermediate code
module Language.Tiger.Semant where

import Data.List

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Writer.Lazy (WriterT(..))
import Data.Functor.Identity
import qualified Data.Sequence as Seq

import Language.Tiger.AST hiding (Ty(..))
import qualified Language.Tiger.AST as AST
import Language.Tiger.Loc (SrcSpan, Span(..))
import Language.Tiger.Types
import Language.Tiger.Gensym
import qualified Language.Tiger.Env as Env
import qualified Language.Tiger.Symtab as Symtab

data TransError
  = TypeMismatch Ty Ty
  | TypeUndefined Symbol
  | NoHOF
  | UnboundVariable Symbol
  | NoSuchField Symbol Symbol Ty

instance Show TransError where
  show = \case
    TypeMismatch expect actual
      -> "Type mismatch: expected " <> show expect <> " got " <> show actual
    TypeUndefined ty
      -> "Undefined type: " <> show ty
    NoHOF
      -> "Higher order functions not implemented"
    UnboundVariable v
      -> "Unbound variable: " <> show v
    NoSuchField fld r rt
      -> "No field named " <> show fld <> " in record " <> show r <> " of type " <> show rt

newtype MultipleErrors = MultipleErrors
  { runMultipleErrors :: Seq.Seq (Span TransError) }

emitError :: (HasSrcSpan a, MonadWriter MultipleErrors m)
          => a -> TransError -> m ()
emitError = tell . Seq.singleton . Span . sp

-- | Variable environment
type VEnv = Symtab.Symtab Env.EnvEntry

-- | Typing environment
type TEnv = Symtab.Symtab Ty

type CheckConstraints m a =
  ( HasSrcSpan a
  , MonadWriter MultipleErrors m
  , MonadError MultipleErrors m
  , MonadReader (VEnv, TEnv) m
  )

type CheckM = (ExceptT MultipleErrors -- can probably get rid of this...
               (WriterT MultipleErrors
                 (ReaderT (VEnv, TEnv)
                   Identity)))

runCheckM :: CheckM a -> Either MultipleErrors a
runCheckM = fmap fst
          . runIdentity
          . runReaderT (Symtab.venv0, Symtab.tenv0)
          . runWriterT
          . runExceptT

lookupV :: (MonadReader (VEnv,TEnv) m) => Symbol -> m (Maybe EnvEntry)
lookupV n = Symtab.lookup n <$> asks fst

lookupT :: (HasSrcSpan a, MonadWriter MultipleErrors m)
        => TEnv -> Symbol -> a -> m (Maybe Ty)
lookupT env t a = maybe (emitError a (TypeUndefined t)) pure $ Symtab.lookup t env

transProg :: CheckConstraints m a
          -> Exp a -> m ()
transProg e = do
  transExp e
  errs <- ask
  if not (Seq.null errs)
    then throwError errs
    else pure ()

transVar :: CheckConstraints m a
         => VEnv -> TEnv
         -> Var a -> m (Exp (Ty, a))
transVar venv tenv = \case
  SimpleVar varName a
    -> Symtab.lookupV varName >>= \case
         Just (VarTy ty) -> pure (Var var (ty, a))
         Just _ -> emitError a NoHOF
         Nothing -> emitError a UnboundVariable

  FieldVar recVar fieldName a
    -> do
      Var _ (ty, _)  <- transVar venv tenv recVar
      case ty of
        RecordTy fields _ _ -> case lookup fieldName fields of
          Just fieldTy -> undefined
          Nothing -> emitError a NoSuchField
        _ -> emitError a TypeUndefined

  SubscriptVar var1 exp1 a
    -> do
      Var _ (ty, _)  <- transVar venv tenv recVar
      undefined

annotTy :: Ty -> Exp a -> Exp (Ty, a)
annotTy t e = (t, ) <$> e

getTy :: Exp (Ty, a) -> Ty
getTy = fst . ann

checkOp :: (HasSrcSpan a)
        => Op -> Ty -> Ty -> a -> m Ty
checkOp op tl tr a
  | isArith
  = if IntTy == tl && IntTy == tr
    then pure IntTy
    else emitError a (TypeMismatch IntTy)
  | isComp =  undefined
  | isEq = undefined
  where
    isArith = any (op ==) [Plus, Minus, Times, Divide]
    isComp  = any (op ==) [Lt, Le, Gt, Ge]
    isEq    = any (op ==) [Eq, Neq]

checkTy :: Ty -> Ty -> m ()
checkTy expect declared = undefined


transExp :: (HasSrcSpan a, MonadWriter MultipleErrors m)
         => VEnv -> TEnv
         -> Exp a -> m (Exp (Ty, a))
transExp venv tenv exp = case exp of
  Var v _
    -> transVar venv tenv v

  Nil _
    -> annotTy NilTy exp

  Int _
    -> annotTy IntTy exp

  String s
    -> annotTy StringTy exp

  Call funTy args a
    -> undefined --case Symtab.lookup venv

  Op l o r _ -> do
    <- transExp
    tl <- getTy l
    tr <- getTy r
    to <- checkOp o tl tr
    annotTy to exp

  Record fields recTy _
    -> lookupT recTy >>= \case
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

  Break a
    -> undefined

  Let binds e _
  -- this only handles the nonrecursive binding case
    -> do (venv', tenv') <- foldM (uncurry transDec) (venv,tenv) binds
          ty <- transExp venv' tenv' e
          annotTy ty exp

  Array arrTy sizeE initE _
    -> case Symtab.lookup tenv arrTy of
         Just actualArrTy@(ArrayTy initTy _ _) -> do
           checkExpTy sizeE IntTy
           checkExpTy initE initTy
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

transDec :: CheckConstraints m a
         => VEnv -> TEnv
         => Dec a
         -> m (VEnv, TEnv)
transDec venv tenv dec = case dec of
  FunctionDecl fs a ->
    let doFun venv tenv Function{..} =
          do formals <- for funParams $ \Field{..} ->
               do ty <- lookupT tenv fieldType fieldAnnot
                  -- TODO throw error if type not defined...
                  pure (fieldName, VarEntry ty)

             retTy <- maybe (pure UnitTy) (uncurry (lookupT tenv)) funResult

             let formalTys = map snd formals
             let funTy = FunEntry formalTys retTy
             let venv' = Symtab.inserts ((funName, funTy):formals) venv

             _ <- transExp venv' tenv' funBody

             return (venv', tenv)
    in
      foldM (uncurry doFun) (venv, tenv) fs

  VarDecl{name, typ, init, meta} ->
    do init' <- transExp venv tenv init
       let initTy = getTy init'
       declaredTy <- traverse (uncurry (lookupT tenv)) typ
       varTy <- case declaredTy of
         Nothing -> pure initTy
         Just ty -> do checkTy initTy ty; pure ty
       let venv' = Symtab.insert name varTy venv
       return (venv', tenv)

  TypeDecl ds a ->
    do tenv' <- foldM (\te (s, ast_t,_) -> do
                          ty <- transTy te ast_t
                          pure $ Symtab.insert s ty te
                      ) tenv ds
       return (venv, tenv')

transTy :: (MonadWriter MultipleErrors m)
        => TEnv
        -> AST.Ty a -> m Ty
transTy tenv ty = case ty of
  NameTy symb maybeTy _
     -> undefined
  RecordTy fields uniq
     -> undefined
  ArrayTy symv uniq _
     -> undefined

  baseTy -> return baseTy

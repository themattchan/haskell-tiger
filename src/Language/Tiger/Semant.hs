{-# LANGUAGE RecordWildCards, TupleSections, OverloadedStrings, LambdaCase #-}

-- | Chapter 5: Semantic analysis
-- Type checking and translation to intermediate code
module Language.Tiger.Semant where

import Data.List
import qualified Data.Sequence as Seq

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Writer.Lazy (WriterT(..))
import Control.Monad.Tardis (TardisT(..))
import Data.Functor.Identity
import qualified Data.List as List

import Language.Tiger.AST hiding (Ty(..))
import qualified Language.Tiger.AST as AST
import Language.Tiger.Loc (SrcSpan, Span(..))
import Language.Tiger.Types
import Language.Tiger.Gensym
import qualified Language.Tiger.Env as Env
import qualified Language.Tiger.Symtab as Symtab
import Language.Tiger.Utils

--------------------------------------------------------------------------------
transProg :: Semant m a
          -> Exp a -> m (Exp (Ty, a))
--------------------------------------------------------------------------------
transProg e = do
  e' <- transExp Env.venv0 Env.tenv0 0 e
  errs <- ask
  if not (Seq.null errs)
    then throwError errs
    else pure e'

--------------------------------------------------------------------------------
transExp :: (HasSrcSpan a, MonadWriter MultipleErrors m)
         => VEnv -> TEnv
         -> Bool
         -> Exp a
         -> m (Exp (Ty, a))
--------------------------------------------------------------------------------
transExp venv tenv canBreak exp =
  let
    go = transExp venv tenv

    goCheck cb e expectTy = do
      e' <- go e cb
      checkTy (ann e') expectTy
      return e'

  in case exp of
    Var v ann ->
      do v' <- transVar venv tenv v
         pure $ Var v' (getTy v', ann)

    Nil ann ->
      NilTy (NilTy, ann)

    Int i ann ->
      Int i (IntTy, ann)

    String s ann ->
      String s (StringTy, ann)

    Call fun args ann ->
      case Symtab.lookup venv fun of
        Just (FunEntry argsTy returnTy)
          | length args == length argsTy ->
            do args' <- for (zip args argsTy) $ \(argE, argTy) ->
                 do argE' <- goCheck False argE argTy
                    return argE'
               return $ Call fun args' (returnTy, ann)
          | otherwise ->
            fatal ann (ArityError fun (length argsTy) (length args))

        Nothing -> fatal ann (UndefinedFunction fun)

    Op l o r ann -> do
      l' <- go False l
      r' <- go False r
      to <- checkOp o (getTy l') (getTy r')
      return $ Op l' o r' (to, ann)

    Seq seqs ann
      | null seqs ->
          Seq [] (UnitTy,ann)
      | otherwise ->
        do seqs' <- mapM (go canBreak) seqs
           return $ Seq seqs' (getTy (last seqs), ann)

    Assign v e ann ->
      do v' <- transVar venv tenv v
         e' <- go False e
         let vTy = getTy v'
         when (vTy /= NilTy) $ -- FIXME probably need to update venv
           checkTy (ann v') (getTy e')
         return $ Assign v' e' (UnitTy, ann)

    If c t mf ann ->
      do c' <- goCheck False c IntTy
         t' <- go canBreak t'
         case mf of
           Nothing ->
             do checkTy (ann t') UnitTy
                return $ If c' t' Nothing (UnitTy, ann)

           Just f ->
             do f' <- goCheck canBreak f (getTy t')
                return $ If c' t' f' (getTy t', ann)

    While c b ann ->
      do c' <- goCheck False c IntTy
         b' <- transExp venv tenv True b
         checkTy (ann b') UnitTy
         return $ While c' b' (UnitTy, ann)

    For i lo hi b ann ->
      do lo' <- goCheck False lo IntTy
         hi' <- goCheck False hi IntTy
         b' <- transExp (Symtab.insert i (VarEntry IntTy) venv) tenv True
         checkTy (ann b') UnitTy
         return $ For i lo' hi' b' (UnitTy, ann)

    Break ann ->
      do unless canBreak $
           nonfatal ann BreakOutsideLoop
           -- Break can only occur inside a loop body
           -- PROVIDED that body is If (then and else branches), or Seq, or body
           -- of let.

         pure $ Break (UnitTy, ann)

    Let binds e ann ->
      do (binds', (venv', tenv')) <-
           mapAccumLM (flip (uncurry transDec)) (venv,tenv) binds
         e' <- transExp venv' tenv' canBreak e
         return $ Let binds' e' (getTy e',ann)

    Record fields recTySymb ann ->
      case Symtab.lookup tenv recTySymb of
        Just ty@(RecordTy fieldTys _) ->
          | length fields == length fieldTys ->
              do fields' <- for (zip fields fieldTys) $
                    \((recFldName, recFldExp, recFldAnn),
                        Field{fieldName, fieldType, fieldAnnot}) ->
                       do recFldName == fieldName
                          e' <- go False recFldExp
                          let fldType = fromJust $ Symtab.lookup tenv fieldType
                           --  this should not fail
                          checkTy (ann e') fldType
                          return (recFldName, e', (getTy e', recFldAnn))
                 return $ Record fields' recTySymb (ty, ann)

          | otherwise ->
             fatal ann $
               TypeMismatch recTySymb
                   (Right ty)
                   (RecordTy (map (\(x,y,_) -> (x,y)) fields) 99999)

        -- recTySymb is not a record
        Just ty -> fatal ann (TypeMismatch recTySymb (Left RecordTyC) ty)

        Nothing -> nonfatal ann (TypeUndefined recTySymb)

    Array arrTySymb sizeE initE ann ->
      case Symtab.lookup tenv arrTySymb of
        Just ty@(ArrayTy elemTy _) -> do
          sizeE' <- goCheck False sizeE IntTy
          initE' <- goCheck False initE elemTy
          return $ Array arrTySymb sizeE' initE' (ty, ann)

        Just ty -> fatal ann (TypeMismatch arrTySymb (Left ArrayTyC) ty)
        Nothing -> nonfatal ann (TypeUndefined arrTySymb) -- if nonfatal,
                   -- should probably add this type into env and continue...

--------------------------------------------------------------------------------
transDec :: Semant m a
         => VEnv -> TEnv
         => Dec a
         -> m (Dec (Ty, a), (VEnv, TEnv))
--------------------------------------------------------------------------------
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
         Just ty -> do checkTy (ann init') ty; pure ty
       let venv' = Symtab.insert name varTy venv
       return (venv', tenv)

  TypeDecl [(s, ast_t,_)] a ->
    do ty <- transTy tenv ast_t
       let tenv' = Symtab.insert s ty tenv
       return (venv, tenv')

--------------------------------------------------------------------------------
transVar :: Semant m a
         => VEnv -> TEnv
         -> Var a -> m (Var (Ty, a))
--------------------------------------------------------------------------------
transVar venv tenv = \case
  SimpleVar varName a ->
    case Symtab.lookup venv varName of
      Just (VarEntry{ty}) -> return $ SimpleVar varName (ty,a)
      Just (FunEntry{}) -> nonfatal a NoHOF
      Nothing -> nonfatal a (UnboundVariable varName)

  FieldVar recVar fieldName a ->
    do recVar' <- transVar venv tenv recVar
       case getTy recVar' of
         RecordTy fields _ ->
           case List.lookup fieldName fields of
             Just fieldTy -> return $ FieldVar recVar' fieldName (fieldTy, a)
             Nothing -> nonfatal a (NoSuchField fieldName recVar (getTy recVar'))
         otherTy -> fatal a (NamedTypeMismatch recVar (Left RecordTyC) otherTy)

  SubscriptVar arrVar indexExp a ->
    do arrVar' <- transVar venv tenv recVar
       case getTy arrVar' of
         Array elemTy _ ->
           do indexExp' <- transExp venv tenv 0 indexExp
              checkTy (ann indexExp') elemTy
              return $ SubscriptVar arrVar' indexExp' (elemTy, a)

         otherTy -> fatal a (NamedTypeMismatch arrVar (Left ArrayTyC) otherTy)

--------------------------------------------------------------------------------
transTy :: (Gensym m, MonadWriter MultipleErrors m)
        => TEnv
        -> AST.Ty a -> m Ty
--------------------------------------------------------------------------------
transTy tenv = \case
  AST.NameTy symb _
    | symb == "nil"    -> return NilTy
    | symb == "unit"   -> return UnitTy
    | symb == "int"    -> return IntTy
    | symb == "string" -> return StringTy
    | otherwise -> undefined
      -- do case Symtab.lookup tenv symb of
      --      undefined

  AST.RecordTy fields _ ->
    -- FIXME what about
    -- type myrec := { x: int, nest: myrec } ??
    do flds <- traverse (sequence . (fieldName &&& lookupT tenv . fieldType)) fields
       uniq <- gensym
       return $ RecordTy flds uniq

  AST.ArrayTy symb _ ->
    Array <$> lookupT tenv symb <*> gensym

--------------------------------------------------------------------------------
-- * Errors

newtype MultipleErrors = MultipleErrors
  { runMultipleErrors :: Seq.Seq (Span SemantError) }

instance Show MultipleErrors where
  show = unlines . map show . Seq.toList . runMultipleErrors

data SemantError
  = NamedTypeMismatch Symbol (Either TyC Ty) Ty
  | TypeMismatch Ty Ty
  | TypeUndefined Symbol
  | ArityError Symbol Int Int
  | NoHOF
  | UndefinedFunction Symbol
  | UnboundVariable Symbol
  | RepeatedDefinition Symbol
  | NoSuchField Symbol Symbol Ty
  | BreakOutsideLoop

instance Show SemantError where
  show = \case
    NamedTypeMismatch symb expect actual ->
      "Type mismatch: "
      <> show symb <> " is a "
      <> show actual <> " not a "
      <> either show show expect
    TypeMismatch expect actual ->
      "Type mismatch: expected " <> show expect <> " got " <> show actual
    TypeUndefined ty ->
      "Undefined type: " <> show ty
    ArityError fn expect actual ->
      show fn <> " called with the wrong number of arguments, expected "
      <> show expect <> " got " <> show actual
    NoHOF ->
      "Higher order functions not implemented"
    UndefinedFunction fn ->
      "Undefined function: " <> show fn
    UnboundVariable v ->
      "Unbound variable: " <> show v
    RepeatedDefinition s ->
      "Repeated definition of " <> show s
    NoSuchField fld r rt ->
      "No field named " <> show fld <> " in record " <> show r <> " of type " <> show rt
    BreakOutsideLoop ->
      "break statement outside loop"

-- | log an error and keep going
nonfatal :: (HasSrcSpan a, MonadWriter MultipleErrors m)
         => a -> SemantError -> m ()
nonfatal = tell . Seq.singleton . Span . sp

-- | log an error and halt
fatal :: (HasSrcSpan a, MonadWriter MultipleErrors m, MonadError MultipleErrors m)
      => a -> SemantError -> m ()
fatal ann err = do
  nonfatal ann err
  errs <- ask
  throwError errs

--------------------------------------------------------------------------------
-- * Monad

-- | Variable environment
type VEnv = Symtab.Symtab Env.EnvEntry

-- | Typing environment
type TEnv = Symtab.Symtab Ty

type Semant m a =
  ( HasSrcSpan a
  , MonadWriter MultipleErrors m
  , MonadError MultipleErrors m
  , MonadReader (VEnv, TEnv) m
  )

type SemantM = (ExceptT MultipleErrors -- can probably get rid of this...
                 (WriterT MultipleErrors
                   (ReaderT (VEnv, TEnv)
                     Identity)))

runSemantM :: SemantM a -> Either MultipleErrors a
runSemantM = fmap fst
           . runIdentity
           . runReaderT (Symtab.venv0, Symtab.tenv0)
           . runWriterT
           . runExceptT

--------------------------------------------------------------------------------
-- * Helpers

lookupV :: (MonadReader (VEnv,TEnv) m) => Symbol -> m (Maybe EnvEntry)
lookupV n = Symtab.lookup n <$> asks fst

lookupT :: (HasSrcSpan a, MonadWriter MultipleErrors m)
        => TEnv -> Symbol -> a -> m Ty
lookupT env t a = maybe (emitError a (TypeUndefined t)) pure $ Symtab.lookup t env

getTy :: Ann f => f (Ty, a) -> Ty
getTy = fst . ann

checkOp :: Semant m a -- (HasSrcSpan a)
        => Op -> Ty -> Ty -> a
        -> m Ty
checkOp op tl tr a
  | tl /= tr
  = emitError a (TypeMismatch tl tr)
  | isArith
  = checks [tl, tr] IntTy

  | isComp = undefined
  | isEq = undefined
  where
    checks expect = (expect <$) . traverse (\x -> checkTy (x,a) expect)

    isArith = any (op ==) [Plus, Minus, Times, Divide]
    isComp  = any (op ==) [Lt, Le, Gt, Ge]
    isEq    = any (op ==) [Eq, Neq]

-- | Given an annotation from a Typed AST piece, check that the inferred type has
-- the asserted type.
checkTy :: (Ty, SrcSpan) -> Ty -> m ()
checkTy (actual, ss) declared
  | actual == declared = pure ()
  | otherwise = fatal ss (TypeMismatch actual declared)

module Language.Tiger.Env where
import Language.Tiger.Types
import qualified Language.Tiger.Translate as Translate
import qualified Language.Tiger.Temp as Temp

{-
data Env
  = VarTy { access :: Translate.Access, ty :: Ty }
  | FunTy { level :: Translate.Level, label :: Temp.Label
          , formals :: [Ty], result :: Ty }

venv0 :: Symtab.Symtab Env
venv0 = fromList
  [ ("print", FunEntry [StringTy] UnitTy)
  , ("flush", FunEntry [] UnitTy)
  , ("getchar", FunEntry [] StringTy)
  , ("ord", FunEntry [StringTy] IntTy)
  , ("chr", FunEntry [IntTy] StringTy)
  , ("size", FunEntry [StringTy] IntTy)
  , ("substring", FunEntry [StringTy, IntTy, IntTy] IntTy)
  , ("concat", FunEntry [StringTy, StringTy] StringTy)
  , ("not", FunEntry [IntTy] IntTy)
  , ("exit", FunEntry [IntTy] UnitTy)
  ]

tenv0 :: Symtab.Symtab Ty
tenv0 = fromList
  [ ("int", IntTy)
  , ("string", StringTy)
  ]
-}

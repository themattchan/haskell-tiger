module Types where

type Symbol = String -> Int     -- hashed string

data Var a
  = SimpleVar Symbol a
  | FieldVar (Var a) Symbol a
  | SubscriptVar (Var a) (Exp a) a

data Op = Plus | Minus | Times | Divide | Eq | Neq | Lt | Le | Gt | Ge

data Exp a
  = Var (Var a)
  | Nil
  | Int Int
  | String String a
  | Call Symbol [Exp a] a
  | Op (Exp a) Op (Exp a) a
  | Record [(Symbol, Exp a, a)] Symbol a
  | Seq [(Exp a, a)]
  | Assign (Var a) (Exp a) a
  | If (Exp a) (Exp a) (Maybe (Exp a)) a
  | While (Exp a) (Exp a) a
  | For Symbol Bool (Exp a) (Exp a) (Exp a) a
  | Break a
  | Let [Decl a] (Exp a) a
  | Array Symbol (Exp a) (Exp a) a

data Decl a
  = FunDecl [Function a]
  | VarDecl Symbol Bool (Maybe (Symbol, a)) (Exp a) a
  | TyDecl  [(Symbol, Ty a, a)]

data Ty a
  = NameTy Symbol a
  | RecordTy [Field a]
  | ArrayTy Symbol a

data Field a = Field { fieldName :: Symbol
                     , fieldEscape :: Bool
                     , fieldType :: Symbol
                     , fieldAnnot :: a
                     }

data Function a = Function { funName :: Symbol
                           , funParams :: [Field a]
                           , funResult :: Maybe (Symbol,a)
                           , funBody :: Exp a
                           , funAnnot :: a
                           }

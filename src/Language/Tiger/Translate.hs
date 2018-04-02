{-# LANGUAGE FlexibleContexts #-}

-- | Chapter 6: Activation Records

module Language.Tiger.Translate where

import Language.Tiger.Gensym
import qualified Language.Tiger.IR as IR
import qualified Language.Tiger.Temp as Temp
import qualified Language.Tiger.Access as Access

data Level = Outermost
           | Level { parent :: Level
                   , name :: Temp.Label
                   , formals :: [Bool]
                   }

--data Access = Access Level Frame.Access

data Exp = Ex IR.Exp | Nx IR.Stm | Cx (Temp.Label -> Temp.Label -> IR.Stm)

unEx :: Gensym Temp.Temp m => Exp -> m IR.Exp
unEx (Ex e) = return e
unEx (Nx stm) = return $ IR.ESeq stm (IR.Const 0)
unEx (Cx genstm) = do
  r <- Temp.newTemp
  t <- Temp.newLabel
  f <- Temp.newLabel
  return $
    IR.ESeq (IR.seq [ IR.Move (IR.Temp r) (IR.Const l)
                    , genstm t f
                    , IR.Label f
                    , IR.Move (IR.Temp r) (IR.Const 0)
                    , IR.Label t
                    ])
            (IR.Temp r)

unNx :: Gensym Temp.Temp m => Exp -> m IR.Stm
unNx (Ex e) = return $ IR.Exp e
unNx (Nx s) = s
unNx cx = unEx cx >>= unNx

unCx :: (Gensym Temp.Temp m) => Exp -> m (Temp.Label -> Temp.Label -> IR.Stm)
unCx (Ex (IR.Const 0 _)) = return $ \_ f -> IR.Jump (IR.Name f [f])
unCx (Ex (IR.Const 1 _)) = return $ \t _ -> IR.Jump (IR.Name t [t])
unCx (Ex e) = return $ \t f -> IR.CJump IR.Ne e (IR.Const 0) t f
unCx (Nx _) = error "IMPOSSIBLE: unCx (Nx _)"
unCx (Cx c) = return c


-- simpleVar :: Access -> Level -> Exp
-- simpleVar = undefined

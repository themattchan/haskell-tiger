-- Chapter 7: Translation to Intermediate Code

module Language.Tiger.IR where
import Data.List
import qualified Language.Tiger.Temp as Temp

data Exp
  = Const Int
  | Name Temp.Label
  | Temp Temp.Temp
  | Binop Binop Exp Exp
  | Mem Exp
  | Call Exp [Exp]
  | ESeq Stm Exp

data Stm
  = Move Exp Exp
  | Exp Exp
  | Jump Exp [Temp.Label]
  | CJump Relop Exp Exp Temp.Label Temp.Label
  | Seq Stm Stm
  | Label Temp.Label

seq :: [Stm] -> Stm
seq = foldr1 Seq

data Binop
  = Plus | Minus | Mul | Div
  | And | Or | LShift | RShift | ArShift | Xor

data Relop
  = Eq | Ne | Lt | Gt | Le | Ge
  | Ult | Ule | Ugt | Uge

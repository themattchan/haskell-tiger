module Language.Tiger.Token where

import qualified Data.ByteString.Lazy as BL

data Token
  = Var
  | While
  | For
  | To
  | Break
  | Let
  | In
  | End
  | Function

  | Type
  | Array
  | If
  | Then
  | Else
  | Do
  | Of
  | Nil

  | Comma
  | Colon
  | Semi
  | LPar
  | RPar
  | LBrack
  | RBrack
  | LBrace
  | RBrace
  | Dot
  | Plus
  | Minus
  | Times
  | Divide
  | Eq
  | Neq
  | Gt
  | Lt
  | Ge
  | Le
  | And
  | Or
  | Assign

  | IntLit Int
  | Ident BL.ByteString
  | StringLit BL.ByteString
  deriving (Eq, Show)

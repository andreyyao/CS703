module Ast where

data Binop = Add | Sub | Mul deriving (Eq, Show)

data Constant = ConstBool Bool | ConstInt Int deriving (Eq, Show)

data Expr
  = Var String
  | Const Constant
  | Binary Expr Binop Expr
  | Projl Expr
  | Projr Expr
  | Pair Expr Expr
  | Define String Expr
  | Set String Expr
  | Lambda String Tipe Expr
  | App Expr Expr
  | Let String Tipe Expr Expr
  | Callcc Expr
  | Abort Expr
  deriving (Eq)

data Tipe
  = TFunc Tipe Tipe
  | TProd Tipe Tipe
  | TSum Tipe Tipe
  | TVoid -- The "false" type, with no elements
  | TBool
  | TInt
  deriving (Eq)
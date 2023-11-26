module Ast where

data Binop = Add | Sub | Mul deriving (Eq)

data Constant = ConstBool Bool | ConstInt Int deriving (Eq)

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
  | Let String Expr Expr
  | Callcc Expr
  | Abort Expr
  | Hole Tipe
  deriving (Eq)

data Tipe
  = TFunc Tipe Tipe
  | TProd Tipe Tipe
  | TVoid -- The "false" type, with no elements
  | TBool
  | TInt
  deriving (Eq)

wrap :: [Char] -> [Char]
wrap s = "(" ++ s ++ ")"

instance Show Expr where
  show e = case e of
    Var x -> x
    Const c -> show c
    Binary e1 op e2 -> wrap (show e1 ++ " " ++ show op ++ " " ++ show e2)
    Projl e -> "fst" ++ wrap (show e)
    Projr e -> "snd" ++ wrap (show e)
    Pair e1 e2 -> "(" ++ show e1 ++ "," ++ show e2 ++ ")"
    Lambda x t e -> "lambda " ++ x ++ " : " ++ show t ++ ". " ++ show e
    App e1 e2 -> wrap (show e1) ++ wrap (show e2)
    Let x e1 e2 -> "let " ++ x ++ " := " ++ show e1 ++ " in " ++ show e2
    Callcc e -> "call/cc" ++ wrap (show e)
    Abort e -> "abort (" ++ show e ++ ")"
    Hole t -> "{|" ++ show t ++ "|}"

instance Show Tipe where
  show t = case t of
    TInt -> "Int"
    TBool -> "Bool"
    TVoid -> "Void"
    TProd x y -> atomize x ++ " * " ++ atomize y
    TFunc x y -> atomize x ++ " -> " ++ atomize y
    where
      atomize t' = case t' of
        TInt -> show t'
        TBool -> show t'
        TVoid -> show t'
        TProd _ _ -> "(" ++ show t' ++ ")"
        TFunc _ _ -> "(" ++ show t' ++ ")"

instance Show Constant where
  show c = case c of
    ConstBool b -> show b
    ConstInt i -> show i

instance Show Binop where
  show c = case c of
    Add -> "+"
    Mul -> "*"
    Sub -> "-"
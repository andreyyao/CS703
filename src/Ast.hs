module Ast where

data Binop = Add | Sub | Mul | Equal | Gt | Lt | And | Or deriving (Eq)

data Constant = ConstBool Bool | ConstInt Int deriving (Eq)

data Expr
  = Var String
  | Const Constant
  | Binary Expr Binop Expr
  | Projl Expr
  | Projr Expr
  | Pair Expr Expr
  | Lambda String Tipe Expr
  | Branch Expr Expr Expr
  | App Expr Expr
  | Let String Expr Expr
  | Callcc Expr
  | Abort Tipe Expr
  | Hole Tipe
  deriving (Eq)

data Tipe
  = TFunc Tipe Tipe
  | TProd Tipe Tipe
  | TVoid -- The "false" type, with no elements
  | TBool
  | TInt
  | TCustom String
  deriving (Eq, Ord)

wrap :: String -> String
wrap s = "(" ++ s ++ ")"

instance Show Expr where
  show expr = case expr of
    Var x -> x
    Const c -> show c
    Binary e1 op e2 -> atomize e1 ++ " " ++ show op ++ " " ++ atomize e2
    Projl e -> "fst " ++ atomize e
    Projr e -> "snd " ++ atomize e
    Pair e1 e2 -> "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
    Branch b e1 e2 -> "if " ++ show b ++ " then " ++ atomize e1 ++ " else " ++ atomize e2
    Lambda x t e -> "lambda " ++ x ++ " : " ++ show t ++ ". " ++ show e
    App e1 e2 -> atomize e1 ++ " " ++ atomize e2
    Let x e1 e2 -> "let " ++ x ++ " := " ++ show e1 ++ " in " ++ show e2
    Callcc e -> "callcc " ++ atomize e
    Abort t e -> "abort " ++ show t ++ " " ++ atomize e
    Hole t -> "{|" ++ show t ++ "|}"
    where
      atomize e = case e of
        Var _ -> show e
        Const _ -> show e
        Hole _ -> show e
        Pair _ _ -> show e
        _ -> wrap (show e)

instance Show Tipe where
  show tipe = case tipe of
    TInt -> "Int"
    TBool -> "Bool"
    TVoid -> "Void"
    TCustom s -> s
    TProd x y -> atomize x ++ " * " ++ atomize y
    TFunc x y -> atomize x ++ " -> " ++ atomize y
    where
      atomize t = case t of
        TProd _ _ -> wrap (show t)
        TFunc _ _ -> wrap (show t)
        _ -> show t

instance Show Constant where
  show c = case c of
    ConstBool b -> if b then "true" else "false"
    ConstInt i -> show i

instance Show Binop where
  show c = case c of
    Add -> "+"
    Mul -> "*"
    Sub -> "-"
    Equal -> "="
    Lt -> "<"
    Gt -> ">"
    And -> "&"
    Or -> "|"


sizeof expr =
  case expr of
    Var _ -> 1
    Const _ -> 1
    Binary e1 _ e2 -> 1 + sizeof e1 + sizeof e2
    Projl e -> 1 + sizeof e
    Projr e -> 1 + sizeof e
    Pair e1 e2 -> 1 + sizeof e1 + sizeof e2
    Branch b e1 e2 -> 1 + sizeof b + sizeof e1 + sizeof e2
    Lambda x t e -> 1 + sizeof e
    App e1 e2 -> 1 + sizeof e1 + sizeof e2
    Let x e1 e2 -> 1 + sizeof e1 + sizeof e2
    Callcc e -> 1 + sizeof e
    Abort t e -> 1 + sizeof e
    Hole t -> 0
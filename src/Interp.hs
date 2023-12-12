module Interp(interp, Value) where
import qualified Data.Map as Map
import Debug.Trace
import Ast

-- The interpreter is implemented as a shallow embedding into the Haskell semantics

data Value
  = VClosure Kont -- A lambda paired with an enrivonment
  | VConstant Constant
  | VPair Value Value -- It could be a pair of closures or something

type Env = Map.Map String Value

-- This is like the evaluation context
type Kont = Value -> Value

eval :: Expr -> Env -> Kont -> Value
eval expr env k =
  case expr of
    Const c -> k (VConstant c)
    Var x -> case Map.lookup x env of
      Just v -> k v
      _ -> error "Unbound variable"
    Binary e1 op e2 -> eval e1 env $ \v1 -> eval e2 env $ \v2 -> case (v1, v2) of
      (VConstant c1, VConstant c2) ->
       k (VConstant (evalConst c1 op c2))
      _ -> error "Invalid addition"
    Projl e -> eval e env $ \v -> case v of
      VPair v1 _ -> k v1
      _ -> error "hehe"
    Projr e -> eval e env $ \v -> case v of
      VPair _ v2 -> k v2
      _ -> error "hehe"
    Pair e1 e2 -> eval e1 env $ \v1 -> eval e2 env $ \v2 -> k (VPair v1 v2)
    Branch b e1 e2 -> eval b env $ \v -> case v of
      VConstant (ConstBool cond) -> if cond then eval e1 env k else eval e2 env k
      _ -> error "hehe"
    Lambda x _ e -> VClosure (\v' -> eval e (Map.insert x v' env) k)
    App e1 e2 -> eval e1 env $ \v1 -> eval e2 env $ \v2 -> case v1 of
      VClosure k' -> k' v2
      _ -> error "hehe"
    Let x e1 e2 -> eval e1 env $ \v1 -> eval e2 (Map.insert x v1 env) k
    Callcc e -> case eval e env id of
      VClosure k' -> k' (VClosure k)
      _ -> error "hehe"
    Abort _ e -> eval e env id
    Hole _ -> error "Encountered hole during interpretation"

interp :: Expr -> Value
interp expr = eval expr Map.empty id

instance Show Value where
  show val = case val of
    VClosure _ -> "<closure>"
    VConstant c -> show c
    VPair v1 v2 -> "(" ++ show v1 ++ ", " ++ show v2 ++ ")"

evalConst :: Constant -> Binop -> Constant -> Constant
evalConst c1 op c2 = case (c1, c2) of
  (ConstInt n1, ConstInt n2) -> case op of
    Add -> ConstInt $ n1 + n2
    Sub -> ConstInt $ n1 - n2
    Mul -> ConstInt $ n1 * n2
    Equal -> ConstBool $ n1 == n2
    Lt -> ConstBool $ n1 < n2
    Gt -> ConstBool $ n1 > n2
    _ -> error "Wrong operator on integers"
  (ConstBool b1, ConstBool b2) -> ConstBool $ case op of
    And -> b1 && b2
    Or -> b1 || b2
    _ -> error "Wrong operator on bools"
  _ -> error "Cannot apply binops on int and bool"

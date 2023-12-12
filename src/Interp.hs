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
type Kont = Value -> Env -> Value

eval :: Expr -> Env -> Kont -> Value
eval expr env k =
  case expr of
    Const c -> k (VConstant c) env
    Var x ->
      case Map.lookup x env of
        Just v -> k v env
        _ -> error "Unbound variable"
    Binary e1 op e2 -> eval e1 env $ \v1 env' -> eval e2 env' $ \v2 env'' -> case (v1, v2) of
      (VConstant (ConstInt a), VConstant (ConstInt b)) ->
       let res = case op of
            Add -> a + b
            Mul -> a * b
            Sub -> a - b in
        k (VConstant $ ConstInt res) env''
      _ -> error "Invalid addition"
    Projl e -> eval e env $ \v env' -> case v of
      VPair v1 _ -> k v1 env'
      _ -> error "hehe"
    Projr e -> eval e env $ \v env' -> case v of
      VPair _ v2 -> k v2 env'
      _ -> error "hehe"
    Pair e1 e2 -> eval e1 env $ \v1 env' -> eval e2 env' $ \v2 env'' -> k (VPair v1 v2) env''
    Branch b e1 e2 -> eval b env $ \v _ -> case v of
      VConstant (ConstBool cond) -> if cond then eval e1 env k else eval e2 env k
      _ -> error "hehe"
    Lambda x _ e -> VClosure (\v' env' -> eval e (Map.insert x v' env) k)
    App e1 e2 -> eval e1 env $ \v1 env' -> eval e2 env' $ \v2 _ -> case traceShowId v1 of
      VClosure k' -> k' v2 env
      _ -> error "hehe"
    Let x e1 e2 -> eval e1 env $ \v1 env' -> eval e2 (Map.insert x v1 env') k
    Callcc e -> case eval e env const of
      VClosure k' -> k' (VClosure k) env
      _ -> error "hehe"
    Abort _ e -> eval e env const
    Hole _ -> error "Encountered hole during interpretation"

interp :: Expr -> Value
interp expr = eval expr Map.empty const

instance Show Value where
  show val = case val of
    VClosure _ -> "<closure>"
    VConstant c -> show c
    VPair v1 v2 -> "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
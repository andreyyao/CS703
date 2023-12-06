module Interp(interp, Value) where
import qualified Data.Map as Map
import Ast

-- The interpreter is implemented as a shallow embedding into the Haskell semantics

type Lamb = (String, Tipe, Expr)

data Value
  = Closure Lamb Env -- A lambda paired with an enrivonment
  | Konstant Constant
  | VPair Value Value -- It could be a pair of closures or something

type Env = Map.Map String Value

type Kont = Value -> Env -> Value

eval :: Expr -> Env -> Kont -> Value
eval expr env k =
  case expr of
    Const c -> Konstant c
    Var x ->
      case Map.lookup x env of
        Just v -> k v env
        _ -> error "Unbound variable"
    Binary e1 op e2 -> eval e1 env $ \v1 env' -> eval e2 env' $ \v2 env'' -> case (v1, v2) of
      (Konstant (ConstInt a), Konstant (ConstInt b)) ->
       let res = case op of
            Add -> a + b
            Mul -> a * b
            Sub -> a - b in
        k (Konstant $ ConstInt res) env''
      _ -> error "Invalid addition"
    Projl e -> eval e env $ \v env' -> case v of
      VPair v1 _ -> v1
      _ -> error "hehe"
    Projr e -> eval e env $ \v env' -> case v of
      VPair _ v2 -> v2
      _ -> error "hehe"
    Pair e1 e2 -> eval e1 env $ \v1 env' -> eval e2 env' $ \v2 env'' -> VPair v1 v2
    Lambda x t e -> k (Closure (x, t, e) env) env
    App e1 e2 -> eval e1 env $ \v1 env' -> eval e2 env' $ \v2 env'' -> case v1 of
      Closure (x, _, e) env''' -> eval e (Map.insert x v2 env''') k
      _ -> error "hehe"
    Let x e1 e2 -> eval e1 env $ \v1 env' -> eval e2 (Map.insert x v1 env') k
    Callcc e -> error "unimplemented"
    Abort e -> eval e env const
    Hole _ -> error "Encountered hole during interpretation"

instance Show Value where
  show val = case val of
    Closure (x, t, e) _ -> "closure<<" ++ show (Lambda x t e) ++ ">>"
    Konstant c -> show c
    VPair v1 v2 -> "(" ++ show v1 ++ ", " ++ show v2 ++ ")"

interp :: Expr -> Value
interp expr = eval expr Map.empty const
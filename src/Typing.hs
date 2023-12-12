module Typing(Context, typecheck) where

import Data.Map (Map)
import qualified Data.Map as Map
import Ast
import GHC.Base (liftA2)

-- The typing context is a mapping from variable names to their types, if the variables are bound in the context.
type Context = Map String Tipe

-- typecheck e returns `Just t` if `e` type checks to `t` in the empty context, and `Nothing` otherwise
typecheck :: Expr -> Maybe Tipe
typecheck = typecheck' Map.empty

-- typecheck c e returns `Just t` if `e` type checks to `t` in context `c`, and `Nothing` otherwise
typecheck' :: Context -> Expr -> Maybe Tipe
typecheck' ctxt expr =
  case expr of
    Var v -> Map.lookup v ctxt
    Const c ->
      case c of
        ConstBool _ -> Just TBool
        ConstInt _ -> Just TInt
    Binary e1 op e2 ->
      case (typecheck' ctxt e1, typecheck' ctxt e2) of
        (Just TInt, Just TInt) | op == Add || op == Sub || op == Mul -> Just TInt
        (Just TInt, Just TInt) | op == Gt || op == Lt || op == Gt -> Just TBool
        (Just TInt, Just TInt) | op == And || op == Or -> Just TBool
        _ -> Nothing
    Projl e ->
      case typecheck' ctxt e of
        Just (TProd t1 _) -> Just t1
        _ -> Nothing
    Projr e ->
      case typecheck' ctxt e of
        Just (TProd _ t2) -> Just t2
        _ -> Nothing
    Pair e1 e2 -> liftA2 TProd (typecheck' ctxt e1) (typecheck' ctxt e2)
    Branch b e1 e2 -> do
      tb <- typecheck' ctxt b
      t1 <- typecheck' ctxt e1
      t2 <- typecheck' ctxt e2
      case tb of
        TBool | t1 == t2 -> Just t1
        _ -> Nothing
    Lambda x t e -> do
      t' <- typecheck' (Map.insert x t ctxt) e
      Just (TFunc t t')
    App e1 e2 -> do
      t1' <- typecheck' ctxt e2
      tf <- typecheck' ctxt e1
      case tf of
        TFunc t1 t2 -> if t1 == t1' then Just t2 else Nothing
        _ -> Nothing
    Let x e1 e2 ->
      typecheck' ctxt e1 >>= (\t -> typecheck' (Map.insert x t ctxt) e2)
    Callcc e ->
      case typecheck' ctxt e of
        Just (TFunc (TFunc t TVoid) t') | t == t' -> Just t
        _ -> Nothing
    Abort t e ->
      case typecheck' ctxt e of
        Just TVoid -> Just t
        _ -> Nothing
    Hole t -> Just t
module Typing where

import Data.Map (Map)
import qualified Data.Map as Map
import Ast
import GHC.Base (liftA2)

-- The typing context is a mapping from variable names to their types, if the variables are bound in the context.
type Context = Map String Tipe

-- typecheck c e returns `Just t` if `e` type checks to `t` in context `c`, and `Nothing` otherwise
typecheck :: Context -> Expr -> Maybe Tipe
typecheck ctxt e =
  case e of
    Var v -> Map.lookup v ctxt
    Const c ->
      case c of
        ConstBool _ -> Just TBool
        ConstInt _ -> Just TInt
    Binary e1 _ e2 ->
      case (typecheck ctxt e1, typecheck ctxt e2) of
        (Just TInt, Just TInt) -> Just TInt
        _ -> Nothing
    Projl e ->
      case typecheck ctxt e of
        Just (TProd t1 t2) -> Just t1
        _ -> Nothing
    Projr e ->
      case typecheck ctxt e of
        Just (TProd t1 t2) -> Just t2
        _ -> Nothing
    Pair e1 e2 -> liftA2 TProd (typecheck ctxt e1) (typecheck ctxt e2)
    Lambda x t e ->
      typecheck (Map.insert x t ctxt) e
    Let x e1 e2 ->
      typecheck ctxt e1 >>= (\ t -> typecheck (Map.insert x t ctxt) e)
    Callcc e ->
      case typecheck ctxt e of
        Just (TFunc (TFunc t TVoid) TVoid) -> Just t
        _ -> Nothing
    Abort e ->
      case typecheck ctxt e of
        Just TVoid -> error "unimplemented"
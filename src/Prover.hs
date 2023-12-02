module Prover where
import Ast
import Logic

typeToProp :: Ast.Tipe -> Logic.Prop
typeToProp t =
  case t of
    TInt -> Atom "Int"
    TBool -> Atom "Bool"
    TFunc t1 TVoid -> Neg (typeToProp t1)
    TFunc t1 t2 -> Impl (typeToProp t1) (typeToProp t2)
    TProd t1 t2 -> Conj (typeToProp t1) (typeToProp t2)
    TVoid -> error "Nothing should have type TVoid"

propToType :: Logic.Prop -> Ast.Tipe
propToType p =
  case p of
    Neg p -> TFunc (propToType p) TVoid
    Conj p1 p2 -> TProd (propToType p1) (propToType p2)
    Impl p1 p2 -> TFunc (propToType p1) (propToType p2)
    Atom s
      | s == "Int" -> TInt
      | s == "Bool" -> TBool
      | otherwise -> error "implement custom types"
module Prover where
import Ast
import Logic
import Typing
import qualified Logic
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

type2Prop :: Tipe -> Prop
type2Prop t =
  case t of
    TInt -> Atom "Int"
    TBool -> Atom "Bool"
    TFunc t1 TVoid -> Neg (type2Prop t1)
    TFunc t1 t2 -> Impl (type2Prop t1) (type2Prop t2)
    TProd t1 t2 -> Conj (type2Prop t1) (type2Prop t2)
    TVoid -> error "Nothing should have type TVoid"

ctxt2Forest :: Context -> Forest
ctxt2Forest = error "TODO implement"

prop2Type :: Prop -> Tipe
prop2Type p =
  case p of
    Neg p -> TFunc (prop2Type p) TVoid
    Conj p1 p2 -> TProd (prop2Type p1) (prop2Type p2)
    Impl p1 p2 -> TFunc (prop2Type p1) (prop2Type p2)
    Atom s
      | s == "Int" -> TInt
      | s == "Bool" -> TBool
      | otherwise -> error "implement custom types"

conc2Expr :: Conclusion -> Expr
conc2Expr c = let (d, p) = c in
  case d of
    Axiom v -> Var v
    Unary r c1 ->
      let e = conc2Expr c1 in
      case r of
        DoubleNegation -> Callcc e
        AndElim1 -> Projl e
        AndElim2 -> Projr e
    Logic.Binary r c1 c2 ->
      let (e1, e2) = (conc2Expr c1, conc2Expr c2) in
      case r of
        ModusPonens -> App e1 e2
        AndIntro -> Pair e1 e2

-- synthesize e returns `Just e'` where `e'` is `e` with holes replace, and it returns `Nothing` if synthesis failed.
synthesize :: Expr -> Maybe Expr
synthesize e = fmap snd (synth' Map.empty e)

synth' :: Typing.Context -> Expr -> Maybe (Tipe, Expr)
synth' ctxt expr =
  case expr of
    Var v -> Just (Maybe.fromJust (Map.lookup v ctxt), expr)
    Const c ->
      case c of
        ConstBool _ -> Just (TBool, expr)
        ConstInt _ -> Just (TInt, expr)
    Ast.Binary e1 o e2 ->
      case (synth' ctxt e1, synth' ctxt e2) of
        (Just (TInt, e1'), Just (TInt, e2')) -> Just (TInt, Ast.Binary e1' o e2')
        _ -> Nothing
    Projl e ->
      case synth' ctxt e of
        Just (TProd t1 t2, e') -> Just (t1, Projl e')
        _ -> Nothing
    Projr e ->
      case synth' ctxt e of
        Just (TProd t1 t2, e') -> Just (t2, Projr e')
        _ -> Nothing
    Pair e1 e2 ->
      case (synth' ctxt e1, synth' ctxt e2) of
        (Just (t1, e1'), Just (t2, e2')) -> Just (TProd t1 t2, Pair e1' e2')
    Lambda x t e ->
      fmap (\p -> (fst p, Lambda x t (snd p))) (synth' (Map.insert x t ctxt) e)
    App e1 e2 -> do
      (t1, e1') <- synth' ctxt e2
      (tf, e2') <- synth' ctxt e1
      case tf of
        TFunc t1' t2 -> if t1 == t1' then Just (t2, App e1' e2') else Nothing
        _ -> Nothing
    Let x e1 e2 -> do
      (t1', e1') <- synth' ctxt e1
      (t2', e2') <- synth' (Map.insert x t1' ctxt) e2
      Just (t2', Let x e1' e2')
    Callcc e -> case synth' ctxt e of
      Just (TFunc (TFunc t TVoid) TVoid, e') -> Just (t, Callcc e')
      Nothing -> Nothing
    Abort e ->
      case synth' ctxt e of
        Just (TVoid, e') -> error "unimplemented"
    Hole t ->
      let p = type2Prop t in
      let f = ctxt2Forest ctxt in
      case synthesisLoop 10 f p of
        Just c -> Just (t, conc2Expr c)
        Nothing -> Nothing
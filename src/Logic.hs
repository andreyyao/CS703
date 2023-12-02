module Logic where
import qualified Data.Map as M

data Prop
  = Atom String -- ATOMIC PROPOSITIONS
  | Conj Prop Prop -- AND
  | Impl Prop Prop -- IMPLIES
  | Neg Prop -- Negation
  deriving(Ord, Eq)
-- Example : Conj (Atom "Int") (Atom "Bool") is the type (Int * Bool)


-- This will just be the name of the inference rules
data Inference
  = ModusPonens
  | DoubleNegation
  | AndIntro
  | AndElim1
  | AndElim2
  deriving(Eq)
  -- | ...

type Conclusion = (Derivation, Prop)
data Derivation
  = Binary Inference Conclusion Conclusion -- The two derivations are the two trees in the premise, and the Prop is the conclusion
  | Unary Inference Conclusion -- One premise, e.g. Double negation elimination
  | Axiom 
  deriving(Eq)

-- For example, we start with an (Axiom, P) conclusion for every type T translated in the context translated to P

-- This terminates when we found the "proposition" representing the type of the hole

-- Just a map from Props to the derivation trees
-- We want this because we don't want derivation trees with duplicate conclusions
type Forest = M.Map Prop Conclusion

-- `unaryHelper` can give one, two, or no derivation trees for any given derivation
unaryHelper :: Conclusion -> [Conclusion]
unaryHelper c = let (d, p) = c in
  case p of
    (Neg (Neg p)) -> [(Unary DoubleNegation c, p)]
    (Conj p1 p2) -> [(Unary AndElim1 c, p1), (Unary AndElim2 c, p2)]
    _ -> []

binaryHelper :: Conclusion -> Conclusion -> Conclusion
binaryHelper c1 c2 = let (d1, p1) = c1 in let (d2, p2) = c2 in
  case (p1, p2) of
    (Impl q1 q2, q1') | q1 == q1' -> (Binary ModusPonens c1 c2, q2)
    _ -> (Binary AndIntro c1 c2, Conj p1 p2)

-- This function grows the forest once
growForest :: Forest -> Forest
growForest f =
  let unaryUpdates = M.foldl (\acc c -> unaryHelper c : acc) [] f in
  let binaryUpdates = M.foldl (\acc c -> foldl (\acc' c' -> binaryHelper c c' : acc') acc f) [] f in
    -- inserts all the updates into f
  foldl (\g c -> M.insertWith (\_ old -> old) (snd c) c g) f (concat unaryUpdates ++ binaryUpdates)

-- `synthesisLoop fuel f` returns `Just c` when succesfully derived target proposition `p`.
-- It returns `Nothing` if the fuel ran out and we didn't find anything
synthesisLoop :: Int -> Forest -> Prop -> Maybe Conclusion
synthesisLoop fuel f p =
  if fuel == 0
  then M.lookup p f
  else synthesisLoop (fuel - 1) (growForest f) p

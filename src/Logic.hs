module Logic where
import qualified Data.Map as M

data Prop
  = Atom String -- ATOMIC PROPOSITIONS
  | Conj Prop Prop -- AND
  | Impl Prop Prop -- IMPLIES
  | Neg Prop -- Negation
  deriving(Ord, Eq)
-- Example : Conj (Atom "Int") (Atom "Bool") is the type (Int * Bool)


-- Binary inference rule names
data BinRule
  = ModusPonens
  | Composition
  | AndIntro
  deriving(Eq)

-- Unary inference rule names
data UnaRule
  = DoubleNegation
  | AndElim1
  | AndElim2
  deriving(Eq)

type Conclusion = (Derivation, Prop)
data Derivation
  = Binary BinRule Conclusion Conclusion -- The two derivations are the two trees in the premise, and the Prop is the conclusion
  | Unary UnaRule Conclusion -- One premise, e.g. Double negation elimination
  | Axiom String -- The string stores the variable name
  deriving(Eq)

-- For example, we start with an (Axiom, P) conclusion for every type T translated in the context translated to P

-- This terminates when we found the "proposition" representing the type of the hole

-- Just a map from Props to the derivation trees
-- We want this because we don't want derivation trees with duplicate conclusions
type Forest = M.Map Prop Conclusion

-- `unaryHelper` can give one, two, or no derivation trees for any given derivation
unaryHelper :: Conclusion -> [Conclusion]
unaryHelper c =
  case snd c of
    (Neg (Neg p)) -> [(Unary DoubleNegation c, p)]
    (Conj p1 p2) -> [(Unary AndElim1 c, p1), (Unary AndElim2 c, p2)]
    _ -> []

binaryHelper :: Conclusion -> Conclusion -> Conclusion
binaryHelper c1 c2 =
  case (snd c1, snd c2) of
    (Impl p1 p2, p1') | p1 == p1' -> (Binary ModusPonens c1 c2, p2)
    (Impl p1 p2, Impl p2' p3') | p2 == p2' -> (Binary Composition c1 c2, Impl p1 p3')
    (p1, p2) -> (Binary AndIntro c1 c2, Conj p1 p2)

-- This function grows the forest once
growForest :: Forest -> Forest
growForest f =
  let unaryUpdates = M.foldl (\acc c -> unaryHelper c : acc) [] f in
  let binaryUpdates = M.foldl (\acc c -> foldl (\acc' c' -> binaryHelper c c' : acc') acc f) [] f in
    -- inserts all the updates into f, but keep old values untouched if key already present
  foldl (\g c -> M.insertWith (\_ old -> old) (snd c) c g) f (concat unaryUpdates ++ binaryUpdates)

-- `synthesisLoop fuel f` returns `Just c` when succesfully derived target proposition `p`.
-- It returns `Nothing` if the fuel ran out and we didn't find anything
synthesisLoop :: Int -> Forest -> Prop -> Maybe Conclusion
synthesisLoop fuel f p =
  if fuel <= 0
  then M.lookup p f
  else let f' = growForest f in
    case M.lookup p f' of
    Just c -> Just c
    Nothing -> synthesisLoop (fuel - 1) f' p

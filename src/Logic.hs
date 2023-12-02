module Logic where

data Prop
  = Atom String -- ATOMIC PROPOSITIONS
  | Conj Prop Prop -- AND
  | Impl Prop Prop -- IMPLIES
  | Neg Prop -- Negation

-- Example : Conj (Atom "Int") (Atom "Bool") is the type (Int * Bool)

-- This will just be the name of the inference rules
data Inference
  = ModusPonens
  | DoubleNegation
  -- | ...

data Derivation
  = Binary Inference Conclusion Conclusion -- The two derivations are the two trees in the premise, and the Prop is the conclusion
  | Unary Inference Conclusion -- One premise, e.g. Double negation elimination
  | Axiom

data Conclusion = (Derivation, Prop)
-- For example, we start with an (Axiom, P) conclusion for every type T translated in the context translated to P

-- This terminates when we found the "proposition" representing the type of the hole
generateTree :: [Derivation] -> Prop -> [Derivation]
generateTree derivations holeProp =
  for d in derivation:
  match d's conclusion
  | (Neg (Neg P)) -> add (Unary DoubleNegation d P) to current list of derivations

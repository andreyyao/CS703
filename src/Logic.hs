module Logic where

data Prop
  = Atom String -- ATOMIC PROPOSITIONS
  | Conj Prop Prop -- AND
  | Impl Prop Prop -- IMPLIES
  | Neg Prop -- Negation
  deriving(Eq)
-- Example : Conj (Atom "Int") (Atom "Bool") is the type (Int * Bool)


-- This will just be the name of the inference rules
data Inference
  = ModusPonens
  | DoubleNegation
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


unaryHelper :: Conclusion -> Conclusion
unaryHelper c = let (d, p) = c in
  case p of
    (Neg (Neg p)) -> (Unary DoubleNegation c, p)
    _ -> c

binaryHelper :: Conclusion -> Conclusion -> Conclusion
binaryHelper c1 c2 = let (d1, p1) = c1 in let (d2, p2) = c2 in
  case (p1, p2) of
    (Impl q1 q2, q1') | q1 == q1' -> (Binary ModusPonens c1 c2, q2)
    (q1', Impl q1 q2) | q1 == q1' -> (Binary ModusPonens c2 c1, q2)
    _ -> c1
  

generateTree :: [Conclusion] -> [Conclusion]
generateTree cs = map unaryHelper cs


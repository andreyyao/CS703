module Prover where

data Formula
  = Atom String
  | Implies Formula Formula
  | And Formula Formula
  | False
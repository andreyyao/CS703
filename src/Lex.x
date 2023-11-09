{
module Lex (scanMany) where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+                        ;
  "--".*                         ;
  let                            { \s -> Let }
  match                          { \s -> Match }
  inl                            { \s -> InL }
  inr                            { \s -> InR }
  call/cc                        { \s -> CallCC }
  proj1                          { \s -> Proj1 }
  proj2                          { \s -> Proj2 }
  $digit+                        { \s -> Int (read s) }
  [\=\+\-\*\/\(\)]               { \s -> Sym (head s) }
  "true"                         { \s -> Bool True}
  "false"                        { \s -> Bool False}
  $alpha [$alpha $digit \_ \']*  { \s -> Var s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = Let
  | Proj1
  | Proj2
  | Match
  | InL
  | InR
  | CallCC
  | Sym Char
  | Var String
  | Int Int
  | Bool Bool
  deriving (Eq, Show)

scanMany :: String -> [Token]
scanMany input = do
  alexScanTokens input
}  
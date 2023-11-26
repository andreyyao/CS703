{
module Lexer (scanMany, Token(..)) where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-z]          -- alphabetic characters

tokens :-

  $white+                        ;
  "--".*                         ;
  Int                            { \s -> Int }
  Bool                           { \s -> Bool }
  Void                           { \s -> Void }
  callcc                         { \s -> CallCC }
  abort                          { \s -> Abort }
  lambda                         { \s -> Lambda }
  let                            { \s -> Let }
  in                             { \s -> In }
  fst                            { \s -> Proj1 }
  snd                            { \s -> Proj2 }
  set                            { \s -> set }
  $digit+                        { \s -> IntLit (read s) }
  "true"                         { \s -> BoolLit True}
  "false"                        { \s -> BoolLit False}
  $alpha [$alpha $digit \_ \']*  { \s -> Var s }
  "."                            { \s -> Dot }
  ","                            { \s -> Comma }
  ":"                            { \s -> Colon }
  "("                            { \s -> LParen }
  ")"                            { \s -> RParen }
  "+"                            { \s -> Plus }
  "-"                            { \s -> Minus }
  "*"                            { \s -> Times }
  "->"                           { \s -> Arrow }
  ":="                           { \s -> Coloneq }
  "{|"                           { \s -> LBrack }
  "|}"                           { \s -> RBrack }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = Proj1
  | Proj2
  | CallCC
  | Let
  | In
  | Lambda
  | Set
  | Abort
  | Var String
  | IntLit Int
  | BoolLit Bool
  | Int
  | Bool
  | Void
  | Arrow
  | Dot
  | Colon
  | Comma
  | Coloneq
  | Slash
  | LParen
  | RParen
  | Plus
  | Minus
  | Times
  | LBrack
  | RBrack
  deriving (Eq, Show)

scanMany :: String -> [Token]
scanMany input = do
  alexScanTokens input
}
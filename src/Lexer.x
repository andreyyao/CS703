{
module Lexer (scanMany, Token(..)) where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-z]          -- alphabetic characters

tokens :-

  $white+                        ;
  "--".*                         ;
  call/cc                        { \s -> CallCC }
  abort                          { \s -> Abort }
  fst                            { \s -> Proj1 }
  snd                            { \s -> Proj2 }
  $digit+                        { \s -> IntLit (read s) }
  "true"                         { \s -> BoolLit True}
  "false"                        { \s -> BoolLit False}
  $alpha [$alpha $digit \_ \']*  { \s -> Var s }
  "."                            { \s -> Dot }
  ","                            { \s -> Comma }
  ":"                            { \s -> Colon }
  "\\"                           { \s -> Slash }
  "("                            { \s -> LParen }
  ")"                            { \s -> RParen }
  "+"                            { \s -> Plus }
  "-"                            { \s -> Minus }
  "*"                            { \s -> Times }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = Proj1
  | Proj2
  | CallCC
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
  | Slash
  | LParen
  | RParen
  | Plus
  | Minus
  | Times
  deriving (Eq, Show)

scanMany :: String -> [Token]
scanMany input = do
  alexScanTokens input
}
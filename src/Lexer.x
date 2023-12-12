{
module Lexer (scanMany, Token(..)) where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-z]          -- alphabetic characters

tokens :-

  $white+                        ;
  "--".*                         ;
  Int                            { \_ -> Int }
  Bool                           { \_ -> Bool }
  Void                           { \_ -> Void }
  callcc                         { \_ -> CallCC }
  abort                          { \_ -> Abort }
  "lambda"                       { \_ -> Lambda }
  "λ"                            { \_ -> Lambda }
  let                            { \_ -> Let }
  in                             { \_ -> In }
  fst                            { \_ -> Proj1 }
  snd                            { \_ -> Proj2 }
  if                             { \_ -> If }
  then                           { \_ -> Then }
  else                           { \_ -> Else }
  $digit+                        { \s -> IntLit (read s) }
  "true"                         { \_ -> BoolLit True}
  "false"                        { \_ -> BoolLit False}
  $alpha [$alpha $digit \_ \']*  { \s -> Var s }
  "."                            { \_ -> Dot }
  ","                            { \_ -> Comma }
  ":"                            { \_ -> Colon }
  "("                            { \_ -> LParen }
  ")"                            { \_ -> RParen }
  "+"                            { \_ -> Plus }
  "-"                            { \_ -> Minus }
  "*"                            { \_ -> Times }
  "="                            { \_ -> Equal }
  "<"                            { \_ -> Lt }
  ">"                            { \_ -> Gt }
  "&"                            { \_ -> And }
  "|"                            { \_ -> Or }
  "->"                           { \_ -> Arrow }
  ":="                           { \_ -> Coloneq }
  "{|"                           { \_ -> LBrack }
  "|}"                           { \_ -> RBrack }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = Proj1
  | Proj2
  | Let
  | In
  | If
  | Then
  | Else
  | Lambda
  | Abort
  | CallCC
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
  | Equal
  | Lt
  | Gt
  | And
  | Or
  | LBrack
  | RBrack
  deriving (Eq, Show)

scanMany :: String -> [Token]
scanMany = alexScanTokens
}
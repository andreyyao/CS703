{
module Parser where

import Lexer as L
import Ast
import Debug.Trace
}

%name parseExpr Expr
%name parseType Type
%tokentype { L.Token }
%error { parseError }

%left '+' '-'
%right '->'
%left '*'
%left '=' '<' '>'
%left '&' '|'


%token
var             { L.Var $$ }
iLit            { L.IntLit $$ }
bLit            { L.BoolLit $$ }
proj1           { L.Proj1 }
proj2           { L.Proj2 }
abort           { L.Abort }
let             { L.Let }
in              { L.In }
callcc          { L.CallCC }
if              { L.If }
then            { L.Then }
else            { L.Else }
lambda          { L.Lambda }
Int             { L.Int }
Bool            { L.Bool }
Void            { L.Void }
'+'             { L.Plus }
'-'             { L.Minus }
'*'             { L.Times }
'='             { L.Equal }
'<'             { L.Lt }
'>'             { L.Gt }
'&'             { L.And }
'|'             { L.Or }
'.'             { L.Dot }
','             { L.Comma }
':'             { L.Colon }
ass             { L.Coloneq }
'('             { L.LParen }
')'             { L.RParen }
'->'            { L.Arrow }
lbrack          { L.LBrack }
rbrack          { L.RBrack }

%%


Type : Int { Ast.TInt }
     | Bool { Ast.TBool }
     | Void { Ast.TVoid }
     | Type '*' Type { Ast.TProd $1 $3 }
     | Type '->' Type { Ast.TFunc $1 $3 }
     | '(' Type ')' { $2 }

Expr
  : lambda var ':' Type '.' Expr { Ast.Lambda $2 $4 $6 }
  | let var ass Expr in Expr { Ast.Let $2 $4 $6 }
  | if Expr then Expr else Expr { Ast.Branch $2 $4 $6 }
  | Expr '-' Expr { Ast.Binary $1 Ast.Sub $3 }
  | Expr '+' Expr { Ast.Binary $1 Ast.Add $3 }
  | Expr '*' Expr { Ast.Binary $1 Ast.Mul $3 }
  | Expr '=' Expr { Ast.Binary $1 Ast.Equal $3 }
  | Expr '<' Expr { Ast.Binary $1 Ast.Lt $3 }
  | Expr '>' Expr { Ast.Binary $1 Ast.Gt $3 }
  | Expr '&' Expr { Ast.Binary $1 Ast.And $3 }
  | Expr '|' Expr { Ast.Binary $1 Ast.Or $3 }
  | callcc Expr { Ast.Callcc $2 }
  | abort Type Expr { Ast.Abort $2 $3 }
  | proj1 Expr { Ast.Projl $2 }
  | proj2 Expr { Ast.Projr $2 }
  | EApp { $1 }

EApp
  : EApp EAtom { Ast.App $1 $2 }
  | EAtom { $1 }

EAtom
  : '(' Expr ',' Expr ')' { Ast.Pair $2 $4 }
  | '(' Expr ')' { $2 }
  | lbrack Type rbrack { Ast.Hole $2 }
  | iLit { Ast.Const (Ast.ConstInt $1) }
  | bLit { Ast.Const (Ast.ConstBool $1) }
  | var { Ast.Var $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
{
module Parser where

import Lexer as L
import Ast
}

%name parse
%tokentype { L.Token }
%error { parseError }

%left '+' '-'
%left '*'
%right arrow

%token
var             { L.Var $$ }
iLit            { L.IntLit $$ }
bLit            { L.BoolLit $$ }
proj1           { L.Proj1 }
proj2           { L.Proj2 }
abort           { L.Abort }
callcc          { L.CallCC }
lambda          { L.Lambda}
Int             { L.Int }
Bool            { L.Bool }
Void            { L.Void }
'+'             { L.Plus }
'-'             { L.Minus }
'*'             { L.Times }
'.'             { L.Dot }
','             { L.Comma }
':'             { L.Colon }
'\\'            { L.Slash }
'('             { L.LParen }
')'             { L.RParen }
arrow           { L.Arrow }

%%


Type : Int { Ast.TInt }
     | Bool { Ast.TBool }
     | Void { Ast.TVoid }
     | Type '+' Type { Ast.TSum $1 $3 }
     | Type '*' Type { Ast.TProd $1 $3 }
     | Type arrow Type { Ast.TFunc $1 $3 }

Expr : lambda var ':' Type '.' Expr { Ast.Lambda $2 $4 $6 }
     | '(' Expr ',' Expr ')' { Ast.Pair $2 $4 }
     | Expr '-' Expr { Ast.Binary $1 Ast.Sub $3 }
     | Expr '+' Expr { Ast.Binary $1 Ast.Add $3 }
     | Expr '*' Expr { Ast.Binary $1 Ast.Mul $3 }
     | callcc Expr { Ast.Callcc $2 }
     | abort Expr { Ast.Abort $2 }
     | proj1 Expr { Ast.Projl $2 }
     | proj2 Expr { Ast.Projr $2 }
     | Expr Expr { Ast.App $1 $2 }
     | iLit { Ast.Const (Ast.ConstInt $1) }
     | bLit { Ast.Const (Ast.ConstBool $1) }
     | var { Ast.Var $1 }
     | '(' Expr ')' { $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
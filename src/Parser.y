{
module Parser where

import Lexer as L
import Ast
}

%name parse
%tokentype { L.Token }
%error { parseError }

%token
sym             { L.Sym $$ }
var             { L.Var $$ }
iLit            { L.IntLit $$ }
bLit            { L.BoolLit $$ }
match           { L.Match }
inl             { L.InL }
inr             { L.InR }
proj1           { L.Proj1 }
proj2           { L.Proj2 }
callcc          { L.CallCC }
Int             { L.Int }
Bool             { L.Bool }
'+'             { L.Sym $$ }
'-'             { L.Sym $$ }
'*'             { L.Sym $$ }
'.'             { L.Dot }
','             { L.Comma }
':'             { L.Colon }
'\\'            { L.Slash }
'('             { L.LParen }
')'             { L.RParen }
arrow           { L.Arrow }

%%

Exp : Exp '-' Exp { Ast.Binary $1 Ast.Sub $3 }
    | Exp '+' Exp { Ast.Binary $1 Ast.Add $3 }
    | Exp '*' Exp { Ast.Binary $1 Ast.Mul $3 }
    | '(' Exp ')' { $2 }

Typ : Int { Ast.TInt }
    | Bool { Ast.TBool }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
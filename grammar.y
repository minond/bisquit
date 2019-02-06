main = { expr } ;

expr = unary-expr
     | binary-expr
     | match-expr
     | cond-expr
     | bind-expr
     | let-expr
     | scalar
     ;

unary-expr = op scalar ;
binary-expr = scalar op scalar ;
match-expr = expr "match" match-case { match-case } ;
match-case = "|" expr "=>" expr ;
cond-expr = "if" expr "then" expr "else" expr ;
bind-expr = ( arg-decl | func-decl ) "=" expr ;
let-expr = "let" { bind-expr } "in" expr ;

arg-decl = id [ type-decl ] ;
type-decl = ":" id ;
func-decl = id , "(" [ arg-decl { arg-decl } ] ")" type-decl ;
func-app = id , "(" [ expr { "," expr } ] ")" ;

scalar = id | number | bool ;
bool = "true" | "false" ;
id = letter , { letter | digit | "_" } ;

number = [ "-" ] ( number-dec | number-bin | number-hex ) ;
number-dec = digit , [ "." ] , { digit } ;
number-bin = "0b" , number-bin-opt , { number-bin-opt } ;
number-hex = "0x" , number-hex-opt , { number-hex-opt } ;
number-bin-opt = "0" | "1" ;
number-hex-opt = digit | "A" | "B" | "C" | "D" | "E" | "F" ;

op     = "+" | "-" | "*" | "&" | "^" | "%" | "!" | "\" | "|" | ">" | "<" ;
digit  = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
letter = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L"
       | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X"
       | "Y" | "Z" ;

(* Sample:

toString(n : int) : string =
  n match
    | 0 => "zero"
    | 1 => "one"
    | 2 => "two"
    | 3 => "three"
    | 4 => "four"
    | _ => "???"

yes : string =
  if true
  then "Yes"
  else "No"

*)

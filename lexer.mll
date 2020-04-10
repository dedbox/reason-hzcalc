{
  open Parser

  exception Error of string

}

let spc = [' ' '\t']+
let num = '-'? ['0'-'9']+
let sym = ['a'-'z']+
let endl = '\r' | '\n' | "\r\n"

rule read = parse
  | spc       { read lexbuf }
  | endl      { Lexing.new_line lexbuf; read lexbuf }
  | "num"     { NUM }
  | "->"      { ARROW }
  | ":"       { COLON }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | "["       { LBRACKET }
  | "]"       { RBRACKET }
  | "\\"      { SLASH }
  | "."       { DOT }
  | "+"       { PLUS }
  | num as n  { NUMBER (int_of_string n) }
  | sym as x  { SYMBOL x }
  | eof       { EOF }
  | _         { raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

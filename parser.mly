%token <int> NUMBER
%token <string> SYMBOL
%token ARROW COLON LBRACKET RBRACKET
%token LPAREN RPAREN SLASH DOT
%token NUM PLUS
%token EOF

%right ARROW

%start <Ast.HExp.t> top
%start <Ast.HTyp.t> typ_top

%{

    open Ast.HExp;;
    open Ast.HTyp;;

%}

%%

top:
  | e=exp EOF  { e }

typ_top:
  | t=typ EOF  { t }

exp:
  | SLASH x=SYMBOL DOT e=exp  { Fun(x, e) }
  | e1=appexp PLUS e2=exp     { Add(e1, e2) }
  | LBRACKET RBRACKET         { Hol(None) }
  | LBRACKET e=exp RBRACKET   { Hol(Some(e)) }
  | appexp                    { $1 }

appexp:
  | e1=appexp e2=atom  { App(e1, e2) }
  | atom               { $1 }

atom:
  | e=atom COLON t=typ    { Asc(e, t) }
  | x=SYMBOL              { Var(x) }
  | n=NUMBER              { Num(n) }
  | LPAREN exp RPAREN     { $2 }

typ:
  | NUM                  { TNum }
  | t1=typ ARROW t2=typ  { TFun(t1, t2) }
  | LBRACKET RBRACKET    { THol }
  | LPAREN typ RPAREN    { $2 }

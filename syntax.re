/******************************************************************************/
/* Printers and Parsers                                                       */
/******************************************************************************/

open Ast.HExp;
open Ast.HTyp;

open Format;

/******************************************************************************/
/* Types                                                                      */
/******************************************************************************/

/* Parse */

let parse_type = str => {
  let lexbuf = Lexing.from_string(str);
  try(Some(Parser.typ_top(Lexer.read, lexbuf))) {
  | Lexer.Error(_)
  | Parser.Error => None
  };
};

/* Print */

let rec pp_type = (fmt, t) =>
  switch (t) {
  | TFun(t1, t2) => fprintf(fmt, "%a -> %a", sub_pp_type, t1, pp_type, t2)
  | TNum => fprintf(fmt, "num")
  | THol => fprintf(fmt, "[]")
  }

and sub_pp_type = (fmt, t) =>
  switch (t) {
  | TFun(t1, t2) => fprintf(fmt, "(%a -> %a)", sub_pp_type, t1, pp_type, t2)
  | TNum
  | THol => pp_type(fmt, t)
  };

/******************************************************************************/
/* Expressions                                                                */
/******************************************************************************/

/* Parse */

let parse_expr = str => {
  let lexbuf = Lexing.from_string(str);
  try(Some(Parser.top(Lexer.read, lexbuf))) {
  | Lexer.Error(_)
  | Parser.Error => None
  };
};

/* Print */

let rec pp_expr = (fmt, e) =>
  switch (e) {
  | Var(x) => fprintf(fmt, "%s", x)
  | Fun(x, e1) => fprintf(fmt, "\\%s.%a", x, pp_expr, e1)
  | App(e1, e2) => fprintf(fmt, "%a %a", app_pp_expr, e1, sub_pp_expr, e2)
  | Num(n) => fprintf(fmt, "%d", n)
  | Add(e1, e2) => fprintf(fmt, "%a + %a", app_pp_expr, e1, sub_pp_expr, e2)
  | Ann(e1, t2) => fprintf(fmt, "%a : %a", sub_pp_expr, e1, pp_type, t2)
  | Hol(None) => fprintf(fmt, "[]")
  | Hol(Some(e1)) => fprintf(fmt, "[%a]", pp_expr, e1)
  }

and app_pp_expr = (fmt, e) =>
  switch (e) {
  | Fun(_, _)
  | Ann(_, _) => sub_pp_expr(fmt, e)
  | App(e1, e2) => fprintf(fmt, "%a %a", app_pp_expr, e1, sub_pp_expr, e2)
  | Add(e1, e2) => fprintf(fmt, "%a + %a", app_pp_expr, e1, sub_pp_expr, e2)
  | _ => pp_expr(fmt, e)
  }

and sub_pp_expr = (fmt, e) =>
  switch (e) {
  | Fun(_, _)
  | App(_, _)
  | Add(_, _)
  | Ann(_, _) => fprintf(fmt, "(%a)", pp_expr, e)
  | _ => pp_expr(fmt, e)
  };

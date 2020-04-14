/******************************************************************************/
/* Parser Unit Tests                                                          */
/******************************************************************************/

open Hzcalc.Syntax;

/******************************************************************************/
/* parse_type                                                                 */
/******************************************************************************/

let htyp = Alcotest.testable(pp_type, (==));

let test_parse_type_failure = () =>
  Alcotest.(check @@ option(htyp))(
    "parse_type failure",
    None,
    parse_type("!!!"),
  );

/* TNum */

let test_parse_type_num = () =>
  Alcotest.(check @@ option(htyp))(
    "parse_type num",
    Some(TNum),
    parse_type("num"),
  );

/* TFun */

let test_parse_type_arrow = () =>
  Alcotest.(check @@ option(htyp))(
    "parse_type arrow",
    Some(TFun(TNum, TNum)),
    parse_type("num -> num"),
  );

let test_parse_type_arrows = () =>
  Alcotest.(check @@ option(htyp))(
    "parse_type arrows",
    Some(TFun(TNum, TFun(TNum, TNum))),
    parse_type("num -> num -> num"),
  );

let test_parse_type_arrows_left = () =>
  Alcotest.(check @@ option(htyp))(
    "parse_type arrows left",
    Some(TFun(TFun(TNum, TNum), TNum)),
    parse_type("(num -> num) -> num"),
  );

let test_parse_type_arrows_right = () =>
  Alcotest.(check @@ option(htyp))(
    "parse_type arrows right",
    Some(TFun(TNum, TFun(TNum, TNum))),
    parse_type("num -> (num -> num)"),
  );

/* THol */

let test_parse_type_hole = () =>
  Alcotest.(check @@ option(htyp))(
    "parse_type hole",
    Some(THol),
    parse_type("[]"),
  );

/******************************************************************************/
/* parse_expr                                                                 */
/******************************************************************************/

let hexp = Alcotest.testable(pp_expr, (==));

let test_parse_expr_failure = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr failure",
    None,
    parse_expr("!!!"),
  );

/* Var */

let test_parse_expr_var = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr var",
    Some(Var("x")),
    parse_expr("x"),
  );

/* Fun */

let test_parse_expr_fun = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr fun",
    Some(Fun("x", Var("x"))),
    parse_expr("\\x.x"),
  );

let test_parse_expr_fun_app = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr fun app",
    Some(Fun("x", App(Var("x"), Var("x")))),
    parse_expr("\\x.x x"),
  );

let test_parse_expr_funs = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr funs",
    Some(Fun("x", Fun("y", Var("x")))),
    parse_expr("\\x.\\y.x"),
  );

let test_parse_expr_funs_app = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr funs app",
    Some(Fun("f", Fun("x", App(Var("f"), Var("x"))))),
    parse_expr("\\f.\\x.f x"),
  );

/* App */

let test_parse_expr_app = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr app",
    Some(App(Var("f"), Var("x"))),
    parse_expr("f x"),
  );

let test_parse_expr_apps = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr apps",
    Some(App(App(Var("f"), Var("x")), Var("y"))),
    parse_expr("f x y"),
  );

let test_parse_expr_apps_left = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr apps left",
    Some(App(App(Var("f"), Var("x")), Var("y"))),
    parse_expr("(f x) y"),
  );

let test_parse_expr_apps_right = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr apps right",
    Some(App(Var("f"), App(Var("x"), Var("y")))),
    parse_expr("f (x y)"),
  );

/* Num */

let test_parse_expr_num_zero = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr num zero",
    Some(Num(0)),
    parse_expr("0"),
  );

let test_parse_expr_num_positive = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr num positive",
    Some(Num(123)),
    parse_expr("123"),
  );

let test_parse_expr_num_negative = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr num negative",
    Some(Num(-456)),
    parse_expr("-456"),
  );

/* Add */

let test_parse_expr_add = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr add",
    Some(Add(Num(1), Num(2))),
    parse_expr("1 + 2"),
  );

let test_parse_expr_adds = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr adds",
    Some(Add(Num(1), Add(Num(2), Num(3)))),
    parse_expr("1 + 2 + 3"),
  );

let test_parse_expr_adds_left = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr adds",
    Some(Add(Add(Num(1), Num(2)), Num(3))),
    parse_expr("(1 + 2) + 3"),
  );

let test_parse_expr_adds_right = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr adds right",
    Some(Add(Num(1), Add(Num(2), Num(3)))),
    parse_expr("1 + (2 + 3)"),
  );

/* Asc */

let test_parse_expr_asc_var = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr ascribe var",
    Some(Asc(Var("x"), TNum)),
    parse_expr("x : num"),
  );

let test_parse_expr_asc_fun = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr ascribe fun",
    Some(Asc(Fun("x", Var("x")), TFun(TNum, TNum))),
    parse_expr("(\\x.x) : num -> num"),
  );

let test_parse_expr_asc_in_fun = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr ascribe in fun",
    Some(Fun("f", Asc(Var("f"), TFun(TNum, TNum)))),
    parse_expr("\\f.f : num -> num"),
  );

/* Hol */

let test_parse_expr_hole_empty = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr hole empty",
    Some(Hol(None)),
    parse_expr("[]"),
  );

let test_parse_expr_hole_full = () =>
  Alcotest.(check @@ option(hexp))(
    "parse_expr hole full",
    Some(Hol(Some(Num(1)))),
    parse_expr("[1]"),
  );

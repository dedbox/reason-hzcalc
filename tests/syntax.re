/******************************************************************************/
/* Parser Unit Tests                                                          */
/******************************************************************************/

open Bdcalc.Ast.HExp;
open Bdcalc.Syntax;

/******************************************************************************/
/* parse_type                                                                 */
/******************************************************************************/

let test_parse_type_failure = () =>
  Alcotest.(check(bool))(
    "parse_type failure",
    true,
    parse_type("!!!") == None,
  );

/* TNum */

let test_parse_type_num = () =>
  Alcotest.(check(bool))(
    "parse_type num",
    true,
    parse_type("num") == Some(TNum),
  );

/* TFun */

let test_parse_type_arrow = () =>
  Alcotest.(check(bool))(
    "parse_type arrow",
    true,
    parse_type("num -> num") == Some(TFun(TNum, TNum)),
  );

let test_parse_type_arrows = () =>
  Alcotest.(check(bool))(
    "parse_type arrows",
    true,
    parse_type("num -> num -> num") == Some(TFun(TNum, TFun(TNum, TNum))),
  );

let test_parse_type_arrows_left = () =>
  Alcotest.(check(bool))(
    "parse_type arrows left",
    true,
    parse_type("(num -> num) -> num") == Some(TFun(TFun(TNum, TNum), TNum)),
  );

let test_parse_type_arrows_right = () =>
  Alcotest.(check(bool))(
    "parse_type arrows right",
    true,
    parse_type("num -> (num -> num)") == Some(TFun(TNum, TFun(TNum, TNum))),
  );

/* THol */

let test_parse_type_hole = () =>
  Alcotest.(check(bool))(
    "parse_type hole",
    true,
    parse_type("[]") == Some(THol),
  );

/******************************************************************************/
/* parse_expr                                                                 */
/******************************************************************************/

let test_parse_expr_failure = () =>
  Alcotest.(check(bool))(
    "parse_expr failure",
    true,
    parse_expr("!!!") == None,
  );

/* Var */

let test_parse_expr_var = () =>
  Alcotest.(check(bool))(
    "parse_expr var",
    true,
    parse_expr("x") == Some(Var("x")),
  );

/* Fun */

let test_parse_expr_fun = () =>
  Alcotest.(check(bool))(
    "parse_expr fun",
    true,
    parse_expr("\\x.x") == Some(Fun("x", Var("x"))),
  );

let test_parse_expr_fun_app = () =>
  Alcotest.(check(bool))(
    "parse_expr fun app",
    true,
    parse_expr("\\x.x x") == Some(Fun("x", App(Var("x"), Var("x")))),
  );

let test_parse_expr_funs = () =>
  Alcotest.(check(bool))(
    "parse_expr funs",
    true,
    parse_expr("\\x.\\y.x") == Some(Fun("x", Fun("y", Var("x")))),
  );

let test_parse_expr_funs_app = () =>
  Alcotest.(check(bool))(
    "parse_expr funs app",
    true,
    parse_expr("\\f.\\x.f x")
    == Some(Fun("f", Fun("x", App(Var("f"), Var("x"))))),
  );

/* App */

let test_parse_expr_app = () =>
  Alcotest.(check(bool))(
    "parse_expr app",
    true,
    parse_expr("f x") == Some(App(Var("f"), Var("x"))),
  );

let test_parse_expr_apps = () =>
  Alcotest.(check(bool))(
    "parse_expr apps",
    true,
    parse_expr("f x y") == Some(App(App(Var("f"), Var("x")), Var("y"))),
  );

let test_parse_expr_apps_left = () =>
  Alcotest.(check(bool))(
    "parse_expr apps left",
    true,
    parse_expr("(f x) y")
    == Some(App(App(Var("f"), Var("x")), Var("y"))),
  );

let test_parse_expr_apps_right = () =>
  Alcotest.(check(bool))(
    "parse_expr apps right",
    true,
    parse_expr("f (x y)")
    == Some(App(Var("f"), App(Var("x"), Var("y")))),
  );

/* Num */

let test_parse_expr_num_zero = () =>
  Alcotest.(check(bool))(
    "parse_expr num zero",
    true,
    parse_expr("0") == Some(Num(0)),
  );

let test_parse_expr_num_positive = () =>
  Alcotest.(check(bool))(
    "parse_expr num positive",
    true,
    parse_expr("123") == Some(Num(123)),
  );

let test_parse_expr_num_negative = () =>
  Alcotest.(check(bool))(
    "parse_expr num negative",
    true,
    parse_expr("-456") == Some(Num(-456)),
  );

/* Add */

let test_parse_expr_add = () =>
  Alcotest.(check(bool))(
    "parse_expr add",
    true,
    parse_expr("1 + 2") == Some(Add(Num(1), Num(2))),
  );

let test_parse_expr_adds = () =>
  Alcotest.(check(bool))(
    "parse_expr adds",
    true,
    parse_expr("1 + 2 + 3") == Some(Add(Num(1), Add(Num(2), Num(3)))),
  );

let test_parse_expr_adds_left = () =>
  Alcotest.(check(bool))(
    "parse_expr adds",
    true,
    parse_expr("(1 + 2) + 3") == Some(Add(Add(Num(1), Num(2)), Num(3))),
  );

let test_parse_expr_adds_right = () =>
  Alcotest.(check(bool))(
    "parse_expr adds right",
    true,
    parse_expr("1 + (2 + 3)") == Some(Add(Num(1), Add(Num(2), Num(3)))),
  );

/* Ann */

let test_parse_expr_ann_var = () =>
  Alcotest.(check(bool))(
    "parse_expr annotate var",
    true,
    parse_expr("x : num") == Some(Ann(Var("x"), TNum)),
  );

let test_parse_expr_ann_fun = () =>
  Alcotest.(check(bool))(
    "parse_expr annotate fun",
    true,
    parse_expr("(\\x.x) : num -> num")
    == Some(Ann(Fun("x", Var("x")), TFun(TNum, TNum))),
  );

let test_parse_expr_ann_in_fun = () =>
  Alcotest.(check(bool))(
    "parse_expr annotate in fun",
    true,
    parse_expr("\\f.f : num -> num")
    == Some(Fun("f", Ann(Var("f"), TFun(TNum, TNum)))),
  );

/* Hol */

let test_parse_expr_hole_empty = () =>
  Alcotest.(check(bool))(
    "parse_expr hole empty",
    true,
    parse_expr("[]") == Some(Hol(None)),
  );

let test_parse_expr_hole_full = () =>
  Alcotest.(check(bool))(
    "parse_expr hole full",
    true,
    parse_expr("[1]") == Some(Hol(Some(Num(1)))),
  );

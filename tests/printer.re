/******************************************************************************/
/* Printer Unit Tests                                                         */
/******************************************************************************/

open Bdcalc.Ast.HTyp;
open Bdcalc.Syntax;
open Format;

/******************************************************************************/
/* pp_type                                                                    */
/******************************************************************************/

/* TNum */

let test_pp_type_num = () =>
  Alcotest.(check(string))(
    "pp_type num",
    asprintf("%a", pp_type, TNum),
    "num",
  );

/* TFun */

let test_pp_type_arrow = () =>
  Alcotest.(check(string))(
    "pp_type arrow",
    asprintf("%a", pp_type, TFun(TNum, TNum)),
    "num -> num",
  );

let test_pp_type_arrows_left = () =>
  Alcotest.(check(string))(
    "pp_type arrows left",
    asprintf("%a", pp_type, TFun(TFun(TNum, TNum), TNum)),
    "(num -> num) -> num",
  );

let test_pp_type_arrows_right = () =>
  Alcotest.(check(string))(
    "pp_type arrows right",
    asprintf("%a", pp_type, TFun(TNum, TFun(TNum, TNum))),
    "num -> num -> num",
  );

/* THol */

let test_pp_type_hole = () =>
  Alcotest.(check(string))(
    "pp_type hole",
    asprintf("%a", pp_type, THol),
    "[]",
  );

/******************************************************************************/
/* pp_expr                                                                    */
/******************************************************************************/

/* Var */

let test_pp_expr_var = () =>
  Alcotest.(check(string))(
    "pp_expr var",
    asprintf("%a", pp_expr, Var("x")),
    "x",
  );

/* Fun */

let test_pp_expr_fun = () =>
  Alcotest.(check(string))(
    "pp_expr fun",
    asprintf("%a", pp_expr, Fun("x", Var("x"))),
    "\\x.x",
  );

let test_pp_expr_fun_app = () =>
  Alcotest.(check(string))(
    "pp_expr fun app",
    asprintf("%a", pp_expr, Fun("x", App(Var("x"), Var("x")))),
    "\\x.x x",
  );

let test_pp_expr_funs = () =>
  Alcotest.(check(string))(
    "pp_expr funs",
    asprintf("%a", pp_expr, Fun("x", Fun("y", Var("x")))),
    "\\x.\\y.x",
  );

let test_pp_expr_funs_app = () =>
  Alcotest.(check(string))(
    "pp_expr funs app",
    asprintf("%a", pp_expr, Fun("f", Fun("x", App(Var("f"), Var("x"))))),
    "\\f.\\x.f x",
  );

/* App */

let test_pp_expr_app = () =>
  Alcotest.(check(string))(
    "pp_expr app",
    asprintf("%a", pp_expr, App(Var("f"), Var("x"))),
    "f x",
  );

let test_pp_expr_apps_left = () =>
  Alcotest.(check(string))(
    "pp_expr apps left",
    asprintf("%a", pp_expr, App(App(Var("f"), Var("x")), Var("y"))),
    "f x y",
  );

let test_pp_expr_apps_right = () =>
  Alcotest.(check(string))(
    "pp_expr apps right",
    asprintf("%a", pp_expr, App(Var("f"), App(Var("x"), Var("y")))),
    "f (x y)",
  );

/* Num */

let test_pp_expr_num_zero = () =>
  Alcotest.(check(string))(
    "pp_expr num zero",
    asprintf("%a", pp_expr, Num(0)),
    "0",
  );

let test_pp_expr_num_positive = () =>
  Alcotest.(check(string))(
    "pp_expr num positive",
    asprintf("%a", pp_expr, Num(123)),
    "123",
  );

let test_pp_expr_num_negative = () =>
  Alcotest.(check(string))(
    "pp_expr num negative",
    asprintf("%a", pp_expr, Num(-456)),
    "-456",
  );

/* Add */

let test_pp_expr_add = () =>
  Alcotest.(check(string))(
    "pp_expr add",
    asprintf("%a", pp_expr, Add(Num(1), Num(2))),
    "1 + 2",
  );

let test_pp_expr_adds_left = () =>
  Alcotest.(check(string))(
    "pp_expr adds left",
    asprintf("%a", pp_expr, Add(Add(Num(1), Num(2)), Num(3))),
    "1 + 2 + 3",
  );

let test_pp_expr_adds_right = () =>
  Alcotest.(check(string))(
    "pp_expr adds right",
    asprintf("%a", pp_expr, Add(Num(1), Add(Num(2), Num(3)))),
    "1 + (2 + 3)",
  );

/* Ann */

let test_pp_expr_ann_var = () =>
  Alcotest.(check(string))(
    "pp_expr annotate var",
    asprintf("%a", pp_expr, Ann(Var("x"), TNum)),
    "x : num",
  );

let test_pp_expr_ann_fun = () =>
  Alcotest.(check(string))(
    "pp_expr annotate fun",
    asprintf("%a", pp_expr, Ann(Fun("x", Var("x")), TFun(TNum, TNum))),
    "(\\x.x) : num -> num",
  );

let test_pp_expr_ann_in_fun = () =>
  Alcotest.(check(string))(
    "pp_expr annotate in fun",
    asprintf("%a", pp_expr, Fun("f", Ann(Var("f"), TFun(TNum, TNum)))),
    "\\f.f : num -> num",
  );

/* Hol */

let test_pp_expr_hole_empty = () =>
  Alcotest.(check(string))(
    "pp_expr hole empty",
    asprintf("%a", pp_expr, Hol(None)),
    "[]",
  );

let test_pp_expr_hole_full = () =>
  Alcotest.(check(string))(
    "pp_expr hole full",
    asprintf("%a", pp_expr, Hol(Some(Num(1)))),
    "[1]",
  );

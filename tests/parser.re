/******************************************************************************/
/* Parser Unit Tests                                                          */
/******************************************************************************/

open Alcotest;
open Bdcalc.Ast.HExp;
open Bdcalc.Syntax;

/******************************************************************************/
/* parse_type                                                                 */
/******************************************************************************/

let htyp = Alcotest.testable(pp_type, (==));

let test_parse_type_failure = () =>
  Alcotest.(check @@ option(htyp))(
    "parse_type failure",
    parse_type("!!!"),
    None,
  );

/* TNum */

let test_parse_type_num = () =>
  Alcotest.(check(option(htyp)))(
    "parse_type num",
    parse_type("num"),
    Some(TNum),
  );

/* TFun */

let test_parse_type_arrow = () =>
  Alcotest.(check(option(htyp)))(
    "parse_type arrow",
    parse_type("num -> num"),
    Some(TFun(TNum, TNum)),
  );

let test_parse_type_arrows = () =>
  Alcotest.(check(option(htyp)))(
    "parse_type arrows",
    parse_type("num -> num -> num"),
    Some(TFun(TNum, TFun(TNum, TNum))),
  );

let test_parse_type_arrows_left = () =>
  Alcotest.(check(option(htyp)))(
    "parse_type arrows left",
    parse_type("(num -> num) -> num"),
    Some(TFun(TFun(TNum, TNum), TNum)),
  );

let test_parse_type_arrows_right = () =>
  Alcotest.(check(option(htyp)))(
    "parse_type arrows right",
    parse_type("num -> (num -> num)"),
    Some(TFun(TNum, TFun(TNum, TNum))),
  );

/* THol */

let test_parse_type_hole = () =>
  Alcotest.(check(option(htyp)))(
    "parse_type hole",
    parse_type("[]"),
    Some(THol),
  );

/******************************************************************************/
/* parse_expr                                                                 */
/******************************************************************************/

let hexp = Alcotest.testable(pp_expr, (==));

let test_parse_expr_failure = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr failure",
    parse_expr("!!!"),
    None,
  );

/* Var */

let test_parse_expr_var = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr var",
    parse_expr("x"),
    Some(Var("x")),
  );

/* Fun */

let test_parse_expr_fun = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr fun",
    parse_expr("\\x.x"),
    Some(Fun("x", Var("x"))),
  );

let test_parse_expr_fun_app = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr fun app",
    parse_expr("\\x.x x"),
    Some(Fun("x", App(Var("x"), Var("x")))),
  );

let test_parse_expr_funs = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr funs",
    parse_expr("\\x.\\y.x"),
    Some(Fun("x", Fun("y", Var("x")))),
  );

let test_parse_expr_funs_app = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr funs app",
    parse_expr("\\f.\\x.f x"),
    Some(Fun("f", Fun("x", App(Var("f"), Var("x"))))),
  );

/* App */

let test_parse_expr_app = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr app",
    parse_expr("f x"),
    Some(App(Var("f"), Var("x"))),
  );

let test_parse_expr_apps = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr apps",
    parse_expr("f x y"),
    Some(App(App(Var("f"), Var("x")), Var("y"))),
  );

let test_parse_expr_apps_left = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr apps left",
    parse_expr("(f x) y"),
    Some(App(App(Var("f"), Var("x")), Var("y"))),
  );

let test_parse_expr_apps_right = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr apps right",
    parse_expr("f (x y)"),
    Some(App(Var("f"), App(Var("x"), Var("y")))),
  );

/* Num */

let test_parse_expr_num_zero = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr num zero",
    parse_expr("0"),
    Some(Num(0)),
  );

let test_parse_expr_num_positive = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr num positive",
    parse_expr("123"),
    Some(Num(123)),
  );

let test_parse_expr_num_negative = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr num negative",
    parse_expr("-456"),
    Some(Num(-456)),
  );

/* Add */

let test_parse_expr_add = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr add",
    parse_expr("1 + 2"),
    Some(Add(Num(1), Num(2))),
  );

let test_parse_expr_adds = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr adds",
    parse_expr("1 + 2 + 3"),
    Some(Add(Num(1), Add(Num(2), Num(3)))),
  );

let test_parse_expr_adds_left = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr adds",
    parse_expr("(1 + 2) + 3"),
    Some(Add(Add(Num(1), Num(2)), Num(3))),
  );

let test_parse_expr_adds_right = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr adds right",
    parse_expr("1 + (2 + 3)"),
    Some(Add(Num(1), Add(Num(2), Num(3)))),
  );

/* Ann */

let test_parse_expr_ann_var = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr annotate var",
    parse_expr("x : num"),
    Some(Ann(Var("x"), TNum)),
  );

let test_parse_expr_ann_fun = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr annotate fun",
    parse_expr("(\\x.x) : num -> num"),
    Some(Ann(Fun("x", Var("x")), TFun(TNum, TNum))),
  );

let test_parse_expr_ann_in_fun = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr annotate in fun",
    parse_expr("\\f.f : num -> num"),
    Some(Fun("f", Ann(Var("f"), TFun(TNum, TNum)))),
  );

/* Hol */

let test_parse_expr_hole_empty = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr hole empty",
    parse_expr("[]"),
    Some(Hol(None)),
  );

let test_parse_expr_hole_full = () =>
  Alcotest.(check(option(hexp)))(
    "parse_expr hole full",
    parse_expr("[1]"),
    Some(Hol(Some(Num(1)))),
  );

/******************************************************************************/

let tests = [
  (
    "parse_type",
    [
      test_case("failure", `Quick, test_parse_type_failure),
      test_case("num", `Quick, test_parse_type_num),
      test_case("arrow", `Quick, test_parse_type_arrow),
      test_case("arrows", `Quick, test_parse_type_arrows),
      test_case("arrows left", `Quick, test_parse_type_arrows_left),
      test_case("arrows right", `Quick, test_parse_type_arrows_right),
      test_case("hole", `Quick, test_parse_type_hole),
    ],
  ),
  (
    "parse_expr",
    [
      test_case("failure", `Quick, test_parse_expr_failure),
      test_case("var", `Quick, test_parse_expr_var),
      test_case("fun", `Quick, test_parse_expr_fun),
      test_case("fun app", `Quick, test_parse_expr_fun_app),
      test_case("funs", `Quick, test_parse_expr_funs),
      test_case("funs app", `Quick, test_parse_expr_funs_app),
      test_case("app", `Quick, test_parse_expr_app),
      test_case("apps", `Quick, test_parse_expr_apps),
      test_case("apps left", `Quick, test_parse_expr_apps_left),
      test_case("apps right", `Quick, test_parse_expr_apps_right),
      test_case("num zero", `Quick, test_parse_expr_num_zero),
      test_case("num positive", `Quick, test_parse_expr_num_positive),
      test_case("num negative", `Quick, test_parse_expr_num_negative),
      test_case("add", `Quick, test_parse_expr_add),
      test_case("adds", `Quick, test_parse_expr_adds),
      test_case("adds left", `Quick, test_parse_expr_adds_left),
      test_case("adds right", `Quick, test_parse_expr_adds_right),
      test_case("annotate var", `Quick, test_parse_expr_ann_var),
      test_case("annotate fun", `Quick, test_parse_expr_ann_fun),
      test_case("annotate in fun", `Quick, test_parse_expr_ann_in_fun),
      test_case("hole empty", `Quick, test_parse_expr_hole_empty),
      test_case("hole full", `Quick, test_parse_expr_hole_full),
    ],
  ),
];

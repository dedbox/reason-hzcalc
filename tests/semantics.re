/******************************************************************************/
/* Static Semantics Unit Tests                                                */
/******************************************************************************/

open Hzcalc.Ast.HTyp;
open Hzcalc.Context;
open Hzcalc.Semantics;
open Hzcalc.Syntax;

/******************************************************************************/
/* synthesize                                                                 */
/******************************************************************************/

let htyp = Alcotest.testable(pp_type, (==));

/* Var */

let test_synthesize_var_fail = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize var fail",
    None,
    Context.empty |> synthesize(Var("x")),
  );

let test_synthesize_var = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize var",
    Some(TNum),
    Context.(empty |> extend("x", TNum)) |> synthesize(Var("x")),
  );

/* App */

let test_synthesize_app_fail = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize app fail",
    None,
    Context.empty |> synthesize(App(Var("f"), Var("x"))),
  );

let test_synthesize_app_fun_fail1 = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize app fun fail1",
    None,
    Context.(empty |> extend("f", TFun(TNum, TNum)))
    |> synthesize(App(Var("f"), Var("x"))),
  );

let test_synthesize_app_fun_fail2 = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize app fun fail2",
    None,
    Context.(
      empty
      |> extend("f", TFun(TNum, TNum))
      |> extend("x", TFun(TNum, TNum))
    )
    |> synthesize(App(Var("f"), Var("x"))),
  );

let test_synthesize_app_fun = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize app fun",
    Some(TNum),
    Context.(empty |> extend("f", TFun(TNum, TNum)) |> extend("x", TNum))
    |> synthesize(App(Var("f"), Var("x"))),
  );

let test_synthesize_app_fun_hole = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize app fun hole",
    Some(TNum),
    Context.(empty |> extend("f", TFun(TNum, TNum)) |> extend("x", THol))
    |> synthesize(App(Var("f"), Var("x"))),
  );

let test_synthesize_app_fun_from_hole = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize app fun from hole",
    Some(TNum),
    Context.(empty |> extend("f", TFun(THol, TNum)) |> extend("x", TNum))
    |> synthesize(App(Var("f"), Var("x"))),
  );

let test_synthesize_app_fun_to_hole = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize app fun to hole",
    Some(THol),
    Context.(empty |> extend("f", TFun(TNum, THol)) |> extend("x", TNum))
    |> synthesize(App(Var("f"), Var("x"))),
  );

let test_synthesize_app_fun_holes = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize app fun holes",
    Some(THol),
    Context.(empty |> extend("f", TFun(THol, THol)) |> extend("x", TNum))
    |> synthesize(App(Var("f"), Var("x"))),
  );

let test_synthesize_app_fun_holes_on_hole = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize app fun holes on hole",
    Some(THol),
    Context.(empty |> extend("f", TFun(THol, THol)) |> extend("x", THol))
    |> synthesize(App(Var("f"), Var("x"))),
  );

let test_synthesize_app_hole_fail = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize app hole fail",
    None,
    Context.(empty |> extend("f", THol))
    |> synthesize(App(Var("f"), Var("x"))),
  );

let test_synthesize_app_hole = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize app hole",
    Some(THol),
    Context.(empty |> extend("f", THol) |> extend("x", TNum))
    |> synthesize(App(Var("f"), Var("x"))),
  );

let test_synthesize_app_holes = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize app holes",
    Some(THol),
    Context.(empty |> extend("f", THol) |> extend("x", THol))
    |> synthesize(App(Var("f"), Var("x"))),
  );

/* Num */

let test_synthesize_num = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize num",
    Some(TNum),
    Context.empty |> synthesize(Num(123)),
  );

/* Add */

let test_synthesize_add_fail1 = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize add fail1",
    None,
    Context.empty |> synthesize(Add(Var("n"), Num(123))),
  );

let test_synthesize_add_fail2 = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize add fail2",
    None,
    Context.empty |> synthesize(Add(Num(123), Var("n"))),
  );

let test_synthesize_add = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize add",
    Some(TNum),
    Context.empty |> synthesize(Add(Num(123), Num(456))),
  );

let test_synthesize_adds_left = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize adds left",
    Some(TNum),
    Context.empty |> synthesize(Add(Add(Num(12), Num(34)), Num(56))),
  );

let test_synthesize_adds_right = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize adds right",
    Some(TNum),
    Context.empty |> synthesize(Add(Num(12), Add(Num(34), Num(56)))),
  );

/* Asc */

/* ??? */
let test_synthesize_asc_fail = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize asc fail",
    None,
    Context.empty |> synthesize(Asc(Var("x"), TNum)),
  );

let test_synthesize_asc = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize asc",
    Some(TFun(TNum, TNum)),
    Context.empty |> synthesize(Asc(Fun("x", Var("x")), TFun(TNum, TNum))),
  );

/* Hol */

let test_synthesize_hole = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize hole",
    Some(THol),
    Context.empty |> synthesize(Hol(None)),
  );

let test_synthesize_hole_var = () =>
  Alcotest.(check @@ option(htyp))(
    "synthesize hole none",
    Some(THol),
    Context.(empty |> extend("x", TNum))
    |> synthesize(Hol(Some(Var("x")))),
  );

/******************************************************************************/
/* analyze                                                                    */
/******************************************************************************/

/* Var */

let test_analyze_var_fail = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze var fail",
    None,
    Context.empty |> analyze(Var("x"), TNum),
  );

/* ??? */
let test_analyze_var_hole = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze var hole",
    None,
    Context.empty |> analyze(Var("x"), THol),
  );

/* App */

let test_analyze_app_fail = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze app fail",
    None,
    Context.empty |> analyze(App(Var("f"), Var("x")), TNum),
  );

/* Fun */

let test_analyze_fun_fail = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze fun fail",
    None,
    Context.empty
    |> analyze(Fun("x", Fun("y", Var("x"))), TFun(TNum, TNum)),
  );

let test_analyze_fun = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze fun",
    Some(TFun(TNum, TNum)),
    Context.empty |> analyze(Fun("x", Var("x")), TFun(TNum, TNum)),
  );

let test_analyze_fun_hole = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze fun hole",
    Some(THol),
    Context.empty |> analyze(Fun("x", Var("x")), THol),
  );

let test_analyze_fun_hole_left = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze fun hole left",
    Some(TFun(THol, TNum)),
    Context.empty |> analyze(Fun("x", Var("x")), TFun(THol, TNum)),
  );

let test_analyze_fun_hole_right = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze fun hole right",
    Some(TFun(TNum, THol)),
    Context.empty |> analyze(Fun("x", Var("x")), TFun(TNum, THol)),
  );

let test_analyze_fun_holes = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze fun holes",
    Some(TFun(THol, THol)),
    Context.empty |> analyze(Fun("x", Var("x")), TFun(THol, THol)),
  );

let test_analyze_funs_left = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze funs left",
    Some(TFun(TFun(TNum, TNum), TFun(TNum, TNum))),
    Context.empty
    |> analyze(
         Fun("f", Fun("x", App(Var("f"), Var("x")))),
         TFun(TFun(TNum, TNum), TFun(TNum, TNum)),
       ),
  );

let test_analyze_funs_right = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze funs right",
    Some(TFun(TNum, TFun(TNum, TNum))),
    Context.empty
    |> analyze(
         Fun("x", Fun("y", Var("x"))),
         TFun(TNum, TFun(TNum, TNum)),
       ),
  );

/* Num */

let test_analyze_num_fail = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze num fail",
    None,
    Context.empty |> analyze(Num(123), TFun(TNum, TNum)),
  );

let test_analyze_num = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze num",
    Some(TNum),
    Context.empty |> analyze(Num(123), TNum),
  );

let test_analyze_num_hole = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze num hole",
    Some(THol),
    Context.empty |> analyze(Num(123), THol),
  );

/* Add */

let test_analyze_add_fail = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze add fail",
    None,
    Context.empty |> analyze(Add(Var("n"), Var("m")), TFun(TNum, TNum)),
  );

let test_analyze_add = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze add",
    Some(TNum),
    Context.empty |> analyze(Add(Num(123), Num(456)), TNum),
  );

let test_analyze_adds_left = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze adds left",
    Some(TNum),
    Context.empty |> analyze(Add(Add(Num(12), Num(34)), Num(56)), TNum),
  );

let test_analyze_adds_right = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze adds right",
    Some(TNum),
    Context.empty |> analyze(Add(Num(12), Add(Num(34), Num(56))), TNum),
  );

/* Asc */

let test_analyze_asc_fail = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze asc fail",
    None,
    Context.empty |> analyze(Asc(Var("x"), TNum), TNum),
  );

let test_analyze_asc = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze asc",
    Some(TNum),
    Context.(empty |> extend("x", TNum))
    |> analyze(Asc(Var("x"), TNum), TNum),
  );

let test_analyze_asc_hole = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze asc hole",
    Some(THol),
    Context.(empty |> extend("x", TNum))
    |> analyze(Asc(Var("x"), THol), THol),
  );

let test_analyze_asc_on_hole = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze asc hole",
    Some(THol),
    Context.(empty |> extend("x", TNum))
    |> analyze(Asc(Var("x"), TNum), THol),
  );

/* Hol */

let test_analyze_hole_fail = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze hole fail",
    None,
    Context.empty |> analyze(Hol(Some(Var("x"))), THol),
  );

let test_analyze_hole_none = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze hole none",
    Some(THol),
    Context.empty |> analyze(Hol(None), THol),
  );

let test_analyze_hole_some = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze hole some",
    Some(THol),
    Context.(empty |> extend("x", TNum))
    |> analyze(Hol(Some(Var("x"))), THol),
  );

let test_analyze_hole_on_hole = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze hole on hole",
    Some(THol),
    Context.empty |> analyze(Hol(Some(Hol(None))), THol),
  );

let test_analyze_hole_on_fun = () =>
  Alcotest.(check @@ option(htyp))(
    "analyze hole on fun",
    Some(TNum),
    Context.(empty |> extend("f", TFun(TNum, TNum)))
    |> analyze(Hol(Some(Var("f"))), TNum),
  );

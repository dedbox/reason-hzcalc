/******************************************************************************/
/* Unit Tests                                                                 */
/******************************************************************************/

open Parser;
open Printer;
open Semantics;

let () =
  Alcotest.(
    run(
      "bdcalc_tests",
      [
        /* parser */
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
            test_case("ascribe var", `Quick, test_parse_expr_asc_var),
            test_case("ascribe fun", `Quick, test_parse_expr_asc_fun),
            test_case("ascribe in fun", `Quick, test_parse_expr_asc_in_fun),
            test_case("hole empty", `Quick, test_parse_expr_hole_empty),
            test_case("hole full", `Quick, test_parse_expr_hole_full),
          ],
        ),
        /* printer */
        (
          "pp_type",
          [
            test_case("num", `Quick, test_pp_type_num),
            test_case("arrow", `Quick, test_pp_type_arrow),
            test_case("arrows left", `Quick, test_pp_type_arrows_left),
            test_case("arrows right", `Quick, test_pp_type_arrows_right),
            test_case("hole", `Quick, test_pp_type_hole),
          ],
        ),
        (
          "pp_expr",
          [
            test_case("var", `Quick, test_pp_expr_var),
            test_case("fun", `Quick, test_pp_expr_fun),
            test_case("fun app", `Quick, test_pp_expr_fun_app),
            test_case("funs", `Quick, test_pp_expr_funs),
            test_case("funs app", `Quick, test_pp_expr_funs_app),
            test_case("app", `Quick, test_pp_expr_app),
            test_case("apps left", `Quick, test_pp_expr_apps_left),
            test_case("apps right", `Quick, test_pp_expr_apps_right),
            test_case("num zero", `Quick, test_pp_expr_num_zero),
            test_case("num positive", `Quick, test_pp_expr_num_positive),
            test_case("num negative", `Quick, test_pp_expr_num_negative),
            test_case("add", `Quick, test_pp_expr_add),
            test_case("adds left", `Quick, test_pp_expr_adds_left),
            test_case("adds right", `Quick, test_pp_expr_adds_right),
            test_case("ascribe var", `Quick, test_pp_expr_asc_var),
            test_case("ascribe fun", `Quick, test_pp_expr_asc_fun),
            test_case("ascribe in fun", `Quick, test_pp_expr_asc_in_fun),
            test_case("hole empty", `Quick, test_pp_expr_hole_empty),
            test_case("hole full", `Quick, test_pp_expr_hole_full),
          ],
        ),
        /* semantics */
        (
          "synthesize",
          [
            test_case("var fail", `Quick, test_synthesize_var_fail),
            test_case("var", `Quick, test_synthesize_var),
            test_case("app fail", `Quick, test_synthesize_app_fail),
            test_case("app fun fail1", `Quick, test_synthesize_app_fun_fail1),
            test_case("app fun fail2", `Quick, test_synthesize_app_fun_fail2),
            test_case("app fun", `Quick, test_synthesize_app_fun),
            test_case("app fun hole", `Quick, test_synthesize_app_fun_hole),
            test_case("app fun holes", `Quick, test_synthesize_app_fun_holes),
            test_case(
              "app fun holes on hole",
              `Quick,
              test_synthesize_app_fun_holes_on_hole,
            ),
            test_case(
              "app fun from hole",
              `Quick,
              test_synthesize_app_fun_from_hole,
            ),
            test_case(
              "app fun to hole",
              `Quick,
              test_synthesize_app_fun_to_hole,
            ),
            test_case("app hole fail", `Quick, test_synthesize_app_hole_fail),
            test_case("app hole", `Quick, test_synthesize_app_hole),
            test_case("app holes", `Quick, test_synthesize_app_holes),
            test_case("num", `Quick, test_synthesize_num),
            test_case("add fail1", `Quick, test_synthesize_add_fail1),
            test_case("add fail2", `Quick, test_synthesize_add_fail2),
            test_case("add", `Quick, test_synthesize_add),
            test_case("adds left", `Quick, test_synthesize_adds_left),
            test_case("adds right", `Quick, test_synthesize_adds_right),
            test_case("asc fail", `Quick, test_synthesize_asc),
            test_case("asc", `Quick, test_synthesize_asc),
            test_case("hole", `Quick, test_synthesize_hole),
            test_case("hole var", `Quick, test_synthesize_hole_var),
          ],
        ),
        (
          "analyze",
          [
            test_case("var fail", `Quick, test_analyze_var_fail),
            test_case("var hole", `Quick, test_analyze_var_hole),
            test_case("app fail", `Quick, test_analyze_app_fail),
            test_case("fun fail", `Quick, test_analyze_fun_fail),
            test_case("fun", `Quick, test_analyze_fun),
            test_case("fun hole", `Quick, test_analyze_fun_hole),
            test_case("fun hole left", `Quick, test_analyze_fun_hole_left),
            test_case("fun hole right", `Quick, test_analyze_fun_hole_right),
            test_case("fun holes", `Quick, test_analyze_fun_holes),
            test_case("funs left", `Quick, test_analyze_funs_left),
            test_case("funs right", `Quick, test_analyze_funs_right),
            test_case("num fail", `Quick, test_analyze_num_fail),
            test_case("num", `Quick, test_analyze_num),
            test_case("num hole", `Quick, test_analyze_num_hole),
            test_case("add fail", `Quick, test_analyze_add_fail),
            test_case("add", `Quick, test_analyze_add),
            test_case("adds left", `Quick, test_analyze_adds_left),
            test_case("adds right", `Quick, test_analyze_adds_right),
            test_case("asc fail", `Quick, test_analyze_asc_fail),
            test_case("asc", `Quick, test_analyze_asc),
            test_case("asc hole", `Quick, test_analyze_asc_hole),
            test_case("asc on hole", `Quick, test_analyze_asc_on_hole),
            test_case("hole fail", `Quick, test_analyze_hole_fail),
            test_case("hole none", `Quick, test_analyze_hole_none),
            test_case("hole some", `Quick, test_analyze_hole_some),
            test_case("hole on hole", `Quick, test_analyze_hole_on_hole),
          ],
        ),
      ],
    )
  );

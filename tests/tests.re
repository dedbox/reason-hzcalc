/******************************************************************************/
/* Unit Tests                                                                 */
/******************************************************************************/

open Parser;
open Printer;

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
            test_case("annotate var", `Quick, test_parse_expr_ann_var),
            test_case("annotate fun", `Quick, test_parse_expr_ann_fun),
            test_case("annotate in fun", `Quick, test_parse_expr_ann_in_fun),
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
            test_case("annotate var", `Quick, test_pp_expr_ann_var),
            test_case("annotate fun", `Quick, test_pp_expr_ann_fun),
            test_case("annotate in fun", `Quick, test_pp_expr_ann_in_fun),
            test_case("hole empty", `Quick, test_pp_expr_hole_empty),
            test_case("hole full", `Quick, test_pp_expr_hole_full),
          ],
        ),
      ],
    )
  );

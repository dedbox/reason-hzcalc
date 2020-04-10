/******************************************************************************/
/* Static Semantics                                                           */
/******************************************************************************/

open Ast.HExp;
open Ast.HTyp;

/******************************************************************************/
/* Typing Contexts                                                            */
/******************************************************************************/

module StringMap =
  Map.Make({
    type t = string;
    let compare = compare;
  });

let lookup = (x, ctx) =>
  ctx |> StringMap.mem(x) ? Some(ctx |> StringMap.find(x)) : None;

let extend = (x, t, ctx) => ctx |> StringMap.add(x, t);

/******************************************************************************/
/* Bidirectional Type Checking                                                */
/******************************************************************************/

let match_arrow = t => {
  switch (t) {
  | THol => Some(TFun(THol, THol))
  | TFun(_, _) => Some(t)
  | _ => None
  };
};

let rec is_consistent = (t, s) => {
  switch (t, s) {
  | (THol, _)
  | (_, THol) => true
  | (TFun(t1, t2), TFun(s1, s2)) =>
    is_consistent(t1, s1) && is_consistent(t2, s2)
  | (t1, t2) => t1 == t2
  };
};

let rec synthesize = (e, ctx) =>
  switch (e) {
  | Var(x) => ctx |> lookup(x)
  | App(e1, e2) =>
    switch (ctx |> synthesize(e1)) {
    | Some(t1) =>
      switch (match_arrow(t1)) {
      | Some(TFun(t2, t)) =>
        switch (ctx |> analyze(e2, t2)) {
        | Some(_) => Some(t)
        | None => None
        }
      | _ => None
      }
    | None => None
    }
  | Num(_) => Some(TNum)
  | Add(e1, e2) =>
    switch (ctx |> analyze(e1, TNum), ctx |> analyze(e2, TNum)) {
    | (Some(_), Some(_)) => Some(TNum)
    | _ => None
    }
  | Ann(e, t) => ctx |> analyze(e, t)
  | Hol(None) => Some(THol)
  | Hol(Some(e1)) =>
    switch (ctx |> synthesize(e1)) {
    | Some(_) => Some(THol)
    | None => None
    }
  | _ => None
  }

and analyze = (e, t, ctx) =>
  switch (e) {
  | Fun(x1, e2) =>
    switch (match_arrow(t)) {
    | Some(TFun(t1, t2)) =>
      switch (ctx |> extend(x1, t1) |> analyze(e2, t2)) {
      | Some(_) => Some(t)
      | None => None
      }
    | _ => None
    }
  | _ =>
    switch (ctx |> synthesize(e)) {
    | Some(t1) => is_consistent(t, t1) ? Some(t) : None
    | None => None
    }
  };

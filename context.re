/******************************************************************************/
/* Variable Contexts                                                          */
/******************************************************************************/

module Context =
  Map.Make({
    type t = string;
    let compare = compare;
  });

let lookup = (x, ctx) =>
  ctx |> Context.mem(x) ? Some(ctx |> Context.find(x)) : None;

let extend = (x, t, ctx) => ctx |> Context.add(x, t);

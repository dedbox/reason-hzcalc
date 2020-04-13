/******************************************************************************/
/* Abstract Syntax Trees                                                      */
/******************************************************************************/

module HTyp = {
  type t =
    | TFun(t, t)
    | TNum
    | THol;
};

module HExp = {
  type t =
    | Var(string)
    | Fun(string, t)
    | App(t, t)
    | Num(int)
    | Add(t, t)
    | Asc(t, HTyp.t)
    | Hol(option(t));
};

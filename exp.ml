type operacao =
  | Som
  | Sub
  | Pro
  | Div
  | Pot

type exp =
  | Cte of float
  | Var of string
  | Op of operacao * exp * exp
  | Atr of string * exp

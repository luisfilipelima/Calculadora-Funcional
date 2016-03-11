type operacao =
  | Som
  | Sub
  | Pro
  | Div
  | Pot
  | Igd
  | ELog
  | OuLog

type exp =
  | Cte of float
  | Var of string
  | Op of operacao * exp * exp
  | Atr of string * exp

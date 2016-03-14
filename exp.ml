type operacao =
    | Som
    | Sub
    | Pro
    | Div
    | Pot
    | Igd
    | Dif
    | ELog
    | OuLog
    | NegaLog

type exp =
    | Cte of float
    | Var of string
    | Op of operacao * exp * exp
    | Atr of string * exp

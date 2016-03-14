(* avaliacao.ml *)

open Exp

let rec avalia memoria exp =
    match exp with
    | Cte x -> ( x, memoria )
    | Var nome -> let valor_var = try List.assoc nome memoria
        with Not_found -> 0.0 in
        ( valor_var, memoria )
    | Op ( op, x, y ) -> let ( valor_x, memoria' ) = avalia memoria x in
        let ( valor_y, memoria'' ) = avalia memoria' y in
        let valor_res =
            match op with
            | Som      -> valor_x +. valor_y
            | Sub      -> valor_x -. valor_y
            | Pro      -> valor_x *. valor_y
            | Div      -> valor_x /. valor_y
            | Pot      -> valor_x ** valor_y
            | Igd      -> if valor_x = valor_y then 1. else 0.
            | Dif      -> if valor_x = valor_y then 0. else 1.
            | ELog     -> if valor_x = 0. || valor_y = 0. then 0. else 1.
            | OuLog    -> if valor_x = 0. && valor_y = 0. then 0. else 1.
            | Mir      -> if valor_x > valor_y then 1. else 0.
            | MirIgd   -> if valor_x >= valor_y then 1. else 0.
            | Mnr      -> if valor_x < valor_y then 1. else 0.
            | MnrIgd   -> if valor_x <= valor_y then 1. else 0.
        in
        ( valor_res, memoria'' )
    | NegaOp ( op, x ) -> let ( valor_x, memoria' ) = avalia memoria x in
        if valor_x = 0. then ( 1., memoria' ) else ( 0., memoria' )
    | Atr ( nome, x ) -> let ( valor_x, memoria' ) = avalia memoria x in
        ( valor_x, ( nome, valor_x ) :: memoria' )

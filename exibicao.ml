(* exibicao.ml *)

open Exp
open Tree

let string_of_operacao f =
    match f with
    | Som -> "Som"
    | Sub -> "Sub"
    | Pro -> "Pro"
    | Div -> "Div"
    | Pot -> "Pot"
    | ELog -> "E"
    | OuLog -> "Ou"

let rec tree_of_exp e =
    match e with
    | Cte x -> Node (string_of_float x, [])
    | Var v -> Node (v, [])
    | Op (f, e1, e2) -> Node (string_of_operacao f,
			    [ tree_of_exp e1; tree_of_exp e2 ]
			   )
    | Atr (v, e) -> Node ("Atrib", [Node (v, []); tree_of_exp e])

let string_of_exp e =
    string_of_tree (tree_of_exp e)

let e1 = Op (Som,
	     Cte 2.,
	     Op (Pro,
		 Var "x",
		 Cte 5.))

(* let _ = *)
(*   print_endline (string_of_exp e1) *)

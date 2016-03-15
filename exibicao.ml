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
    | Igd -> "Igd"
    | Dif -> "Dif"
    | ELog -> "E"
    | OuLog -> "Ou"
    | NegaLog -> "Nega"
    | Mir -> "Mir"
    | Mnr -> "Mnr"
    | MirIgd -> "MirIgd"
    | MnrIgd -> "MnrIgd"

let rec tree_of_exp e =
    match e with
    | Cte x -> Node (string_of_float x, [])
    | Var v -> Node (v, [])
    | Op (f, e1, e2) -> Node (string_of_operacao f,
			    [ tree_of_exp e1; tree_of_exp e2 ]
			   )
    | NegaOp ( f, e1 ) -> Node ( string_of_operacao f, [ tree_of_exp e1 ] )
    | ExCon ( x, y, z ) -> Node ( "if", [tree_of_exp x; tree_of_exp y; tree_of_exp z] )
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

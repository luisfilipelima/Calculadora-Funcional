(* tree.ml *)

type 'a tree =
  | Empty
  | Node of 'a * 'a tree list

let mkt x ts = Node (x, ts)


let rec map f t =
  match t with
  | Empty -> Empty
  | Node (info, children) -> Node (f info, List.map (map f) children)


let string_of_tree t =
  let buf = Buffer.create 16 in
  let rec loop indentation t =
    let child_to_string continue_indentation x =
      Buffer.add_char buf  '\n';
      Buffer.add_string buf indentation;
      Buffer.add_string buf "+- ";
      loop (indentation ^ continue_indentation) x
    in
    let rec children_to_string children =
      match children with
      | [x]     -> child_to_string "   " x
      | x :: xs -> child_to_string "|  " x;
                   children_to_string xs
      | []      -> ()
    in
    match t with
    | Empty -> Buffer.add_char buf '.'
    | Node (x, children) -> Buffer.add_string buf x;
			    children_to_string children
  in
  loop "" t;
  Buffer.contents buf

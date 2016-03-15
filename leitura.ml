(* leitura.ml *)

open Exp
open Exibicao

type palavra =
    | PalAPar
    | PalFPar
    | PalOp of operacao
    | PalLit of float
    | PalId of string
    | PalAtr
    | PalIf
    | PalThen
    | PalElse

let string_of_palavra p =
    match p with
    | PalAPar -> "PalAPar"
    | PalFPar -> "PalFPar"
    | PalOp f -> "PalOp(" ^ string_of_operacao f ^ ")"
    | PalLit x -> "PalLit(" ^ string_of_float x ^ ")"
    | PalId id -> "PalId(" ^ id ^ ")"
    | PalAtr -> "PalAtr"
    | PalIf -> "PalIf"
    | PalThen -> "PalThen"
    | PalElse -> "PalElse"

let is_digit c =
    c >= '0' && c <= '9'

let is_letter c =
    c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

let salta_pred pred str comeco =
    let n = String.length str in
    let rec loop i =
        if i < n && pred (String.get str i) then
            loop (i+1)
        else
            i
    in
    loop comeco

exception Caracter_invalido of char

let divide_em_palavras str =
    let n = String.length str in
    let rec loop i lst =
        if i = n then
            List.rev lst
        else
            match String.get str i with
            | ' '
            | '\t'
            | '\n' -> loop ( i + 1 ) lst
            | '(' -> loop ( i + 1 ) ( PalAPar::lst )
            | ')' -> loop ( i + 1) ( PalFPar::lst )
            | '+' -> loop ( i + 1 ) ( PalOp Som::lst )
            | '-' -> loop ( i + 1 ) ( PalOp Sub::lst )
            | '*' -> let j = i + 1 in
                   if j < n && String.get str j = '*' then
                     loop ( j + 1 ) ( PalOp Pot::lst )
                   else
                     loop ( i + 1 ) ( PalOp Pro::lst )
            | '/' -> loop ( i + 1 ) ( PalOp Div::lst )
            | '^' -> loop ( i + 1 ) ( PalOp Pot::lst )
            | ':' -> let j = i + 1 in
                   if j < n && String.get str j = '=' then
                     loop ( j + 1 ) ( PalAtr::lst )
                   else
                      raise ( Caracter_invalido ':' )
            | '=' -> loop ( i + 1 ) ( PalOp Igd::lst )
            | '>' -> let j = i + 1 in
                   if j < n && String.get str j = '=' then
                        loop ( j + 1 ) ( PalOp MirIgd::lst )
                   else if j < n then
                        loop ( j ) ( PalOp Mir::lst )
                   else
                        raise ( Caracter_invalido '>' )
            | '<' -> let j = i + 1 in
                   if j < n && String.get str j = '>' then
                        loop ( j + 1 ) ( PalOp Dif::lst )
                   else if j < n && String.get str j = '=' then
                        loop ( j + 1 ) ( PalOp MnrIgd::lst )
                   else if j < n then
                        loop ( j ) ( PalOp Mnr::lst )
                   else
                     raise ( Caracter_invalido '<' )
            | '&' -> let j = i + 1 in
                   if j < n && String.get str j = '&' then
                     loop ( j + 1 ) ( PalOp ELog::lst )
                   else
                      raise ( Caracter_invalido '&' )
            | '|' -> let j = i + 1 in
                   if j < n && String.get str j = '|' then
                     loop ( j + 1 ) ( PalOp OuLog::lst )
                   else
                      raise ( Caracter_invalido '|' )
            | '~' -> loop ( i + 1 ) ( PalOp NegaLog::lst )
            | c ->
                if is_digit c then
                    let j =
            	      let j0 = salta_pred is_digit str ( i + 1 ) in
                      if j0 < n && String.get str j0 = '.' then
                        salta_pred is_digit str ( j0 + 1 )
                      else
            	        j0
                      in

                      let texto = String.sub str i ( j - i ) in
                      let numero = float_of_string texto in
                      loop j ( PalLit numero::lst )

                else if is_letter c then
                   let j = salta_pred ( fun x -> is_letter x ||
                       is_digit x || x = '_' ) str i in
                   let texto = String.sub str i ( j ) in
                   if texto = "if" then
                        loop j ( PalIf::lst )
                   else if texto = "then" then
                        loop j ( PalThen::lst )
                   else if texto = "else" then
                        loop j ( PalElse::lst )
                   else
                        loop j ( PalId texto::lst )
                else
                   raise ( Caracter_invalido c )
    in
    loop 0 []

exception Sintaxe of string

let rec expressao palavras =
    match palavras with
    | PalId nome :: PalAtr :: resto -> let ( e, resto' ) = expressao_aritmetica resto in
                                     ( Atr ( nome, e ), resto' )
    | _ -> expressao_logica palavras

(*
and expressao_logica_nega palavras =
    let ( x, resto ) = expressao_logica palavras in
    let rec todos_os_termos x resto =
        match resto with
        | PalOp op :: resto' when op = NegaLog ->
            let ( y, resto'' ) = expressao_logica resto' in
            todos_os_termos ( NegaOp ( op, x ) ) resto''
        | _ -> ( x, resto )
    in
    todos_os_termos x resto
*)

and expressao_logica palavras =
    let ( x, resto ) = expressao_relacional palavras in
    let rec todos_os_termos x resto =
        match resto with
        | PalOp op :: resto' when op = ELog || op = OuLog ->
            let ( y, resto'' ) = expressao_relacional resto' in
            todos_os_termos ( Op ( op, x, y ) ) resto''
        | _ -> ( x, resto )
    in
    todos_os_termos x resto

and expressao_relacional palavras =
	let ( x, resto ) = expressao_aritmetica palavras in
	match resto with
	| PalOp op :: resto' when op = Igd  || op = Dif || op = Mir || op = MirIgd ||
        op = MnrIgd || op = Mnr -> let ( y, resto'' ) =
        expressao_aritmetica resto'
        in
        ( Op ( op, x, y ), resto'' )
	|_ -> ( x, resto )

and expressao_aritmetica palavras =
    let ( x, resto ) = termo palavras in
    let rec todos_os_termos x resto =
        match resto with
        | PalOp op :: resto' when op = Som || op = Sub ->
             let ( y, resto'' ) = termo resto' in
             todos_os_termos ( Op ( op, x, y ) ) resto''
        | _ -> ( x, resto )
    in
    todos_os_termos x resto

and termo palavras =
    let ( x, resto ) = fator palavras in
    let rec todos_os_fatores x resto =
        match resto with
        | PalOp op :: resto' when op = Pro || op = Div ->
            let ( y, resto'' ) = fator resto' in
            todos_os_fatores ( Op ( op, x, y ) ) resto''
        | _ -> ( x, resto )
    in
    todos_os_fatores x resto

and fator palavras =
    let ( x, resto ) = basica palavras in
    match resto with
    | PalOp Pot :: resto' -> let ( y, resto'' ) = fator resto' in
                           ( Op ( Pot, x, y ), resto'' )
    | _ -> ( x, resto )

and basica palavras =
    match palavras with
    | PalLit x :: resto -> ( Cte x, resto )
    | PalId v :: resto -> ( Var v, resto )
    | PalAPar :: resto -> ( let ( x, resto' ) = expressao resto in
                          match resto' with
                          | PalFPar :: resto'' -> ( x,resto'' )
                          | _ -> raise ( Sintaxe ") esperado" )
                        )
    | PalOp Sub :: resto -> let ( x, resto' ) = fator resto in
                          ( Op ( Sub, Cte 0.0, x ), resto' )
    | PalOp NegaLog :: resto -> let ( x, resto' ) = fator resto in
                          ( NegaOp ( NegaLog, x ), resto' )
    | PalIf :: resto -> let ( x, resto1 ) = expressao resto in
        begin
        match resto1 with
        | PalThen :: resto2 -> let ( y, resto3 ) = expressao resto2 in
            begin
            match resto3 with
            | PalElse :: resto3 -> let ( z, resto4 ) = expressao resto3 in
                ( ExCon ( x, y, z), resto4 )
            | _ -> raise ( Sintaxe "else nao encontrado" )
            end
        | _ -> raise ( Sintaxe "then nao encontrado" )
        end

    | _ -> raise ( Sintaxe "fator esperado" )


let exp_of_string string =
begin
    match expressao ( divide_em_palavras string ) with
    | ( x, lst ) -> x
    | _ -> raise ( Sintaxe "expressão inválida" )    
end

(*
let teste1 =
  let entrada = read_line () in
  let palavras = divide_em_palavras entrada in
  List.iter (fun p -> print_endline (string_of_palavra p)) palavras
*)

(*
let teste2 =
  let entrada = read_line () in
  let (e, sobra) = exp_of_string entrada in
  print_endline (string_of_exp e);
  print_newline ();
  List.iter (fun p -> print_endline (string_of_palavra p)) sobra
 *)


let rec le_avalia_exibe memoria =
  try
    print_string ">>> ";
    let entrada = read_line () in
    (*
    let palavras = Leitura.divide_em_palavras entrada in
    List.iter (fun p -> print_endline (Leitura.string_of_palavra p)) palavras;
    let (e,sobra) = Leitura.expressao palavras in
    print_endline (Exibicao.string_of_exp e);
    List.iter (fun p -> print_endline (Leitura.string_of_palavra p)) sobra;
     *)
    begin
      try
        let e = Leitura.exp_of_string entrada in
        print_endline "Estrutura da expressão:";
        print_endline (Exibicao.string_of_exp e);
        print_newline ();
        let (v,m) = Avaliacao.avalia memoria e in
        print_endline "Valor da expressão:";
        print_float v;
        print_newline ();
        print_newline ();
        print_endline "Memória:";
        List.iter (fun (var,valor) -> Printf.printf "%s: %f\n" var valor) m;
        print_newline ();
        print_newline ();
        le_avalia_exibe m
      with Leitura.Sintaxe msg -> print_endline msg
         | Leitura.Caracter_invalido c -> print_endline "caracter inválido"
    end
  with End_of_file -> ()


let _ =
  print_endline "Calculadora v0.0.1";
  print_endline "------------------------------------";
  le_avalia_exibe []

(*

TRABALHO:

Acrescentar expressões condicionais:

   operadores relacionais:
      = (igual)
      <> (difernte)
      < (menor que)
      > (maior que)
      <= (menor ou igual a)
      >= (menor ou igual a)

   operadores lógicos:
      && (e lógico)
      || (ou lógico)
   
   negação lógica:
      ~ (operador unário)

   expressão condicional:
      if <expressao> then <expressao> else <expressao>
      
      Exemplos:
        if 2 > 0 then 5+1 else 5-1  =>  6.0

        if x > 0 then a := 2 else a := -2  => -2.0   [efeito de a := -2.0]

 Use a prioridade e associatividade usual dos operadores.

 Como não temos um tipo para expressões booleanas, considere:
      verdadeiro: qualquer valor diferente de 0.0
      falso: 0.0

*)

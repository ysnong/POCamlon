open Interpreter

let rec repl env =
  print_string ">> ";
  flush stdout;
  try
    let line = input_line stdin in
    (* End executing when exit/quit command *)
    if String.trim line = "exit" || String.trim line = "quit" then
      (print_endline "Goodbye, trainer!"; env)
    else
      let lexbuf = Lexing.from_string line in
      let ast = Parser.main Lexer.token lexbuf in
      let (env', _) = eval env ast in
      repl env'
  with
  | End_of_file ->
      print_endline "\nThe Pokédex is closing... Goodbye, trainer!";
      env

  | Lexer.Lexing_error msg ->
      Printf.printf "⚠️ A wild character appeared! %s\n" msg;
      repl env

  | Parser.Error ->
      Printf.printf "❓Pikachu doesn't understand that move...\n";
      repl env

  | Failure msg ->
      Printf.printf "💥 Whaaat? Say that again... (%s)\n" msg;
      repl env

  | _ ->
      Printf.printf "⁉️ makes Pokemons confused!\n";
      repl env

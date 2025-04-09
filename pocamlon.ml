open Interpreter
open Repl

let process_file env filename =
  let channel = open_in filename in
  let rec loop env =
    try
      let line = input_line channel in
      if String.trim line = "" then loop env else
      let lexbuf = Lexing.from_string line in
      let ast = Parser.main Lexer.token lexbuf in
      let (env', _) = eval env ast in
      loop env'
    with
    | End_of_file ->
        close_in channel;
        env
    | Lexer.Lexing_error msg ->
        Printf.printf "⚠️ A wild character appeared! %s\n" msg;
        loop env
    | Parser.Error ->
        Printf.printf "❓ Pikachu doesn't understand that move... \n";
        loop env
    | Failure msg ->
        Printf.printf "💥 Whaaat? Say that again... (%s)\n" msg;
        loop env
    | _ ->
        Printf.printf "⁉️ makes Pokemons confused! \n";
        loop env
  in
  ignore (loop env)

let () =
  let initial_env = [] in
  if Array.length Sys.argv > 1 then
    process_file initial_env Sys.argv.(1)
  else
    ignore (repl initial_env)

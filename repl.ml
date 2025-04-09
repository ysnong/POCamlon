open Interpreter

let rec repl env =
  print_string ">> ";
  flush stdout;
  try
    let line = input_line stdin in
    let lexbuf = Lexing.from_string line in
    let ast = Parser.main Lexer.token lexbuf in
    let (env', _) = eval env ast in
    repl env'
  with
  | End_of_file -> env
  | Failure msg ->
    Printf.eprintf "Error: %s\n" msg;
    repl env
  | Parser.Error ->
    Printf.eprintf "Parse error.\n";
    repl env

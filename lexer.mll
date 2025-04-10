{
  open Parser
  exception Lexing_error of string
}

rule token = parse
| [' ' '\t' '\n' '\r'] { token lexbuf }  (* skip whitespace *)
| "\"" ([^ '"' '\n']* as s) "\"" { STRING s }

| "let"                { LET }
| "battle"             { BATTLE }
| "PokeMon"            { POKEMON }
| "Electric"           { ELECTRIC }
| "Water"              { WATER }
| "Fire"               { FIRE }
| "if"                 { IF } 
| "then"               { THEN } 
| "else"               { ELSE } 
| "print"              { PRINT }
| "statall"            { STATALL }
| "fun" { FUN }
| "->"  { ARROW }
| "typeof"  { TYPEOF }
| "in"       { IN } 
| "true" { BOOL(true) }
| "false" { BOOL(false) }


| '"' [^ '"']* '"' as s  { 
    (* Grab string without quotes *)
    let inner = String.sub s 1 (String.length s - 2) in
    STRING inner
  }

| ['0'-'9']+ as digits { INT (int_of_string digits) }

| ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { IDENT id }

| '='   { EQ }
| '['   { LBRACKET }
| ']'   { RBRACKET }
| ','   { COMMA }
| '('   { LPAREN }
| ')'   { RPAREN }
| "+"    { PLUS }
| "-"    { MINUS }
| "*"    { TIMES }
| "<"    { LT }
| ">"    { GT }
| "=="   { EQQ }
| '.'   { DOT }
| eof { EOF }

| _ as c { raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c)) }

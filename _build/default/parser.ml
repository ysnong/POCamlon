
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WATER
    | STRING of (
# 8 "parser.mly"
       (string)
# 16 "parser.ml"
  )
    | RPAREN
    | RBRACKET
    | POKEMON
    | LPAREN
    | LET
    | LBRACKET
    | INT of (
# 7 "parser.mly"
       (int)
# 27 "parser.ml"
  )
    | IDENT of (
# 9 "parser.mly"
       (string)
# 32 "parser.ml"
  )
    | FIRE
    | EQ
    | EOF
    | ELECTRIC
    | COMMA
    | BATTLE
  
end

include MenhirBasics

# 1 "parser.mly"
  
  open Ast

# 49 "parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_main) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState14 : (('s, _menhir_box_main) _menhir_cell1_LPAREN, _menhir_box_main) _menhir_state
    (** State 14.
        Stack shape : LPAREN.
        Start symbol: main. *)

  | MenhirState17 : (('s, _menhir_box_main) _menhir_cell1_BATTLE, _menhir_box_main) _menhir_state
    (** State 17.
        Stack shape : BATTLE.
        Start symbol: main. *)

  | MenhirState18 : ((('s, _menhir_box_main) _menhir_cell1_BATTLE, _menhir_box_main) _menhir_cell1_expr, _menhir_box_main) _menhir_state
    (** State 18.
        Stack shape : BATTLE expr.
        Start symbol: main. *)

  | MenhirState24 : (('s, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_IDENT, _menhir_box_main) _menhir_state
    (** State 24.
        Stack shape : LET IDENT.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and 's _menhir_cell0_type_name = 
  | MenhirCell0_type_name of 's * (Ast.poketype)

and ('s, 'r) _menhir_cell1_BATTLE = 
  | MenhirCell1_BATTLE of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_IDENT = 
  | MenhirCell0_IDENT of 's * (
# 9 "parser.mly"
       (string)
# 91 "parser.ml"
)

and ('s, 'r) _menhir_cell1_LET = 
  | MenhirCell1_LET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_POKEMON = 
  | MenhirCell1_POKEMON of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_STRING = 
  | MenhirCell0_STRING of 's * (
# 8 "parser.mly"
       (string)
# 107 "parser.ml"
)

and _menhir_box_main = 
  | MenhirBox_main of (Ast.expr) [@@unboxed]

let _menhir_action_01 =
  fun _2 _4 ->
    (
# 20 "parser.mly"
                    ( Let(_2, _4, Int 0) )
# 118 "parser.ml"
     : (Ast.expr))

let _menhir_action_02 =
  fun _1 ->
    (
# 21 "parser.mly"
       ( _1 )
# 126 "parser.ml"
     : (Ast.expr))

let _menhir_action_03 =
  fun _1 ->
    (
# 24 "parser.mly"
      ( Int _1 )
# 134 "parser.ml"
     : (Ast.expr))

let _menhir_action_04 =
  fun _1 ->
    (
# 25 "parser.mly"
        ( Var _1 )
# 142 "parser.ml"
     : (Ast.expr))

let _menhir_action_05 =
  fun _2 _3 _5 _7 ->
    (
# 27 "parser.mly"
  ( PokeMon(_2, _3, _5, _7) )
# 150 "parser.ml"
     : (Ast.expr))

let _menhir_action_06 =
  fun _2 _3 ->
    (
# 28 "parser.mly"
                   ( Battle(_2, _3) )
# 158 "parser.ml"
     : (Ast.expr))

let _menhir_action_07 =
  fun _2 ->
    (
# 30 "parser.mly"
                     ( _2 )
# 166 "parser.ml"
     : (Ast.expr))

let _menhir_action_08 =
  fun _1 ->
    (
# 17 "parser.mly"
              ( _1 )
# 174 "parser.ml"
     : (Ast.expr))

let _menhir_action_09 =
  fun _1 ->
    (
# 38 "parser.mly"
         ( [_1] )
# 182 "parser.ml"
     : (string list))

let _menhir_action_10 =
  fun _1 _3 ->
    (
# 39 "parser.mly"
                         ( _1 @ [_3] )
# 190 "parser.ml"
     : (string list))

let _menhir_action_11 =
  fun () ->
    (
# 40 "parser.mly"
              ( [] )
# 198 "parser.ml"
     : (string list))

let _menhir_action_12 =
  fun () ->
    (
# 33 "parser.mly"
           ( Electric )
# 206 "parser.ml"
     : (Ast.poketype))

let _menhir_action_13 =
  fun () ->
    (
# 34 "parser.mly"
        ( Water )
# 214 "parser.ml"
     : (Ast.poketype))

let _menhir_action_14 =
  fun () ->
    (
# 35 "parser.mly"
       ( Fire )
# 222 "parser.ml"
     : (Ast.poketype))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | BATTLE ->
        "BATTLE"
    | COMMA ->
        "COMMA"
    | ELECTRIC ->
        "ELECTRIC"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | FIRE ->
        "FIRE"
    | IDENT _ ->
        "IDENT"
    | INT _ ->
        "INT"
    | LBRACKET ->
        "LBRACKET"
    | LET ->
        "LET"
    | LPAREN ->
        "LPAREN"
    | POKEMON ->
        "POKEMON"
    | RBRACKET ->
        "RBRACKET"
    | RPAREN ->
        "RPAREN"
    | STRING _ ->
        "STRING"
    | WATER ->
        "WATER"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_goto_command : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_08 _1 in
          MenhirBox_main _v
      | _ ->
          _eRR ()
  
  let _menhir_run_27 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_02 _1 in
      _menhir_goto_command _menhir_stack _v _tok
  
  let _menhir_run_25 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LET _menhir_cell0_IDENT -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _v _tok ->
      let MenhirCell0_IDENT (_menhir_stack, _2) = _menhir_stack in
      let MenhirCell1_LET (_menhir_stack, _) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_01 _2 _4 in
      _menhir_goto_command _menhir_stack _v _tok
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_POKEMON (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRING _v ->
          let _menhir_stack = MenhirCell0_STRING (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WATER ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_13 () in
              _menhir_goto_type_name _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | FIRE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_14 () in
              _menhir_goto_type_name _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | ELECTRIC ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_12 () in
              _menhir_goto_type_name _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_type_name : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_POKEMON _menhir_cell0_STRING -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_type_name (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | LBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_0 in
              let _v = _menhir_action_09 _1 in
              _menhir_goto_move_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | COMMA | RBRACKET ->
              let _v = _menhir_action_11 () in
              _menhir_goto_move_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_move_list : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_POKEMON _menhir_cell0_STRING _menhir_cell0_type_name -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let MenhirCell0_type_name (_menhir_stack, _3) = _menhir_stack in
              let MenhirCell0_STRING (_menhir_stack, _2) = _menhir_stack in
              let MenhirCell1_POKEMON (_menhir_stack, _menhir_s) = _menhir_stack in
              let (_7, _5) = (_v_0, _v) in
              let _v = _menhir_action_05 _2 _3 _5 _7 in
              _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | COMMA ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v_1 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let (_1, _3) = (_v, _v_1) in
              let _v = _menhir_action_10 _1 _3 in
              _menhir_goto_move_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_27 _menhir_stack _v _tok
      | MenhirState24 ->
          _menhir_run_25 _menhir_stack _v _tok
      | MenhirState14 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState18 ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState17 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_20 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_07 _2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_19 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_BATTLE, _menhir_box_main) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_BATTLE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_06 _2 _3 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_18 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_BATTLE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | POKEMON ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | LPAREN ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | INT _v_0 ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState18
      | IDENT _v_1 ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState18
      | BATTLE ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | _ ->
          _eRR ()
  
  and _menhir_run_14 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState14 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | POKEMON ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BATTLE ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_15 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_03 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_16 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_04 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_17 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BATTLE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState17 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | POKEMON ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BATTLE ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | POKEMON ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | EQ ->
                  let _menhir_s = MenhirState24 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | POKEMON ->
                      _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | LPAREN ->
                      _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INT _v ->
                      _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | IDENT _v ->
                      _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | BATTLE ->
                      _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | INT _v ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BATTLE ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

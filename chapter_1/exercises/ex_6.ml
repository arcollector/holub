(*
  parse and evualate lisp syntax
  ( * 4 (+ 1 2))
*)

let rec lexer = parser
  | [< ' (' ' | '\n' | '\r' | '\t'); stream >] -> lexer stream
  | [< ' ('('); stream >] -> [< ' `Lp; lexer stream >]
  | [< ' (')'); stream >] -> [< ' `Rp; lexer stream >]
  | [< ' ('+'); stream >] -> [< ' `Plus; lexer stream >]
  | [< ' ('*'); stream >] -> [< ' `Times; lexer stream >]
  | [< ' ('0'..'9' as ch); stream >] -> 
    let rec lexer_integer buf' = parser
    | [< ' ('0'..'9' as ch'); stream >] ->
      Buffer.add_char buf' ch';
      lexer_integer buf' stream
    | [< >] ->
      Buffer.contents buf' |> int_of_string in
    let buf = Buffer.create 1 in
    Buffer.add_char buf ch;
    let integer = lexer_integer buf stream in
    [< ' `Integer integer; lexer stream >]

let rec evaluator = parser
  | [<
    ' `Lp;
    ' (`Plus | `Times as op) ?? "Expected binary operation after '('\n";
    integers = parse_integers [];
    ' `Rp
  >] ->
    begin match op with
    | `Plus -> List.fold_right (+) integers 0
    | `Times -> List.fold_right ( * ) integers 1 end

  | [< >] -> Stream.Error "unknown syntax error\n" |> raise

and parse_integers acc = parser
  | [< ' `Integer integer; stream >] -> parse_integers (integer::acc) stream
  | [< ?= [ `Lp ]; stream >] -> parse_integers ((evaluator stream)::acc) stream
  | [< ?= [ `Rp ] >] -> acc
  | [< >] -> Stream.Error "expected ')' after integers declaration\n" |> raise

let _ = 
  "(* 4 (+ 1 2))" |>
    Stream.of_string |>
    lexer |>
    evaluator |>
    string_of_int |>
    print_endline ;

  "(+ 1 2 3 4 5 6 7 8 9 10)" |>
    Stream.of_string |>
    lexer |>
    evaluator |>
    string_of_int |>
    print_endline
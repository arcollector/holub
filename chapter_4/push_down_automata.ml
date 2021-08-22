open Stack_using_functions

(*
  statements    -> epsilon
                | expression ; statements
  expression    -> term expression'
  expression'   -> + term expression'
                | epsilon
  term          -> factor term'
  term'         -> * factor term'
                | epsilon
  factor        -> NUMBER
                | ID
                | ( expression )
*)

type production_rules =
  | Statements
  | Expression
  | Expression'
  | Term
  | Term'
  | Factor
  | CharSymbol of char
  | NumSymbol

let get_c stream =
  match Stream.peek stream with
  | Some c -> c
  | None -> raise (Stream.Error "stream fail unexpecited")

let junk stream =
  Stream.junk stream

let is_stream_empty stream =
  try match Stream.empty stream with () -> true
  with _ -> false

let raise_syntax_error ?(c = ' ') at =
  let error_string =
    match c with
    | ' ' ->
      "syntax error " ^ at
    | c ->
      "syntax error " ^ at ^ " received an " ^ String.make 1 c
  in
  Stream.Error error_string |> raise

let rec parse_digits stream buf =
  match get_c stream with
  | '0' | '1' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c ->
    Buffer.add_char buf c;
    junk stream;
    parse_digits stream buf
  | _ ->
    Buffer.contents buf

let process stream stack =
  match Stack.top stack with
  | Statements ->
    if is_stream_empty stream then
      Stack.pop stack
    else begin match get_c stream with
    | '(' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
      Stack.pop stack |>
      Stack.push Statements |>
      Stack.push (CharSymbol ';') |>
      Stack.push Expression
    | c ->
      raise_syntax_error ~c "Statements" end

  | Expression ->
    if is_stream_empty stream then
      raise_syntax_error "Expression: stream is empty"
    else begin match get_c stream with
    | '(' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
      Stack.pop stack |>
      Stack.push Expression' |>
      Stack.push Term
    | c ->
      raise_syntax_error ~c "Expression" end

  | Expression' ->
    begin match get_c stream with
    | ';' | ')' ->
      Stack.pop stack
    | '+' ->
      Stack.pop stack |>
      Stack.push Expression' |>
      Stack.push Term |>
      Stack.push (CharSymbol '+')
    | c ->
      raise_syntax_error ~c "Expression'" end

  | Term ->
    if is_stream_empty stream then
      raise_syntax_error "Term: stream is empty"
    else begin match get_c stream with
    | '(' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
      Stack.pop stack |>
      Stack.push Term' |>
      Stack.push Factor
    | c ->
      raise_syntax_error ~c "Term" end

  | Term' ->
    if is_stream_empty stream then
      raise_syntax_error "Term': stream is empty"
    else begin match get_c stream with
    | ';' | '+' | ')' ->
      Stack.pop stack
    | '*' ->
      Stack.pop stack |>
      Stack.push Term' |>
      Stack.push Factor |>
      Stack.push (CharSymbol '*')
    | c ->
      raise_syntax_error ~c "Term'" end

  | Factor ->
    if is_stream_empty stream then
      raise_syntax_error "Factor: stream is empty"
    else begin match get_c stream with
    | '0' | '1' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
      Stack.pop stack |>
      Stack.push NumSymbol
    | '(' ->
      Stack.pop stack |>
      Stack.push (CharSymbol ')') |>
      Stack.push Expression |>
      Stack.push (CharSymbol '(')
    | c ->
      raise_syntax_error ~c "Factor" end

  | CharSymbol c ->
    begin match get_c stream, c with
    | '+', '+' ->
      print_endline "+";
      junk stream;
      Stack.pop stack
    | '*', '*' ->
      print_endline "*";
      junk stream;
      Stack.pop stack
    | '(', '(' ->
      print_endline "(";
      junk stream;
      Stack.pop stack
    | ')', ')' ->
      print_endline ")";
      junk stream;
      Stack.pop stack
    | ';', ';' ->
      print_endline ";";
      junk stream;
      Stack.pop stack
    | current, expected ->
      let error_string =
        "expecting an " ^ String.make 1 expected ^ " symbol " ^
        "current is " ^ String.make 1 current in
      raise_syntax_error error_string end

  | NumSymbol ->
    begin match get_c stream with
    | '0' | '1' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c ->
      junk stream;
      let buf = Buffer.create 1 in
      Buffer.add_char buf c;
      let digits = parse_digits stream buf in
      print_endline digits;
      Stack.pop stack
    | c ->
      raise_syntax_error ~c "NumSymbol" end

let rec top_down_parse stream stack =
  if Stack.is_empty stack then
    ()
  else
    process stream stack |> top_down_parse stream 
  
let () =
  let stack = Stack.(push Statements empty) in
  let stream = "1+1*(1+1);" |> Stream.of_string in
  top_down_parse stream stack

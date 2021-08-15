(*
  stmt -> expr ; | expr ; stmt
  expr -> term + expr | term
  term -> factor * term | factor
  factor -> number | ( expr )
*)

let get_c stream =
  match Stream.peek stream with
  | Some c -> c
  | _ -> raise (Stream.Error "stream is empty")

let junk stream =
  Stream.junk stream

let is_empty stream =
  try match Stream.empty stream with () -> true
  with _ -> false

let rec stmt stream =
  let res = expr stream in
  match get_c stream with
  | ';' ->
    junk stream;
    string_of_int res |> print_endline;
    if is_empty stream then () else stmt stream
  | _ -> raise (Stream.Error "expected ';' at stmt production")

and expr stream =
  let res = term stream in
  match get_c stream with
  | '+' ->
    junk stream;
    let res' = expr stream in
    res + res'
  | _ -> res

and term stream =
  let res = factor stream in
  match get_c stream with
  | '*' ->
    junk stream;
    let res' = term stream in
    res * res'
  | _ -> res

and factor stream =
  match get_c stream with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c ->
    junk stream;
    let buf = Buffer.create 1 in
    Buffer.add_char buf c;
    read_digits stream buf
  | '(' ->
      junk stream;
      let res = expr stream in
      begin match get_c stream with
      | ')' ->
        junk stream;
        res
      | _ -> raise (Stream.Error "expected closing parenthesis") end
  | c ->
    raise (Stream.Error ("factor " ^ String.make 1 c ^ " is unrecognized"))

and read_digits stream buf =
  match get_c stream with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c ->
    junk stream;
    Buffer.add_char buf c;
    read_digits stream buf
  | _ ->
    Buffer.contents buf |> int_of_string

let () =
  "1+2*(3+4)+5;1+2;" |> Stream.of_string |> stmt

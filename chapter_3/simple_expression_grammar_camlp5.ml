(*
  stmt -> expr ; | expr ; stmt
  expr -> term + expr | term
  term -> utop * term | utop
  utop -> - factor | factor
  factor -> NUMBER | ( expr )
*)

let rec stmt = parser
  | [< res = expr; ' (';'); stream >] ->
    string_of_int res |> print_endline;
    stmt stream
  | [< >] -> ()

and expr = parser
  | [< 
    left = term;
    res = parser
    | [< ' ('+'); right = expr >] -> left + right
    | [< >] -> left
  >] -> res
    
and term = parser
  | [<
    left = utop;
    res = parser
    | [< ' ('*'); right = term >] -> left * right
    | [< >] -> left
  >] -> res

and utop = parser
  | [< ' ('-'); number = factor >] -> -number
  | [< number = factor >] -> number

and factor = parser
  | [< ' ('0' .. '9') as c; stream >] ->
    let buf = Buffer.create 1 in
    Buffer.add_char buf c;
    read_digits buf stream
  | [< ' ('('); number = expr; ' (')') ?? "expecting a '(' when reading expr" >] ->
    number

and read_digits buf = parser
  | [< ' ('0' .. '9') as c; stream >] ->
    Buffer.add_char buf c;
    read_digits buf stream
  | [< >] ->
    Buffer.contents buf |> int_of_string

let () =
  "1+2*(3+4)+5;1+2;" |> Stream.of_string |> stmt

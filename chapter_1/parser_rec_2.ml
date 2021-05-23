module L = Lexer_o

let names = [| "t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"; "t7" |]
let cur_name = ref 0
let new_name () =
  let pos = !cur_name in
  if pos >= Array.length names then
    raise (Failure "Expression too complex\n")
  else
    cur_name := pos + 1;
    Array.get names pos

let free_name () =
  let pos = !cur_name in
  if pos = 0 then
    raise (Failure "(Internal error) Name stack underflow\n")
  else
    cur_name := pos - 1

let output = ref ""
let add_output str = 
  output := !output ^ str
let clear_output () =
  output := ""

let legal_lookahead = parser
  | [< ?= [ L.NumOrId _ | L.LeftParent ] >] ->
    ()
  | [< >] ->
    raise (Stream.Error "Syntax error\n")

(* Grammar
 * statements  -> expression ; |-
 *              | expression ; statments
 * expression  -> term expression'
 * expression' -> + term expression'
 *              | epsilon
 * term        -> factor term'
 * term'       -> * factor term'
 *              | epsilon
 * factor      -> num_or_id
 *              | ( expression ) *)

let rec statements = parser
  (* statements  -> expression ; |-
   *              | expression ; statments *)
  
  | [< ' L.Semi; stream >] ->
    (* do another statement *)
    statements stream

  | [< ' L.Eoi >] ->
    let res = !output in
    clear_output ();
    res

  | [< stream >] ->
    let temp_var = new_name () in
    let stream_2 = expression temp_var stream in
    free_name ();

    statements stream_2
    
and expression temp_var stream =
  (* expression  -> term expression' *)

  legal_lookahead stream;

  ignore(term temp_var stream);

  let rec loop = parser
  | [< ' L.Plus >] ->
    let temp_var_2 = new_name () in
    let stream_2 = term temp_var_2 stream in
    add_output (Printf.sprintf "\t%s += %s\n" temp_var temp_var_2);
    free_name ();

    loop stream_2

  | [< stream >] ->
    stream
  in

  loop stream

and term temp_var stream =
  (* term        -> factor term' *)

  legal_lookahead stream;

  ignore(factor temp_var stream);

  let rec loop = parser
  | [< ' L.Times >] ->
    let temp_var_2 = new_name () in
    let stream_2 = factor temp_var_2 stream in
    add_output (Printf.sprintf "\t%s *= %s\n" temp_var temp_var_2);
    free_name ();

    loop stream_2

  | [< stream >] ->
    stream
  in

  loop stream

and factor temp_var = parser
  (* factor      -> num_or_id
   *              | ( expression ) *)

  | [<
    ?= [ L.NumOrId _ | L.LeftParent ];
    stream = parser
    | [< ' L.NumOrId num_or_id; stream >] ->
      (* Print the assignment instruction. The %0.*s conversion
       * is a form of %X.Ys, where X is the field width and Y
       * is the maximum number of characters that will be printed
       * (even if the string is longer). I'm using the %0.*s to
       * print the string because it's not \0 terminated.
       * The field has a default width of 0, but it will grow
       * the size needed to print the string. The ".*" tells
       * printf() to take the maximum-number-of-characters
       * count from the next argument (yyleng). *)
      add_output (Printf.sprintf "\t%s = %s\n" temp_var num_or_id);
      stream
  
    | [<
      ' L.LeftParent;
      e=expression temp_var;
      ' L.RightParent ?? "Mismatched parenthesis\n";
      stream
    >] ->
      stream
  >] ->
    stream

  | [< >] ->
    raise (Stream.Error "Number or identifier expected\n")

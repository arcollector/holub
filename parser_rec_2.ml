module L = Lexer_o

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
    ()

  | [< stream >] ->
    statements (expression stream)
    
and expression stream =
  (* expression  -> term expression' *)

  legal_lookahead stream;

  ignore(term stream);

  let rec loop = parser
  | [< ' L.Plus >] ->
    Printf.printf "Plus\n";
    loop (term stream)

  | [< stream >] ->
    stream
  in

  loop stream

and term stream =
  (* term        -> factor term' *)

  legal_lookahead stream;

  ignore(factor stream);

  let rec loop = parser
  | [< ' L.Times >] ->
    Printf.printf "Times\n";
    loop (factor stream)

  | [< stream >] ->
    stream
  in

  loop stream

and factor = parser
  (* factor      -> num_or_id
   *              | ( expression ) *)

  | [<
    ?= [ L.NumOrId _ | L.LeftParent ];
    stream = parser
    | [< ' L.NumOrId num_or_id; stream >] ->
      Printf.printf "NumOrId: %s\n" num_or_id;
      stream
  
    | [<
      ' L.LeftParent;
      e=expression;
      ' L.RightParent ?? "Mismatched parenthesis\n";
      stream
    >] ->
      stream
  >] ->
    stream

  | [< >] ->
    raise (Stream.Error "Number or identifier expected\n")

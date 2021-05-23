module L = Lexer_o

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

  expression' (term stream)

and expression' = parser
  (* expression' -> + term expression'
  *              | epsilon *)

  | [< ' L.Plus; stream >] ->
    Printf.printf "Plus\n";
    expression' (term stream)

  | [< stream >] ->
    stream

and term stream =
  (* term        -> factor term' *)

  term' (factor stream)

and term' = parser
  (* term'       -> * factor term'
   *              | epsilon *)

  | [< ' L.Times; stream >] ->
    Printf.printf "Times\n";
    term' (factor stream)

  | [< stream >] ->
    stream

and factor = parser
  (* factor      -> num_or_id
   *              | ( expression ) *)

  | [< ' L.NumOrId num_or_id; stream >] ->
    Printf.printf "NumOrId: %s\n" num_or_id;
    stream

  | [<
    ' L.LeftParent;
    e=expression;
    ' L.RightParent ?? "Mismatched parenthesis";
    stream
  >] ->
    stream

  | [< >] ->
    raise (Stream.Error "Number or identifier expected")

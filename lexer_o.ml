type token =
  | Eoi
  | Semi
  | Plus
  | Times
  | LeftParent
  | RightParent
  | NumOrId of string

let rec lexer acc = parser
  | [< ' (' ' | '\t' | '\n'); stream >] -> lexer acc stream

  | [< ' (';'); stream >] -> lexer (Semi::acc) stream
 
  | [< ' ('+'); stream >] -> lexer (Plus::acc) stream

  | [< ' ('*'); stream >] -> lexer (Times::acc) stream

  | [< ' ('('); stream >] -> lexer (LeftParent::acc) stream

  | [< ' (')'); stream >] -> lexer (RightParent::acc) stream

  | [< ' ('a'..'z' | 'A'..'Z' | '0'..'9' as ch); stream >] ->
    let buf = Buffer.create 1 in
    Buffer.add_char buf ch;
    lexer_num_or_id buf stream;
    let num_or_id = Buffer.contents buf in
    lexer (NumOrId num_or_id::acc) stream

  (* this not means EOF, this means that when a not matching
   * char is encounter, the lexering process will ends *)
  | [< >] -> List.rev acc

and lexer_num_or_id buf stream =
  let rec readstream = parser
  | [< ' ('a'..'z' | 'A'..'Z' | '0'..'9' as ch) >] ->
    Buffer.add_char buf ch;
    readstream stream
    
  | [< >] ->
    ()
  in
  readstream stream

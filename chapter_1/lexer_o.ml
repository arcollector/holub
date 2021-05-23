type token =
  | Eoi
  | Semi
  | Plus
  | Times
  | LeftParent
  | RightParent
  | NumOrId of string
  | Kwd of char

let rec lexer = parser
  | [< ' (' ' | '\t' | '\n'); stream >] ->
    lexer stream

  | [< ' (';'); stream >] ->
    [< ' Semi; lexer stream >]
 
  | [< ' ('+'); stream >] ->
    [< ' Plus; lexer stream >]

  | [< ' ('*'); stream >] ->
    [< ' Times; lexer stream >]

  | [< ' ('('); stream >] ->
    [< ' LeftParent; lexer stream >]

  | [< ' (')'); stream >] ->
    [< ' RightParent; lexer stream >]

  | [< ' ('a'..'z' | 'A'..'Z' | '0'..'9' as ch); stream >] ->
    let buf = Buffer.create 1 in
    Buffer.add_char buf ch;
    lexer_num_or_id buf stream

  | [< ' ch; stream >] ->
    [< ' Kwd ch; lexer stream >]

  (* this not means EOF, this means that when a not matching
   * char is encounter, the lexering process will ends *)
  | [< >] ->
    [< ' Eoi >]

and lexer_num_or_id buf = parser
  | [< ' ('a'..'z' | 'A'..'Z' | '0'..'9' as ch); stream >] ->
    Buffer.add_char buf ch;
    lexer_num_or_id buf stream
    
  | [< stream >] ->
    let num_or_id = Buffer.contents buf in
    [< ' NumOrId num_or_id; lexer stream >]

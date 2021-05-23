type token = [
  Eoi
  | Semi
  | Plus
  | Times
  | LeftParent
  | RightParent
  | NumOrId of string
];

value lexer_num_or_id buf stream =
  let rec readstream = parser [
    [: ` ('a'..'z' | 'A'..'Z' | '0'..'9' as ch) :] -> do {
      Buffer.add_char buf ch;
      readstream stream
    }
    | [: :] ->
      ()
  ]
  in
  readstream stream
;

value rec lexer acc = parser [
  [: ` (' ' | '\t' | '\n'); stream :] -> lexer acc stream

  | [: ` (';'); stream :] -> lexer [Semi::acc] stream
 
  | [: ` ('+'); stream :] -> lexer [Plus::acc] stream

  | [: ` ('*'); stream :] -> lexer [Times::acc] stream

  | [: ` ('('); stream :] -> lexer [LeftParent::acc] stream

  | [: ` (')'); stream :] -> lexer [RightParent::acc] stream

  | [: ` ch; stream :] -> do {
    let buf = Buffer.create 1 in
    Buffer.add_char buf ch;
    lexer_num_or_id buf stream;
    let num_or_id = Buffer.contents buf in
    lexer [NumOrId num_or_id::acc] stream
  }
  | [: :] -> List.rev acc
];

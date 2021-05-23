type token =
  (* typeness_variable *)
  | Int
  | Char
  | Float
  | Double
  (* widthness_variable *)
  | Short
  | Long
  (* signness_variable *)
  | Signed
  | Unsigned
  (* lifeness_variable *)
  | Const
  | Volatile

  | Identifier of string
  | Kwd of char

(* ie: volatile unsigned long int x *)
let rec lexer = parser
  | [< ' (' ' | '\t' | '\r' | '\n'); stream >] ->
    [< lexer stream >]

  | [< ' ('a'..'z' | 'A'..'Z' as ch); stream >] ->
    let buf = Buffer.create 1 in
    Buffer.add_char buf ch;
    let identifier = lexer_identifier buf stream in
    [< identifier; lexer stream >]

  | [< ' ch; stream >] ->
    [< ' Kwd ch; lexer stream >]
  
  | [< >] ->
    [< >]

and lexer_identifier buf = parser
  | [< ' ('a'..'z' | 'A'..'Z' | '0'..'9' as ch); stream >] ->
    Buffer.add_char buf ch;
    lexer_identifier buf stream

  | [< >] ->
    let identifier = Buffer.contents buf in
    match identifier with
    | "int" -> [< ' Int >]
    | "char" -> [< ' Char >]
    | "float" -> [< ' Float >]
    | "double" -> [< ' Double >]
    | "short" -> [< ' Short >]
    | "long" -> [< ' Long >]
    | "signed" -> [< ' Signed >]
    | "unsigned" -> [< ' Unsigned >]
    | "const" -> [< ' Const >]
    | "volatile" -> [< ' Volatile >]
    | id -> [< ' Identifier id >]

let rec parser_variable acc = parser
  | [<
    ?= [ Volatile ; Unsigned; Long ];
    ' Volatile; ' Unsigned; ' Long;
    stream
  >] ->
    volatile_unsigned_long acc stream
  | [<
    ?= [ Volatile; Long; Unsigned ];
    ' Volatile; ' Long; ' Unsigned;
    stream 
  >] ->
    volatile_unsigned_long acc stream
  | [<
    ?= [ Long; Volatile; Unsigned ];
    ' Long; ' Volatile; ' Unsigned;
    stream
  >] ->
    volatile_unsigned_long acc stream
  | [< 
    ?= [ Long; Unsigned; Volatile ];
    ' Long; ' Unsigned; ' Volatile;
    stream
  >] ->
    volatile_unsigned_long acc stream
  | [<
    ?= [ Unsigned; Long; Volatile ];
    ' Unsigned; ' Long; ' Volatile;
    stream
  >] ->
    volatile_unsigned_long acc stream
  | [< 
    ?= [ Unsigned; Volatile; Long ];
    ' Unsigned; ' Volatile; ' Long;
    stream
  >] ->
    volatile_unsigned_long acc stream
      
  | [< ' Long; stream >] ->
    parser_variable (acc @ [ "long" ]) stream
  
  | [< ' Int; ' Identifier id ?? "expected identifier" >] ->
    acc @ [ "int"; id ]
  | [< ' Identifier id >] ->
    acc @ [ "int"; id ]

  | [< >] ->
      raise (Stream.Error "Syntax error")

and volatile_unsigned_long acc stream =
  parser_variable (acc @ ["volatile"; "unsigned"; "long"]) stream
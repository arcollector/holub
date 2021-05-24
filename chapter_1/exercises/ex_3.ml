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

  | LeftBracket
  | RightBracket

  | Identifier of string
  | Integer of int

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

  | [< ' ('0'..'9' as ch); stream >] ->
    let buf = Buffer.create 1 in
    Buffer.add_char buf ch;
    let integer = lexer_integer buf stream in
    [< ' Integer integer; lexer stream >]

  | [< ' ('['); stream >] ->
    [< ' LeftBracket ; lexer stream >]
  | [< ' (']'); stream >] ->
    [< ' RightBracket ; lexer stream >]

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

and lexer_integer buf = parser
  | [< ' ('0'..'9' as ch); stream >] ->
    Buffer.add_char buf ch;
    lexer_integer buf stream

  | [< >] ->
    let integer = Buffer.contents buf in
    int_of_string integer

(* ---------------------- *)
(* ---------------------- *)
(* ---------------------- *)

(* What are defaults values ?? *)
module IntVariable = struct
  type t = {
    volatile: bool;
    unsigned: bool;
    long: bool;
    long_long: bool;
    int_: bool;
    identifier: string;
    array: int list;
  }

  let empty = {
    volatile = false;
    unsigned = false;
    long = false;
    long_long = false;
    int_ = false;
    identifier = "";
    array = [];
  }

  let set_config config = function
  | Volatile ->
    if config.volatile then
      raise (Stream.Error "volatile already set\n")
    else
      { config with volatile = true; }
  | Unsigned ->
    if config.unsigned then
      raise (Stream.Error "unsigned already set\n")
    else
      { config with unsigned = true; }
  | Long ->
    if config.long_long then
      raise (Stream.Error "long long already set\n")
    else if config.long && (not config.long_long) then
      { config with long = false; long_long = true }
    else
      { config with long = true; }
  | Int ->
    if config.int_ then
      raise (Stream.Error "int already set\n")
    else
      { config with int_ = true; }
  | Identifier id ->
    if config.identifier <> "" then
      raise (Stream.Error "identifier already set\n")
    else
      { config with identifier = id; }
  | Integer value ->
    { config with array = config.array @ [value]; }
  | _ ->
    raise (Stream.Error "Unsupported keyword\n")

  let rec parse config = parser
  | [< ' Volatile; stream >] ->
    parse (set_config config Volatile) stream

  | [< ' Unsigned; stream >] ->
    parse (set_config config Unsigned) stream

  | [< ' Long; stream >] ->
    parse (set_config config Long) stream

  | [< ' Int; stream >] ->
    parse (set_config config Int) stream

  | [< ' Identifier id; stream >] ->
    parse (set_config config (Identifier id)) stream

  | [<
    ' LeftBracket;
    ' Integer size ?? "Expected array size declaration after '['\n";
    ' RightBracket ?? "Expected ']' after array size declaration\n";
    stream
  >] ->
    parse (set_config config (Integer size)) stream

  | [< >] ->
    config

  let stringify config =
    let rec get_modifiers acc config' =
      match config' with
      | { volatile = true } ->
        get_modifiers ("volatile" :: acc) { config' with volatile = false }
      | { unsigned = true } ->
        get_modifiers ("unsigned" :: acc) { config' with unsigned = false }
      | { long = true } ->
        get_modifiers ("long" :: acc) { config' with long = false }
      | { long_long = true } ->
        get_modifiers ("long" :: "long" :: acc) { config' with long_long = false }
      | _ -> List.rev acc in
  
  let modifiers = String.concat " " (get_modifiers [] config) in
  let variable_name = config.identifier in

  if List.length config.array = 0 then
    variable_name ^ " is an " ^ modifiers ^ " int"
  else
    let size = List.nth config.array 0 |> string_of_int in
    variable_name ^ " is an array of " ^ size ^ " " ^ modifiers ^ " int"

  let print config =
    let list_to_string list =
      "[" ^ (
        List.fold_right (fun x acc -> string_of_int x :: acc) list [] |>
        String.concat ";"
      ) ^ "]" in
      
    "{" ^
      "volatile = " ^ string_of_bool config.volatile ^ ";" ^
      "unsigned = " ^ string_of_bool config.unsigned ^ ";" ^
      "long = " ^ string_of_bool config.long ^ ";" ^
      "long_long = " ^ string_of_bool config.long_long ^ ";" ^
      "int_ = " ^ string_of_bool config.int_ ^ ";" ^
      "identifier = " ^ config.identifier ^ ";" ^
      "array = " ^ list_to_string config.array ^ ";" ^
    "}"

  let cmp a b = 
    a.volatile = b.volatile &&
    a.unsigned = b.unsigned &&
    a.long = b.long &&
    a.long_long = b.long_long &&
    a.int_ = b.int_ &&
    a.identifier = b.identifier &&
    a.array = b.array
end

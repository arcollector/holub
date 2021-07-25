let lexer_stream_to_string stream =
  let rec stringify acc = parser
    | [< ' `Ch ch; >] -> stringify (("Ch of " ^ String.make 1 ch) :: acc) stream
    | [< ' `LeftBracket >] -> stringify ("LeftBracket" :: acc) stream
    | [< ' `CclNegated >] -> stringify ("CclNegated" :: acc) stream
    | [< ' `RightBracket >] -> stringify ("RightBracket" :: acc) stream
    | [< ' `Optional >] ->  stringify ("Optional" :: acc) stream
    | [< ' `OneOrMore >] -> stringify ("OneOrMore" :: acc) stream
    | [< ' `ZeroOrMore >] -> stringify ("ZeroOrMore" :: acc) stream
    | [< ' `Any >] -> stringify ("Any" :: acc) stream
    | [< ' `LeftParen >] -> stringify ("LeftParen" :: acc) stream
    | [< ' `RightParen >] -> stringify ("RightParen" :: acc) stream
    | [< ' `Or >] -> stringify ("Or" :: acc) stream
    | [< >] -> acc in
  stringify [] stream |> List.rev |> String.concat "; "

let lexer stream =
  let inside_ccl     = 0x00001 in
  let outside_ccl    = lnot inside_ccl in
  let negated_ccl    = 0x00010 in
  let unnegated_ccl  = lnot negated_ccl in

  let rec lexer' flags = parser
    | [< ?= [ ('[') ; ('^') ]; ' _; ' _; stream >] ->
      [< ' lexer_special_chars flags `LeftBracket;
        ' lexer_special_chars (flags lor inside_ccl) `CclNegated;
        lexer' (flags lor inside_ccl lor negated_ccl) stream >]

    | [< ' ('['); stream >] ->
      [< ' lexer_special_chars flags `LeftBracket;
        lexer' (flags lor inside_ccl) stream >]

    (* TODO: match especial escaped characters like \d \s *)
    (* TODO: right now we are just ignoreing escaped characters *)
    | [< ' ('\\'); ' ch; stream >] -> 
      [< ' `Ch ch; lexer' flags stream >]

    (* TODO: match double escaped charactees like \\d that would match \d *)
    (* meaning that \\d dont interpret it as <digit> *)
    (* TODO: matach double escapred charrates like \\n that would match \\n *)
    (* meaning that match in a string \\n NOT match the newline \n *)

    | [< ' (']'); stream >] ->
      [< ' lexer_special_chars flags `RightBracket;
        lexer' (flags land outside_ccl land unnegated_ccl) stream >]

    | [< ' ('?'); stream >] ->
      [< ' lexer_special_chars flags `Optional;
        lexer' flags stream >]
    | [< ' ('+'); stream >] ->
      [< ' lexer_special_chars flags `OneOrMore;
        lexer' flags stream >]
    | [< ' ('*'); stream >] ->
      [< ' lexer_special_chars flags `ZeroOrMore;
        lexer' flags stream >]
    | [< ' ('.'); stream >] ->
      [< ' lexer_special_chars flags `Any;
        lexer' flags stream >]
    | [< ' ('('); stream >] ->
      [< ' lexer_special_chars flags `LeftParen;
        lexer' flags stream >]
    | [< ' (')'); stream >] ->
      [< ' lexer_special_chars flags `RightParen;
        lexer' flags stream >]

    | [< ' ('|'); stream >] ->
      [< ' lexer_special_chars flags `Or;
        lexer' flags stream >]

    | [< ' ch; stream >] ->
      [< ' `Ch ch;
        lexer' flags stream >]

    | [< >] ->
      [< >]

  and lexer_special_chars flags type_class =
    let is_inside_ccl = flags land inside_ccl = inside_ccl in
    let is_ccl_negated = flags land negated_ccl = negated_ccl in
    match type_class with
      | `LeftBracket when is_inside_ccl -> `Ch '['
      | `LeftBracket -> `LeftBracket
      | `RightBracket when not is_inside_ccl -> `Ch ']' 
      | `RightBracket -> `RightBracket
      | `CclNegated when is_inside_ccl && not is_ccl_negated -> `CclNegated
      | `CclNegated -> `Ch '^'
      | `Any when is_inside_ccl -> `Ch '.'
      | `Any -> `Any
      | `Optional when is_inside_ccl -> `Ch '?'
      | `Optional -> `Optional
      | `OneOrMore when is_inside_ccl -> `Ch '+'
      | `OneOrMore -> `OneOrMore
      | `ZeroOrMore when is_inside_ccl -> `Ch '*'
      | `ZeroOrMore -> `ZeroOrMore
      | `LeftParen when is_inside_ccl -> `Ch '('
      | `LeftParen -> `LeftParen
      | `RightParen when is_inside_ccl -> `Ch ')'
      | `RightParen -> `RightParen
      | `Or when is_inside_ccl -> `Ch '|'
      | `Or -> `Or in

  lexer' 0 stream
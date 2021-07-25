open OUnit2

module MyRegex = MyRegexLexer

let test_lexer regex_string expected =
  assert_equal
    (expected |> Stream.of_list)
    (regex_string |> Stream.of_string |> MyRegex.lexer)
    ~printer: MyRegex.lexer_stream_to_string;
  ()

let tests =
  "Lexer" >::: [
    "[^0-9.]" >:: (fun _ -> test_lexer "[^0-9.]" [
      `LeftBracket;
      `CclNegated;
      `Ch '0';
      `Ch '-';
      `Ch '9';
      `Ch '.';
      `RightBracket
    ]);

    "[0-9]+(\.[0-9]*)?" >:: (fun _ -> test_lexer "[0-9]+(\.[0-9]*)?" [
      `LeftBracket;
      `Ch '0';
      `Ch '-';
      `Ch '9';
      `RightBracket;
      `OneOrMore;
      `LeftParen;
      `Ch '.';
      `LeftBracket;
      `Ch '0';
      `Ch '-';
      `Ch '9';
      `RightBracket;
      `ZeroOrMore;
      `RightParen;
      `Optional
    ]);

    "[0-9.?+*^].[^^a-z]" >:: (fun _ -> test_lexer "[0-9.?+*^].[^^a-z]" [
      `LeftBracket;
      `Ch '0';
      `Ch '-';
      `Ch '9';
      `Ch '.';
      `Ch '?';
      `Ch '+';
      `Ch '*';
      `Ch '^';
      `RightBracket;
      `Any;
      `LeftBracket;
      `CclNegated;
      `Ch '^';
      `Ch 'a';
      `Ch '-';
      `Ch 'z';
      `RightBracket;
    ]);

    "[\[]\.\+\?\*]" >:: (fun _ -> test_lexer "[\[]\.\+\?\*]" [
      `LeftBracket;
      `Ch '[';
      `RightBracket;
      `Ch '.';
      `Ch '+';
      `Ch '?';
      `Ch '*';
      `Ch ']';
    ]);

    "[[\]\.\+\?\*]" >:: (fun _ -> test_lexer "[[\]\.\+\?\*]" [
      `LeftBracket;
      `Ch '[';
      `Ch ']';
      `Ch '.';
      `Ch '+';
      `Ch '?';
      `Ch '*';
      `RightBracket
    ]);

    "a|b[a|c]" >:: (fun _ -> test_lexer "a|b[a|c]" [
      `Ch 'a';
      `Or;
      `Ch 'b';
      `LeftBracket;
      `Ch 'a';
      `Ch '|';
      `Ch 'c';
      `RightBracket
    ]);
  ] 

let () =
  run_test_tt_main tests

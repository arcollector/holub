open OUnit2

module MyRegex = MyRegexParser

let test_expr regex_string expected =
  assert_equal
    expected
    (regex_string |> Stream.of_string |> MyRegexLexer.lexer |> MyRegex.expr)
    ~printer: MyRegex.nfa_list_to_string;
  ()

let epsilon_nfa = MyRegex.new_nfa Epsilon []

let tests =
  "Expression (Or operator)" >::: [
    "a|b" >:: (fun _ -> test_expr "a|b" [
      { epsilon_nfa with next_2 = Some 3 };

      MyRegex.new_nfa (Char 'a') [];
      { epsilon_nfa with next = Some 2 };

      MyRegex.new_nfa (Char 'b') [];

      epsilon_nfa
    ]);

    "b*|a" >:: (fun _ -> test_expr "b*|a" [
      { epsilon_nfa with next_2 = Some 6 };

      { epsilon_nfa with next_2 = Some 3 };
      MyRegex.new_nfa (Char 'b') [];
      { epsilon_nfa with next_2 = Some (-2) };
      epsilon_nfa;
      { epsilon_nfa with next = Some 2 };

      MyRegex.new_nfa (Char 'a') [];

      epsilon_nfa
    ]);
  ]

let () =
  run_test_tt_main tests

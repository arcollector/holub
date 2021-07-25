open OUnit2

module MyRegex = MyRegexParser

let test_char regex_string expected = 
  assert_equal 
    expected
    (regex_string |> Stream.of_string |> MyRegexLexer.lexer |> MyRegex.term);
  ()

let suite =
  "Char" >::: [
    "a" >:: (fun _ -> test_char "a" [ MyRegex.new_nfa (Char 'a') [] ]);
    "." >:: (fun _ -> test_char "." [ MyRegex.new_nfa Ccl ['^'; '\r'; '\n'] ]);
  ]

let () =
  run_test_tt_main suite
open OUnit2

module MyRegex = MyRegexParser

let test_ccl regex_string expected = 
  assert_equal 
    expected
    (regex_string |> Stream.of_string |> MyRegexLexer.lexer |> MyRegex.term);
  ()

let alpha_ccl = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o';
 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']

let digits_ccl =  ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

let suite =
  "Character class" >::: [
    (* these two are not standard *)
    "[]" >:: (fun _ -> test_ccl "[]" [ MyRegex.new_nfa Ccl [' '] ]);
    "[^]" >:: (fun _ -> test_ccl "[^]" [ MyRegex.new_nfa Ccl ['^'; ' '; '\r'; '\n'] ]);
    
    "[.]" >:: (fun _ -> test_ccl "[.]" [ MyRegex.new_nfa Ccl ['.'] ]);
    "[a-z]" >:: (fun _ -> test_ccl "[a-z]" [ MyRegex.new_nfa Ccl alpha_ccl ]);
    "[abc]" >:: (fun _ -> test_ccl "[abc]" [ MyRegex.new_nfa Ccl ['a'; 'b'; 'c'] ]);
    "[0-9]" >:: (fun _ -> test_ccl "[0-9]" [ MyRegex.new_nfa Ccl digits_ccl ]);
    "[abc0-9]" >:: (fun _ -> test_ccl "[abc0-9]" [ MyRegex.new_nfa Ccl (['a'; 'b'; 'c'] @ digits_ccl) ]);
  ]

let () =
  run_test_tt_main suite
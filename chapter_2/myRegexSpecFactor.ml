open OUnit2

module MyRegex = MyRegexParser

let test_factor regex_string expected = 
  assert_equal
    expected
    (regex_string |> Stream.of_string |> MyRegexLexer.lexer |> MyRegex.factor)
    ~printer: MyRegex.nfa_list_to_string;
  ()

let epsilon_nfa = MyRegex.new_nfa Epsilon []
let digits_ccl =  ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

let tests =
  "Factor" >::: [
    "a+" >:: (fun _ -> test_factor "a+" [
      MyRegex.new_nfa (Char 'a') [];
      { epsilon_nfa with next_2 = Some (-1) }
    ]);
    
    "a?" >:: (fun _ -> test_factor "a?" [
      { epsilon_nfa with next_2 = Some 2 };
      MyRegex.new_nfa (Char 'a') [];
      epsilon_nfa
    ]);

    "a*" >:: (fun _ -> test_factor "a*" [
      { epsilon_nfa with next_2 = Some 3 };
      MyRegex.new_nfa (Char 'a') [];
      { epsilon_nfa with next_2 = Some (-2) };
     epsilon_nfa;
    ]);

    "[0-9]+" >:: (fun _ -> test_factor "[0-9]+" [
      MyRegex.new_nfa Ccl digits_ccl;
      { epsilon_nfa with next_2 = Some (-1) };
    ])
  ]

let () =
  run_test_tt_main tests
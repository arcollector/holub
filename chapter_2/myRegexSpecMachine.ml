open OUnit2

module MyRegex = MyRegexParser

let epsilon_nfa = MyRegex.new_nfa Epsilon []
let none_nfa = { (MyRegex.new_nfa Epsilon []) with next = None }
let digits_ccl =  ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

let test_machine regex_string expected =
  assert_equal
    expected
    (regex_string |> Stream.of_string |> MyRegexLexer.lexer |> MyRegex.machine [])
    ~printer: MyRegex.nfa_list_to_string;
  ()

let tests =
  "Machine" >::: [
    "ab" >:: (fun _ -> test_machine "ab" [
      MyRegex.new_nfa (Char 'a') [];
      MyRegex.new_nfa (Char 'b') [];
      none_nfa
    ]);

    "ab[0-9]+c" >:: (fun _ -> test_machine "ab[0-9]+c" [
      MyRegex.new_nfa (Char 'a') [];
      MyRegex.new_nfa (Char 'b') [];

      MyRegex.new_nfa Ccl digits_ccl;
      { epsilon_nfa with next_2 = Some (-1) };

      MyRegex.new_nfa (Char 'c') [];

      none_nfa
    ]);

    "(abc)*" >:: (fun _ -> test_machine "(abc)*" [
      { epsilon_nfa with next_2 = Some 5 };
      MyRegex.new_nfa (Char 'a') [];
      MyRegex.new_nfa (Char 'b') [];
      MyRegex.new_nfa (Char 'c') [];
      { epsilon_nfa with next_2 = Some (-4) };
      epsilon_nfa;
      none_nfa
    ]);

    "(abc)*|(123)+" >:: (fun _ -> test_machine "(abc)*|(123)+" [
      { epsilon_nfa with next_2 = Some 8 };

      { epsilon_nfa with next_2 = Some 5 };
      MyRegex.new_nfa (Char 'a') [];
      MyRegex.new_nfa (Char 'b') [];
      MyRegex.new_nfa (Char 'c') [];
      { epsilon_nfa with next_2 = Some (-4) };
      epsilon_nfa;
      { epsilon_nfa with next = Some 5 };

      MyRegex.new_nfa (Char '1') [];
      MyRegex.new_nfa (Char '2') [];
      MyRegex.new_nfa (Char '3') [];
      { epsilon_nfa with next_2 = Some (-3) };
      
      epsilon_nfa;

      none_nfa
    ]);

    "[0-9]+" >:: (fun _ -> test_machine "[0-9]+" [
      MyRegex.new_nfa Ccl digits_ccl;
      { epsilon_nfa with next_2 = Some (-1) };
      none_nfa;
    ]);
  ]

let () =
  run_test_tt_main tests

open OUnit2

module MyRegex = MyRegexParser

let epsilon_nfa = MyRegex.new_nfa Epsilon []
(* start -> e or start  -> (D+)?.D ---> e or end -> end *)
(*                  |--->  D.(D+)? ---|                 *)
let nfa_list = [
  epsilon_nfa; (*0*)

  { epsilon_nfa with next_2 = Some 7 }; (*1*)

  { epsilon_nfa with next_2 = Some 3 }; (*2*)
  MyRegex.new_nfa (Char 'D') []; (*3*)
  { epsilon_nfa with next_2 = Some (-1) }; (*4*)
  MyRegex.new_nfa (Char '.') []; (*5*)
  MyRegex.new_nfa (Char 'D') []; (*6*)
  { epsilon_nfa with next = Some 7 }; (*7*)

  MyRegex.new_nfa (Char 'D') []; (*8*)
  MyRegex.new_nfa (Char '.') []; (*9*)
  { epsilon_nfa with next_2 = Some 3 }; (*10*)
  MyRegex.new_nfa (Char 'D') []; (*11*)
  { epsilon_nfa with next_2 = Some (-1) }; (*12*)
  epsilon_nfa; (*13*)

  { epsilon_nfa with next = None }; (*14*)
]

let char_set = ['D'; '.']

let move_tests = 
  "nfa_dfa" >::: [
    "nfa_dfa" >:: (fun _ -> 
      let nfa_dfa = Nfa_to_dfa.NfaToDfa.(make char_set nfa_list |> run [0]) in
      Nfa_to_dfa.NfaToDfa.print nfa_dfa |> print_endline;
      ()
    );
  ]

let () =
  run_test_tt_main ("Nfa to dfa" >::: [
    move_tests;
  ])

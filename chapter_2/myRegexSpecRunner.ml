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

let print_int_list l =
  "[" ^ (
    List.fold_right (fun x acc -> string_of_int x :: acc) l [] |>
    String.concat ";"
  ) ^ "]"
  
let print_e_closure (l, accept) =
  let str = print_int_list l in
  match accept with
  | None -> "(" ^ str ^ ", None)"
  | Some index -> "(" ^ str ^ ", Some " ^ string_of_int index ^ ")"

let e_closure_tests =
  "e closure" >::: [
    "e closure at state [0]" >:: (fun _ -> 
      let e_closure_set = MyRegexRunner.e_closure nfa_list [0] in
      assert_equal ([0;1;2;3;5;8], None) e_closure_set ~printer:print_e_closure
    );

    "e closure at state [4;9]" >:: (fun _ ->
      let e_closure_set = MyRegexRunner.e_closure nfa_list [4;9] in
      assert_equal ([3;4;5;9], None) e_closure_set ~printer:print_e_closure
    );

    "e closure at state [6;10]" >:: (fun _ ->
      let e_closure_set = MyRegexRunner.e_closure nfa_list [6;10] in
      assert_equal ([6;10;11;13;14], Some 14) e_closure_set ~printer:print_e_closure
    );

    "e closure at state [7;12]" >:: (fun _ ->
      let e_closure_set = MyRegexRunner.e_closure nfa_list [7;12] in
      assert_equal ([7;11;12;13;14], Some 14) e_closure_set ~printer:print_e_closure
    );

    "e closure at state [14]" >:: (fun _ ->
      let e_closure_set = MyRegexRunner.e_closure nfa_list [14] in
      assert_equal ([], Some 14) e_closure_set ~printer:print_e_closure
    );

    "e closure when initial state is empty" >:: (fun _ ->
      assert_raises
        MyRegexRunner.GIVEN_STATE_IS_EMPTY
        (fun () -> MyRegexRunner.e_closure nfa_list [])
    );

    "e closure when nfa state is empty" >:: (fun _ ->
      assert_raises
        MyRegexRunner.NFA_STATES_IS_EMPTY
        (fun () -> MyRegexRunner.e_closure [] [0])
    );
  ]

let move_tests = 
  "move" >::: [
    "move with set [0;1;2;3;5;8]" >:: (fun _ -> 
      let next_set = MyRegexRunner.move nfa_list [0;1;2;3;5;8] 'D' in
      assert_equal [4;9] next_set ~printer:print_int_list
    );

    "move with set [3;4;5;9]" >:: (fun _ ->
      let next_set = MyRegexRunner.move nfa_list [3;4;5;9] '.' in
      assert_equal [6;10] next_set ~printer:print_int_list
    );

    "move with set [6;10;11;13;14]" >:: (fun _ ->
      let next_set = MyRegexRunner.move nfa_list [6;10;11;13;14] 'D' in
      assert_equal [7;12] next_set ~printer:print_int_list
    );

    "move with set [7;11;12;13;14]" >:: (fun _ ->
      let next_set = MyRegexRunner.move nfa_list [7;11;12;13;14] '0' in
      assert_equal [] next_set ~printer:print_int_list
    );
  ]

let () =
  run_test_tt_main ("Runner" >::: [
    e_closure_tests;
    move_tests;
  ])

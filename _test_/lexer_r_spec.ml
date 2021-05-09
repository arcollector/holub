open OUnit2
module L = Lexer_r

let lexer_printer = function
  | L.Eoi -> print_string "EOF"
  | L.Semi -> print_string ";"
  | L.Plus -> print_string "+"
  | L.Times -> print_string "*"
  | L.LeftParent -> print_string "("
  | L.RightParent ->  print_string ")"
  | L.NumOrId str  -> print_string str

let tests = "test suite" >::: [
  "221 + 110" >:: (fun _ ->
      let payload = "221 + 110" in
      let r = payload |> Stream.of_string |> L.lexer [] in
      let s = [L.NumOrId "221"; L.Plus; L.NumOrId "110"] in
      assert_equal s r
  );

  "22 * 45" >:: (fun _ ->
      let payload = "22 * 45" in
      let r = payload |> Stream.of_string |> L.lexer [] in
      let s = [L.NumOrId "22"; L.Times; L.NumOrId "45"] in
      assert_equal s r
  );

  "(1 + 2)" >:: (fun _ ->
    let payload = "(1 + 2)" in
    let r = payload |> Stream.of_string |> L.lexer [] in
    let s = [
      L.LeftParent;
      L.NumOrId "1";
      L.Plus;
      L.NumOrId "2";
      L.RightParent
    ] in
    assert_equal s r
  );

  "(1 + (1 + (1 + 2)))" >:: (fun _ ->
    let payload = "(1 + (1 + (1 + 2)))" in
    let r = payload |> Stream.of_string |> L.lexer [] in
    let s = [
      L.LeftParent;
      L.NumOrId "1";
      L.Plus;
      L.LeftParent;
      L.NumOrId "1";
      L.Plus;
      L.LeftParent;
      L.NumOrId "1";
      L.Plus;
      L.NumOrId "2";
      L.RightParent;
      L.RightParent;
      L.RightParent
    ] in
    assert_equal s r
  );

  "1 + 2 * 25 + (10 + 99) * 2" >:: (fun _ ->
      let payload = "1 + 2 * 25 + (10 + 99) * 2" in
      let r = payload |> Stream.of_string |> L.lexer [] in
      let s = [
        L.NumOrId "1";
        L.Plus;
        L.NumOrId "2";
        L.Times;
        L.NumOrId "25";
        L.Plus;
        L.LeftParent;
        L.NumOrId "10";
        L.Plus;
        L.NumOrId "99";
        L.RightParent;
        L.Times;
        L.NumOrId "2"
      ] in
      assert_equal s r
  );

  "multline, tabs and spaces" >:: (fun _ ->
    let payload = "
    2 *
      (
        1 + 1
      )
      *
    2
    " in
    let r = payload |> Stream.of_string |> L.lexer [] in
    let s = [
      L.NumOrId "2";
      L.Times;
      L.LeftParent;
      L.NumOrId "1";
      L.Plus;
      L.NumOrId "1";
      L.RightParent;
      L.Times;
      L.NumOrId "2"
    ] in
    assert_equal s r
  );
]

let _ = run_test_tt_main tests

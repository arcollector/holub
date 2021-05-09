open OUnit2
module L = Lexer_o

let lexer_printer = function
  | L.Eoi -> "EOF"
  | L.Semi -> ";"
  | L.Plus -> "+"
  | L.Times -> "*"
  | L.LeftParent -> "("
  | L.RightParent ->  ")"
  | L.NumOrId str  -> str

let lexer_list_printer = List.fold_left
  (fun acc x -> lexer_printer x ^ acc)
  ""

let tests = "test suite" >::: [
  "221 + 110" >:: (fun _ ->
      let payload = "221 + 110" in
      let r = payload |> Stream.of_string |> L.lexer [] in
      Printf.printf "Length is %d\n" (List.length r);
      begin
        List.iter (fun x -> lexer_printer x |> print_string) r;
        print_newline ()
      end;
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

  "1 2" >:: (fun _ -> 
    let payload = "1 2" in
    let r = payload |> Stream.of_string |> L.lexer [] in
    let s = [
      L.NumOrId "1";
      L.NumOrId "2";
    ] in
    assert_equal s r
  );

  "1 { 1 }" >:: (fun _ ->
    let payload = "1 { 1 }" in
    let r = payload |> Stream.of_string |> L.lexer [] in
    let s = [
      L.NumOrId "1";
    ] in
    assert_equal s r ~printer:lexer_list_printer
  );
]

let _ = run_test_tt_main tests

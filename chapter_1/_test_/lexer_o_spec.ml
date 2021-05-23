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
  | L.Kwd ch -> String.make 1 ch

let lexer_list_printer list =
  let acc = ref "" in
  begin
    Stream.iter
      (fun x -> acc := !acc ^ lexer_printer x; ())
      list
  end;
  !acc

let tests = "test suite" >::: [
  "22 * 45" >:: (fun _ ->
      let payload = "22 * 45" in
      let r = payload |> Stream.of_string |> L.lexer in
      let s = [
        L.NumOrId "22";
        L.Times;
        L.NumOrId "45";
        L.Eoi
      ] |> Stream.of_list in
      assert_equal s r ~printer:lexer_list_printer
  );

  "(1 + 2)" >:: (fun _ ->
    let payload = "(1 + 2)" in
    let r = payload |> Stream.of_string |> L.lexer in
    let s = [
      L.LeftParent;
      L.NumOrId "1";
      L.Plus;
      L.NumOrId "2";
      L.RightParent;
      L.Eoi
    ] |> Stream.of_list in
    assert_equal s r ~printer:lexer_list_printer
  );

  "(1 + (1 + (1 + 2)))" >:: (fun _ ->
    let payload = "(1 + (1 + (1 + 2)))" in
    let r = payload |> Stream.of_string |> L.lexer in
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
      L.RightParent;
      L.Eoi
    ] |> Stream.of_list in
    assert_equal s r ~printer:lexer_list_printer
  );

  "1 + 2 * 25 + (10 + 99) * 2" >:: (fun _ ->
    let payload = "1 + 2 * 25 + (10 + 99) * 2" in
    let r = payload |> Stream.of_string |> L.lexer in
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
      L.NumOrId "2";
      L.Eoi
    ] |> Stream.of_list in
    assert_equal s r ~printer:lexer_list_printer
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
    let r = payload |> Stream.of_string |> L.lexer in
    let s = [
      L.NumOrId "2";
      L.Times;
      L.LeftParent;
      L.NumOrId "1";
      L.Plus;
      L.NumOrId "1";
      L.RightParent;
      L.Times;
      L.NumOrId "2";
      L.Eoi
    ] |> Stream.of_list in
    assert_equal s r ~printer:lexer_list_printer
  );

  "1 2 3" >:: (fun _ -> 
    let payload = "1 2 3" in
    let r = payload |> Stream.of_string |> L.lexer in
    let s = [
      L.NumOrId "1";
      L.NumOrId "2";
      L.NumOrId "3";
      L.Eoi
    ] |> Stream.of_list in
    assert_equal s r ~printer:lexer_list_printer
  );

  "1 {[ 1 ]} ()" >:: (fun _ ->
    let payload = "1 {[ 1 ]} ()" in
    let r = payload |> Stream.of_string |> L.lexer in
    let s = [
      L.NumOrId "1";
      L.Kwd '{';
      L.Kwd '[';
      L.NumOrId "1";
      L.Kwd ']';
      L.Kwd '}';
      L.LeftParent;
      L.RightParent;
      L.Eoi
    ] |> Stream.of_list in
    assert_equal s r ~printer:lexer_list_printer
  );
]

let _ = run_test_tt_main tests

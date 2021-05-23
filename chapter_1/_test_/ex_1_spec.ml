open OUnit2

let lexer_printer = function
  | Ex_1.Int -> "int"
  | Ex_1.Identifier id -> id
  | Ex_1.Kwd ch -> String.make 1 ch
  | _ -> "TODO"

let lexer_list_printer list =
  let acc = ref "" in
  begin
    Stream.iter
      (fun x -> acc := !acc ^ lexer_printer x; ())
      list
  end;
  !acc

let lexer_cmp a b =
  let a_str = lexer_list_printer a in
  let b_str = lexer_list_printer b in
  a_str = b_str

let list_of_string_to_string list =
  List.fold_right (fun elem acc -> elem ^ acc) list ""

let tests = "test suite" >::: [
  "int total lexer" >:: (fun _ ->
      let payload = "int total" in
      let real = payload |> Stream.of_string |> Ex_1.lexer in
      let expected = [
        Ex_1.Int;
        Ex_1.Identifier "total";
      ] |> Stream.of_list in
      assert_equal expected real ~printer:lexer_list_printer ~cmp:lexer_cmp
  );

  "int total parser" >:: (fun _ ->
    let payload = "int total" in
    let real =
      payload |>
      Stream.of_string |>
      Ex_1.lexer |>
      Ex_1.parser_variable []
    in
    let expected = [
      "int";
      "total";
    ] in
    assert_equal expected real
  );

  "unsigned long volatile int x parser" >:: (fun _ ->
    let payload = "unsigned long volatile int x" in
    let real =
      payload |>
      Stream.of_string |>
      Ex_1.lexer |>
      Ex_1.parser_variable []
    in
    let expected = [
      "volatile";
      "unsigned";
      "long";
      "int";
      "x";
    ] in
    assert_equal expected real ~printer:list_of_string_to_string
  );

  "long x lexer" >:: (fun _ ->
    let payload = "long x" in
    let real =
      payload |>
      Stream.of_string |>
      Ex_1.lexer
    in
    let expected = [
      Ex_1.Long;
      Ex_1.Int;
      Ex_1.Identifier "x";
    ] |> Stream.of_list in
    assert_equal expected real ~printer:lexer_list_printer ~cmp:lexer_cmp
  );

  "long x parser" >:: (fun _ ->
    let payload = "long x" in
    let real =
      payload |>
      Stream.of_string |>
      Ex_1.lexer |>
      Ex_1.parser_variable []
    in
    let expected = [
      "long";
      "int";
      "x";
    ] in
    assert_equal expected real ~printer:list_of_string_to_string 
  );
]

let _ = run_test_tt_main tests

open OUnit2

let lexer_to_string = function
  | Ex_3.Long -> "Long"
  | Ex_3.Int -> "Int"
  | Ex_3.Identifier id -> "Identifier " ^ id
  | _ -> "TODO"

let lexter_stream_to_string stream =
  let output = ref "" in
  Stream.iter
    (fun x -> output := !output ^ lexer_to_string x)
    stream;
  !output

let lexter_stream_cmp a b =
  lexter_stream_to_string a = lexter_stream_to_string b

let string_identity a = a

let tests = "test suite" >::: [
  "unsigned long volatile int x parser" >:: (fun _ ->
    let payload = "unsigned long volatile int x" in
    let real =
      payload |>
      Stream.of_string |>
      Ex_3.lexer |>
      Ex_3.IntVariable.(parse empty)
    in
    let expected = {
      Ex_3.IntVariable.empty with
      unsigned = true;
      long = true;
      volatile = true;
      int_ = true;
      identifier = "x";
    } in
    assert_equal expected real
      ~printer:Ex_3.IntVariable.print
      ~cmp:Ex_3.IntVariable.cmp
  );

  "long x[10] lexer" >:: (fun _ ->
    let payload = "long x[10]" in
    let real =
      payload |>
      Stream.of_string |>
      Ex_3.lexer
    in
    let expected = [
      Ex_3.Long;
      Ex_3.Int;
      Ex_3.Identifier "x";
      Ex_3.LeftBracket;
      Ex_3.Integer 10;
      Ex_3.RightBracket;
    ] |> Stream.of_list in
    assert_equal expected real
      ~printer:lexter_stream_to_string
      ~cmp:lexter_stream_cmp
  );

  "long x[10] parser" >:: (fun _ ->
    let payload = "long x[10]" in
    let real =
      payload |>
      Stream.of_string |>
      Ex_3.lexer |>
      Ex_3.IntVariable.(parse empty)
    in
    let expected = {
      Ex_3.IntVariable.empty with
      long = true;
      identifier = "x";
      array = [10];
    } in
    assert_equal expected real
      ~printer:Ex_3.IntVariable.print
      ~cmp:Ex_3.IntVariable.cmp
  );

  "unsigned long volatile int x stringify" >:: (fun _ ->
    let payload = "unsigned long volatile int x" in
    let real =
      payload |>
      Stream.of_string |>
      Ex_3.lexer |>
      Ex_3.IntVariable.(parse empty) |>
      Ex_3.IntVariable.stringify
    in
    let expected = "x is an volatile unsigned long int" in
    assert_equal expected real ~printer:string_identity
  );

  "long x[10] stringify" >:: (fun _ ->
    let payload = "long x[10]" in
    let real =
      payload |>
      Stream.of_string |>
      Ex_3.lexer |>
      Ex_3.IntVariable.(parse empty) |>
      Ex_3.IntVariable.stringify
    in
    let expected = "x is an array of 10 long int" in
    assert_equal expected real ~printer:string_identity
  );
]

let _ = run_test_tt_main tests

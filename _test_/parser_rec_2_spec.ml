open OUnit2
module L = Lexer_o

let tests = "test suite" >::: [
  "22 * 45" >:: (fun _ ->
    let payload = [
      L.NumOrId "22";
      L.Times;
      L.NumOrId "45";
      L.Eoi
    ] |> Stream.of_list in
    Parser_rec_2.statements payload
  );

  "22 + 45 * (154 + 28)" >:: (fun _ ->
    let payload = [
      L.NumOrId "22";
      L.Plus;
      L.NumOrId "45";
      L.Times;
      L.LeftParent;
      L.NumOrId "154";
      L.Plus;
      L.NumOrId "28";
      L.RightParent;
      L.Eoi
    ] |> Stream.of_list in
    Parser_rec_2.statements payload
  );
  
  "22 45" >:: (fun _ ->
    let payload = [
      L.NumOrId "22";
      L.NumOrId "45";
      L.Eoi
    ] |> Stream.of_list in
    Parser_rec_2.statements payload
  );

  "( )" >:: (fun _ ->
    let payload = [
      L.LeftParent;
      L.RightParent;
      L.Eoi
    ] |> Stream.of_list in
    assert_raises
      (Stream.Error "Syntax error\n")
      (fun () -> Parser_rec_2.statements payload)
  );

  "(22" >:: (fun _ ->
    let payload = [
      L.LeftParent;
      L.NumOrId "22";
      L.Eoi
    ] |> Stream.of_list in
    assert_raises
      (Stream.Error "Mismatched parenthesis\n")
      (fun () -> Parser_rec_2.statements payload)
  );

  "22 *" >:: (fun _ ->
    let payload = [
      L.NumOrId "22";
      L.Times;
      L.Eoi
    ] |> Stream.of_list in
    assert_raises
      (Stream.Error "Number or identifier expected\n")
      (fun () -> Parser_rec_2.statements payload)
  );

  "22 +" >:: (fun _ ->
    let payload = [
      L.NumOrId "22";
      L.Plus;
      L.Eoi
    ] |> Stream.of_list in
    assert_raises
      (Stream.Error "Syntax error\n")
      (fun () -> Parser_rec_2.statements payload)
  );
]

let _ = run_test_tt_main tests

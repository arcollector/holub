let egrep regex_string input_string engine =
  let input_string_arr = String.to_seq input_string |> List.of_seq in

  (*
    1: Compile the NFA; initiliaze move() & e_closure()
    2: Create the initial stat, the set of all NFA states that can
      be reached by making epsilon transitions from the NFA start state
    3: Initialize the current state to the start state
  *)

  let expr_lexer = MyRegexLexer.lexer (Stream.of_string regex_string) in
  let nfa_states = MyRegexParser.machine [] expr_lexer in
  let start = [0] in
  let initial_states, _ = MyRegexRunner.e_closure nfa_states start in

  (*
    Now interpret the NFA: the next state is the set of all NFA states that
    can be reached after we've make a transition on the current input
    character from any of the NFA states in the current state. The current
    input line is printed every time an accept state is encountered.
    The machine is reset to the initial state when a failure transition
    is encountered
  *)

  let iterate_non_greedy () =
    let rec iterate' next string_index matched_string_buf =
      if List.length input_string_arr = string_index then
        ()
      else 
        let ch = List.nth input_string_arr string_index in
        Buffer.add_char matched_string_buf ch;
        let current = MyRegexRunner.move nfa_states next ch in
        try
          let next, accept = MyRegexRunner.e_closure nfa_states current in
          begin match accept with
          | Some _ ->
            Buffer.contents matched_string_buf |> print_endline;
            Buffer.clear matched_string_buf
          | None ->
            ()
          end;
          iterate' next (string_index + 1) matched_string_buf
        with MyRegexRunner.GIVEN_STATE_IS_EMPTY ->
          iterate' initial_states (string_index + 1) matched_string_buf
    in
    iterate' initial_states 0 (Buffer.create 1)
  in

  let iterate_greedy () = 
    let print_and_clear buf =
      if Buffer.length buf > 0 then
        begin Buffer.contents buf |> print_endline;
        Buffer.clear buf end
      else () in
    let rec iterate' next string_index matched_string_buf last_accept =
      if List.length input_string_arr = string_index then
        begin if last_accept then print_and_clear matched_string_buf end
      else
        let ch = List.nth input_string_arr string_index in 
        let current = MyRegexRunner.move nfa_states next ch in
        try
          let next, accept = MyRegexRunner.e_closure nfa_states current in
          Buffer.add_char matched_string_buf ch;
          match accept with
          | Some _ ->
            iterate' next (string_index + 1) matched_string_buf true
          | None -> 
            iterate' next (string_index + 1) matched_string_buf false
        with MyRegexRunner.GIVEN_STATE_IS_EMPTY ->
          begin if last_accept then print_and_clear matched_string_buf end;
          iterate' initial_states (string_index + 1) matched_string_buf last_accept
    in
    iterate' initial_states 0 (Buffer.create 1) false
  in

  match engine with
  | `Greedy -> iterate_greedy ()
  | `Lazy -> iterate_non_greedy ()

let () =
  let regex_string = Sys.argv.(1) in
  let input_string = Sys.argv.(2) in
  let engine =
    match Sys.argv.(3) with 
    | "greedy" -> `Greedy
    | _ -> `Lazy in
  egrep regex_string input_string engine
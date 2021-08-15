let rec stmt_list buf = 
  let remember = stmt buf in
  if not_at_end_of_input buf then begin
    stmt_list buf;
    process_statement remember
  end else
    process_statement remember

and stmt buf =
  read buf

and read buf =
  let c = Stream.peek buf in
  Stream.junk buf;
  c

and not_at_end_of_input buf =
  not (try match Stream.empty buf with () -> true
  with _ -> false)

and process_statement = function
  | Some c -> String.make 1 c |> print_endline
  | _ -> ()


let () =
  "ABC" |> Stream.of_string |> stmt_list
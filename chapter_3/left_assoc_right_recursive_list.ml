(*
  stmt_list -> stmt_list smmt | stmt
  stmt -> A | B | C
*)

let input_string = Stream.of_string "ABC"

let read input_string =
  let c = Stream.peek input_string in
  Stream.junk input_string;
  c

let stmt input_string =
  read input_string

let process_statement = function
  | Some remember -> print_endline (String.make 1 remember)
  | None -> ()

let not_at_end_of_input input_string =
  not (try 
    match Stream.empty input_string with () -> true
  with
    _ -> false)

let rec stmt_list input_string =
  let remember = stmt input_string in
  process_statement remember;
  if not_at_end_of_input input_string then stmt_list input_string

let () =
  stmt_list input_string
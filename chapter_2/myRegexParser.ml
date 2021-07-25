type edge = Char of char | Ccl | Epsilon
type nfa = {
  edge: edge;
  next: int option;
  next_2: int option;
  bitset: char list
}

let new_nfa edge bitset = {
  edge = edge;
  next = Some 1;
  next_2 = None;
  bitset = bitset
}

let string_of_nfa nfa = 
  "{" ::
  ("edge: " ^ match nfa.edge with
  | Char ch -> String.make 1 ch
  | Ccl -> "Ccl"
  | Epsilon -> "Epsilon") ::
  ("next: " ^ match nfa.next with
  | Some i -> string_of_int i
  | None -> "None"
  ) :: 
  ("next_2: " ^ match nfa.next_2 with
  | Some i -> string_of_int i
  | None -> "None"
  ) :: 
  ("bitset: " ^ List.fold_left (fun acc x -> acc ^ String.make 1 x) "" nfa.bitset) ::
  "}" ::
  [] |> String.concat " "

let nfa_list_to_string list =
  List.fold_left (fun acc x -> acc @ [string_of_nfa x]) [] list |>
    String.concat ";"

let rec machine acc = parser
  | [< nfa_list = expr; stream >] ->
    machine (acc @ nfa_list) stream
  | [< >] ->
    acc @ [ { new_nfa Epsilon [] with next = None } ]

and expr =
  let parse_expr upper_branch = parser
    (* ┌-->upper_branch-->e-->┐ *)
    (* s                      e *)
    (* └-->lower_branch------>┘ *)
    (*                           -----------------| *)
    (* s--> upper_branch --> e--| lower_branch -> e *)
    (* |---------------------------|                *)
    | [< ' (`Or); stream >] ->
      let lower_branch = expr_concatenate stream in
      let upper_branch_length = List.length upper_branch in
      let s = { new_nfa Epsilon [] with
        next_2 = Some (upper_branch_length + 2) } in
      let lower_branch_length = List.length lower_branch in
      let upper_branch_last_last = { new_nfa Epsilon [] with
        next = Some (lower_branch_length + 1) } in
      let e = new_nfa Epsilon [] in
      [s] @ upper_branch @ [upper_branch_last_last] @ lower_branch @ [e]

    (* not match *)
    | [< >] -> upper_branch in

  parser
  | [< nfa_list = expr_concatenate;
    nfa_list' = parse_expr nfa_list >] -> nfa_list'

and expr_concatenate =
  let parse_expr_concatenate nfa_list = parser
  (* not our job to parse these *)
  | [< ?= [ (`LeftParen | `Or) ] >] -> nfa_list

  (* keep concatenating *)
  | [< nfa_list' = expr_concatenate >] -> nfa_list @ nfa_list'

  (* end of stream *)
  | [< >] -> nfa_list in

  parser
  | [< nfa_list = factor;
    nfa_list' = parse_expr_concatenate nfa_list >] -> nfa_list'

and factor =
  let parse_factor nfa_list = parser
    (* ie:     a?         *)
    (* ε -> nfa_list -> ε *)
    (* └----------------^ *)
    | [< ' (`Optional) >] ->
      let length = List.length nfa_list in
      let first_first = { new_nfa Epsilon [] with
        next_2 = Some (length + 1) } in
      let last_last = new_nfa Epsilon [] in
      [ first_first ] @ nfa_list @ [ last_last ]

    (* ie:  a+       *)
    (* nfa_list -> ε *)
    (*   ^--------<┘ *)
    | [< ' (`OneOrMore) >] ->
      let length = List.length nfa_list in
      let last_last = { new_nfa Epsilon [] with
        next_2 = Some (-length) } in
      nfa_list @ [ last_last ]

    (* ie:     a*              *)
    (* ┌>--------------------┐ *)
    (* ε -> nfa_list -> ε -> ε *)
    (*         ^-------<┘      *)
    | [< ' (`ZeroOrMore) >] ->
      let length = List.length nfa_list in
      let first_first = { new_nfa Epsilon [] with
        next_2 = Some (length + 2) } in
      let last_last = { new_nfa Epsilon [] with
        next_2 = Some (-length -1) } in
      let last_last_last = new_nfa Epsilon [] in
      [ first_first ] @ nfa_list @ [ last_last ] @ [ last_last_last ]
  
    (* there is not + or * or ? present *)
    | [< >] -> nfa_list in

  parser
  | [< nfa_list = term;
    nfa_list' = parse_factor nfa_list >] -> nfa_list'

and term = parser
  (* case ( -> match (expr) *)
  | [< ' (`LeftParen); nfa_list = expr; ' (`RightParen) ?? "parse error: expecting a )\n" >] ->
    nfa_list

  (* case [] -> match only spaces *)
  | [< ?= [ `LeftBracket; `RightBracket ]; ' _; ' _ >] ->
    [ new_nfa Ccl [' '] ]

  (* case [^] -> match all except spaces, newlines, carraige reutnr *)
  | [< ?= [ `LeftBracket; `CclNegated; `RightBracket ]; ' _; ' _; ' _ >] ->
    [ new_nfa Ccl ['^' ; ' ' ; '\r' ; '\n' ] ]

  (* case [^ -> match all except bitset characters *)
  | [< ?= [ `LeftBracket; `CclNegated ]; ' _; ' _; stream >] ->
    [ new_nfa Ccl ('^' :: (dodash [] stream)) ]

  (* case [ -> match all bitset characters *)
  | [<' (`LeftBracket); stream >] ->
    [ new_nfa Ccl (dodash [] stream) ]
    
  (* case . -> match all characters except newlinbes, carriage return *)
  | [< ' (`Any) >] ->
    [ new_nfa Ccl ['^'; '\r'; '\n'] ]

  (* case c -> match a single character *)
  | [<' (`Ch ch) >] ->
    [ new_nfa (Char ch) [] ]

and dodash acc stream =
  let junk_stream n = for _ = 1 to n do Stream.junk stream done in

  (* look for range *)
  match Stream.npeek 3 stream with
  | (`Ch s) :: (`Ch '-') :: (`Ch e) :: _ ->
      junk_stream 3;
      let s' = Char.code s in
      let e' = Char.code e in
      let range = e' - s' + 1 in
      if range < 0 then
        begin Stream.Error ("not valid character class range: " ^
          String.make 1 s ^ "-" ^ String.make 1 e ^ "\n") |> raise end
      else
        let list_of_chars = List.init range (fun i -> Char.unsafe_chr(s' + i)) in
        dodash (acc @ list_of_chars) stream

  (* we are done *)
  | `RightBracket :: _ ->
    junk_stream 1;
    acc

  | (`Ch ch) :: _ ->
    junk_stream 1;
    dodash (acc @ [ch]) stream

  | _ ->
    begin Stream.Error "fatal error at parsing character class" |> raise end

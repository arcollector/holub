module MyRegex = MyRegexParser

module NfaToDfa = struct
  type nfa_dfa = {
    nfa_state : int list;
    dfa_state : int;
    visited : bool;
  }
  type transition = {
    dfa_state: int ;
    c : char ;
    next_dfa_state : int option;
    accept: int option;
  }

  type t = {
    char_set : char list;
    nfa_list : MyRegex.nfa list ;
    nfa_dfa_states : nfa_dfa list ;
    transition_list : transition list ;
  }

  let int_option_to_string = function
    | Some x -> "Some " ^ string_of_int x
    | None -> "None"

  let print_int_list l =
    "[" ^ (
      List.fold_right (fun x acc -> string_of_int x :: acc) l [] |>
      String.concat ";"
    ) ^ "]" 
    
  let print_transition_list l =
    List.fold_right
      (fun x acc ->
        ("{ dfa_state = " ^ string_of_int x.dfa_state ^ ";" ^
          "c = " ^ String.make 1 x.c ^ ";" ^
          "next_dfa_state = " ^ int_option_to_string x.next_dfa_state ^ ";" ^
          "accept = " ^ int_option_to_string x.accept ^ " }") :: acc
      )
      l
      [] |>
    String.concat ";\n"

  let print_nfa_dfa_list l =
    List.fold_right
      (fun x acc ->
        ("{ nfa_state = " ^ print_int_list x.nfa_state ^ ";" ^
          "dfa_state = " ^ string_of_int x.dfa_state ^ ";" ^
          "visited = " ^ string_of_bool x.visited ^ "}") :: acc
      )
      l
      [] |>
    String.concat ";\n"

  let print_closures l =
    List.fold_right
      (fun (closure, accept) acc ->
        ("{ closure = " ^ print_int_list closure ^ ";" ^
          "accept = " ^ int_option_to_string accept ^ " }") :: acc
      )
      l
      [] |>
    String.concat ";\n"

  let print t =
    [ "{ " ;
    "nfa_dfa_states = " ^ print_nfa_dfa_list t.nfa_dfa_states ^ ";" ;
    "transition_list = " ^ print_transition_list t.transition_list ^ ";" ;
    "}" ] |>
    String.concat ";\n"

  let make (char_set: char list) (nfa_list: MyRegex.nfa list) : t = {
    char_set = char_set;
    nfa_list = nfa_list;
    nfa_dfa_states = [];
    transition_list = [];
  }

  let add_nfa_dfa_entry (entry: nfa_dfa) t =
    let pred = fun x -> x.nfa_state = entry.nfa_state in
    match List.exists pred t.nfa_dfa_states with
    | true -> t
    | false -> { t with nfa_dfa_states = t.nfa_dfa_states @ [entry] }

  let add_transition_entry (dfa_state: int) (c: char) (next_dfa_state: int option) (accept: int option)  t =
    { t with transition_list = t.transition_list @ [{
      dfa_state = dfa_state;
      c = c;
      next_dfa_state = next_dfa_state;
      accept = accept;
    }] }

  let get_nfa_dfa_state (e_closure: int list) (c: char) t =
    let nfa_state = MyRegexRunner.move t.nfa_list e_closure c in
    match nfa_state with
    | [] -> None
    | _ ->
      let pred = fun x -> x.nfa_state = nfa_state in
      match List.find_opt pred t.nfa_dfa_states with
      | Some entry -> Some entry
      | None -> Some {
        nfa_state = nfa_state;
        dfa_state = List.length t.nfa_dfa_states + 1;
        visited = false;
      }

  let compute_nfa_dfa_states (e_closure: int list) (accept: int option) (current_dfa_state: int) t =
    List.fold_left
      (fun t' c ->
        match get_nfa_dfa_state e_closure c t' with
        | Some entry ->
          (*print_endline "\t\tmove was successfully";*)
          add_transition_entry current_dfa_state c (Some entry.dfa_state) accept t' |>
          add_nfa_dfa_entry entry
        | None ->
          (*print_endline "\t\tmove was empty";*)
          add_transition_entry current_dfa_state c None accept t'
      )
      t (*acc*)
      t.char_set

  let compute_nfa_dfa_states_for_all_closures (e_closures: (int list * int option) list) (current_dfa_state: int) t =
    List.fold_left2
      (fun t' (e_closure, accept) i ->
        (*"\tcomputando " ^ print_int_list e_closure |> print_endline;
        "\tcon current_dfa_state " ^ string_of_int (current_dfa_state - i) |> print_endline;*)
        compute_nfa_dfa_states
          e_closure
          accept
          (current_dfa_state - i)
          t'
      )
      t (*acc*)
      (List.rev e_closures)
      (List.init (List.length e_closures) (fun x -> x))

  let rec iterate (e_closures: (int list * int option) list) (current_dfa_state: int) t =
    let t = compute_nfa_dfa_states_for_all_closures
      e_closures
      current_dfa_state
      t in
    let e_closures = List.filter
      (fun x -> x.visited = false)
      t.nfa_dfa_states in
    match e_closures with
    | [] -> t
    | _ ->
      (*print_nfa_dfa_list e_closures |> print_endline ; print_endline "----";*)
      let e_closures = List.map
        (fun x -> MyRegexRunner.e_closure t.nfa_list x.nfa_state)
        e_closures in
      (*print_closures e_closures |> print_endline; print_newline ();*)
      let nfa_dfa_states = List.map
        (fun x -> { x with visited = true })
        t.nfa_dfa_states in
      iterate
        e_closures
        (List.length nfa_dfa_states)
        { t with nfa_dfa_states = nfa_dfa_states }

  let run (nfa_state: int list) t =
    iterate [MyRegexRunner.e_closure t.nfa_list nfa_state] 0 t

end

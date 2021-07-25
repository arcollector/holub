module MyRegex = MyRegexParser

(*
  computes the epsilon closure set for the input states
  the output set will contain all states that can be reacgd
  by making epsilon transtions from all NFA states in the input
  set. Returns an empty set if the input set or the closure set
  is empty

  ie advance to next the state if node edge is epsilon
*)

exception NFA_STATES_IS_EMPTY
exception GIVEN_STATE_IS_EMPTY

let e_closure (nfa_list: MyRegex.nfa list) (indexes: int list) : int list * int option =
  if List.length nfa_list = 0 then
    raise NFA_STATES_IS_EMPTY
  else if List.length indexes = 0 then
    raise GIVEN_STATE_IS_EMPTY
  else
    let add_to_set elem set =
      if List.mem elem set then set
      else elem :: set in
    let rec e_closure' indexes' set accept =
      if List.length indexes' = 0 then
        List.sort Stdlib.compare set, accept 
      else
        let hd = List.hd indexes' in
        let tl = List.tl indexes' in
        let nfa = List.nth nfa_list hd in
        if nfa.edge = MyRegex.Epsilon then
          match nfa.next, nfa.next_2 with
          | Some i, Some j ->
            let i' = hd + i in
            let j' = hd + j in
            e_closure'
              (tl @ [i'; j'])
              (add_to_set j' (add_to_set i' (add_to_set hd set)))
              accept
          | Some i, _ ->
            let i' = hd + i in
            e_closure'
              (tl @ [i'])
              (add_to_set i' (add_to_set hd set))
              accept
          (* when None, None it means this is the final state *)
          | None, None ->
            e_closure'
              tl
              set
              (Some hd)
          | _, _ ->
            e_closure'
              tl
              (add_to_set hd set)
              accept
        else
          e_closure'
            tl
            (add_to_set hd set)
            accept
    in
    e_closure' indexes [] None

(*
  Return a set that contains all NFA states that ca be
  reached on "c" from any NFA state in "inp_set"
  Returns NULL if there are no such transitions.
  The inp_set is not modified
*)
let move (nfa_list: MyRegex.nfa list) (indexes: int list) (c: char) : int list =
  let get_next_nfa index acc =
    let nfa = List.nth nfa_list index in
    let is_match =
      match nfa.edge with
      | MyRegex.Char ch ->
        if ch = c then nfa.next else None
      | MyRegex.Ccl ->
        if List.mem c nfa.bitset then nfa.next else None
      | _ ->
        None in
    match is_match with
    | Some nfa -> (index + nfa) :: acc
    | None -> acc in
  List.fold_right get_next_nfa indexes []

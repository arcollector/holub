module NfaToDfa = Nfa_to_dfa.NfaToDfa
module MyRegex = MyRegexParser

let epsilon_nfa = MyRegex.new_nfa Epsilon []
(* start -> e or start  -> (D+)?.D ---> e or end -> end *)
(*                  |--->  D.(D+)? ---|                 *)
let nfa_list = [
  epsilon_nfa; (*0*)

  { epsilon_nfa with next_2 = Some 7 }; (*1*)

  { epsilon_nfa with next_2 = Some 3 }; (*2*)
  MyRegex.new_nfa (Char 'D') []; (*3*)
  { epsilon_nfa with next_2 = Some (-1) }; (*4*)
  MyRegex.new_nfa (Char '.') []; (*5*)
  MyRegex.new_nfa (Char 'D') []; (*6*)
  { epsilon_nfa with next = Some 7 }; (*7*)

  MyRegex.new_nfa (Char 'D') []; (*8*)
  MyRegex.new_nfa (Char '.') []; (*9*)
  { epsilon_nfa with next_2 = Some 3 }; (*10*)
  MyRegex.new_nfa (Char 'D') []; (*11*)
  { epsilon_nfa with next_2 = Some (-1) }; (*12*)
  epsilon_nfa; (*13*)

  { epsilon_nfa with next = None }; (*14*)
]
let char_set = ['D'; '.']

let dfa_minimization (dfa: NfaToDfa.t) =
  let transition_list: NfaToDfa.transition list = dfa.transition_list in
  let g1, g2 = List.partition (fun (x: NfaToDfa.transition) -> x.accept = None) transition_list in
  (*
g1 = { dfa_state = 0;c = D;next_dfa_state = Some 1;accept = None };
{ dfa_state = 0;c = .;next_dfa_state = Some 2;accept = None };
{ dfa_state = 2;c = D;next_dfa_state = Some 3;accept = None };
{ dfa_state = 2;c = .;next_dfa_state = None;accept = None };
{ dfa_state = 1;c = D;next_dfa_state = Some 4;accept = None };
{ dfa_state = 1;c = .;next_dfa_state = Some 5;accept = None };
{ dfa_state = 4;c = D;next_dfa_state = Some 4;accept = None };
{ dfa_state = 4;c = .;next_dfa_state = Some 2;accept = None }

g2 = { dfa_state = 5;c = D;next_dfa_state = Some 6;accept = Some 14 };
{ dfa_state = 5;c = .;next_dfa_state = None;accept = Some 14 };
{ dfa_state = 3;c = D;next_dfa_state = None;accept = Some 14 };
{ dfa_state = 3;c = .;next_dfa_state = None;accept = Some 14 };
{ dfa_state = 6;c = D;next_dfa_state = Some 7;accept = Some 14 };
{ dfa_state = 6;c = .;next_dfa_state = None;accept = Some 14 };
{ dfa_state = 7;c = D;next_dfa_state = Some 7;accept = Some 14 };
{ dfa_state = 7;c = .;next_dfa_state = None;accept = Some 14 }
  *)

  let print_int_option = function
    | Some x -> "Some " ^ string_of_int x
    | None -> "None" in

  let rec find_next (g: NfaToDfa.transition list) (dfa_state: int) i : int option =
    if List.length g = i then None
    else
      let elem = List.nth g i in
      if elem.dfa_state != dfa_state then Some i
      else find_next g dfa_state (i + 1) in

  let rec find_elem_by_c (g: NfaToDfa.transition list) (c: char) (dfa_state: int) i =
    if List.length g = i then None
    else
      let elem = List.nth g i in
      if elem.c = c && elem.dfa_state = dfa_state then Some elem
      else find_elem_by_c g c dfa_state (i + 1) in

  let get_items_by_dfa_state dfa_state g = List.filter (fun (x: NfaToDfa.transition) -> x.dfa_state = dfa_state) g in

  let iterate' g (first : NfaToDfa.transition) (first_index: int) (next : NfaToDfa.transition option) next_index acc =
    match next, next_index with
    | Some next, Some next_index ->
      let rec just_one_partition char_set char_set_index =
        if List.length char_set = char_set_index then
          []
        else
        let c = List.nth char_set char_set_index in
        "visitando: " ^ (String.make 1 c) |> print_endline;
        let first = find_elem_by_c g c first.dfa_state first_index in
        let next = find_elem_by_c g c next.dfa_state next_index in
        match first, next with
        | Some first, Some next ->
          let first_next_index = first.next_dfa_state in
          let next_next_index = next.next_dfa_state in
          NfaToDfa.print_transition_list [first] |> print_endline;
          print_int_option first_next_index |> print_endline;
          NfaToDfa.print_transition_list [next] |> print_endline;
          print_int_option next_next_index |> print_endline;
          begin match first_next_index, next_next_index with
          | Some first_next_index, Some next_next_index ->
            let pred target = (fun (x: NfaToDfa.transition) -> x.dfa_state = target) in
            let where1 = List.exists (pred first_next_index) g in
            let where2 = List.exists (pred next_next_index) g in
            string_of_bool where1 |> print_endline;
            string_of_bool where2 |> print_endline;
            begin
            match where1, where2 with
            | true, false
            | false, true ->
              (* tengo que buscar en g todos los items con
                dfa_state = next.dfa_state y ponerlos todos juntos
              *)
              acc @ (get_items_by_dfa_state next.dfa_state g)
            | false, false ->
              acc @ (get_items_by_dfa_state next.dfa_state g)
            | true, true ->
              just_one_partition char_set (char_set_index + 1)
            end
          | Some first_next_index, None ->
            (* next_next_index apunta a None, se va de este grupo *)
            acc @ (get_items_by_dfa_state next.dfa_state g)
          | _, _->
            just_one_partition char_set (char_set_index + 1) end

        | Some first, None ->
          print_endline "Some first, None";
          NfaToDfa.print_transition_list [first] |> print_endline;
          just_one_partition char_set (char_set_index + 1)
        | None, Some next ->
          print_endline "None, Some next";
          NfaToDfa.print_transition_list [next] |> print_endline;
          acc @ (get_items_by_dfa_state next.dfa_state g)
        | None, None ->
          just_one_partition char_set (char_set_index + 1)
      in
      let new_group = just_one_partition char_set 0 in
      acc @ new_group
    | _, _ -> acc
  in

  let rec iterate (groups: NfaToDfa.transition list list) groups_index =
    if List.length groups = groups_index then
      groups
    else
      let g = List.nth groups groups_index in
      match g with
      | [] ->
        iterate groups (groups_index + 1)
      | _ ->
        let first_index = 0 in
        let first = List.nth g first_index in
        let next_index = find_next g first.dfa_state first_index in
        let next =
          match next_index with
          | Some next_index -> List.nth_opt g next_index
          | None -> None in
        (* si g' es vacio quiere decir que todos
        los elmentos de g son validos *)
        print_endline "voy a analizar este grupo";
        NfaToDfa.print_transition_list g |> print_endline; print_newline ();
        let g' = iterate' g first first_index next next_index [] in
        print_endline "----------------->";
        NfaToDfa.print_transition_list g' |> print_endline; print_newline ();
        let groups = List.filter (fun x -> x != g) groups in
        match g' with
        | [] ->
          iterate ([g] @ groups) (groups_index + 1)
        | _ ->
          (* g'' aquellos que estan en g y no en g' *)
          let g'' = List.filter (fun g_x -> not (List.exists (fun g_x' -> g_x' = g_x) g')) g in
          NfaToDfa.print_transition_list g'' |> print_endline; print_newline ();
          iterate ([g''] @ [g'] @ groups) groups_index
  in

  iterate [g1; g2] 0

let () =
  let nfa_dfa = NfaToDfa.(make char_set nfa_list |> run [0]) in
  print_endline "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&";
  let minimized_dfa = dfa_minimization nfa_dfa in
  minimized_dfa |> List.length |> string_of_int |> print_endline;
  List.iter (fun x -> NfaToDfa.print_transition_list x |> print_endline; print_endline "-!") minimized_dfa;
  ()
module Stack = struct

  exception STACK_IS_EMPTY

  type 'a t =
    | Empty
    | Value of 'a * (unit -> 'a t)

  let empty =
    fun () -> Empty

  let push v acc = 
    fun () -> Value (v, acc)

  let top acc =
    match acc () with
    | Empty -> raise STACK_IS_EMPTY
    | Value (v, _) -> v

  let pop acc = 
    match acc () with
    | Empty -> raise STACK_IS_EMPTY
    | Value (_, next) -> next

  let is_empty acc =
    match acc () with
    | Empty -> true
    | _ -> false

end

let rec print_int acc =
  match Stack.is_empty acc with
  | true -> ()
  | false ->
    Stack.top acc |> string_of_int |> print_endline;
    Stack.pop acc |> print_int

let rec print_char acc =
  match Stack.is_empty acc with
  | true -> ()
  | false ->
    Stack.top acc |> String.make 1 |> print_endline;
    Stack.pop acc |> print_char

let tests () =
  let stack = Stack.(push 1 empty |> push 2 |> push 3 |> pop |> pop |> push 4) in
  print_int stack;
  let stack = Stack.(push 'a' empty |> push 'b' |> push 'c') in
  print_char stack;
  ()

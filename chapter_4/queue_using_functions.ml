module Queue = struct
  exception QUEUE_IS_EMPTY

  type 'a t =
    | Empty
    | Value of 'a * (unit -> 'a t)

  let empty =
    fun () -> Empty
  
  let enqueue v queue = 
    fun () -> Value (v, queue)
  
  let top queue =
    match queue () with
    (* dont iterate if queue is already empty *)
    | Empty -> raise QUEUE_IS_EMPTY
    | Value (v, queue') ->
      (* if next element is empty we reached top element *)
      let rec top' last_v = function
        | Empty -> last_v
        | Value (v, queue') -> queue' () |> top' v in
      queue' () |> top' v
  
  let dequeue queue = 
    match queue () with
    (* dont iterate if queue is already empty *)
    | Empty -> raise QUEUE_IS_EMPTY
    | _ ->
      let rec dequeue' new_queue = function
        | Empty -> new_queue
        | Value (v, queue') ->
          (* is the next element is empty then this v is top element *)
          begin match queue' () with
          (* ignore this top element and terminated recursion *)
          | Empty -> new_queue
          | _ -> enqueue v (dequeue' new_queue (queue' ())) end in
        (* doing this alreday dequeue first element *)
        queue () |> dequeue' empty
  
  let is_empty q =
    match q () with
    | Empty -> true
    | _ -> false

end

let rec print_int q =
  match Queue.is_empty q with
  | true -> ()
  | false ->
    Queue.top q |> string_of_int |> print_endline;
    Queue.dequeue q |> print_int

let rec print_char q =
  match Queue.is_empty q with
  | true -> ()
  | false ->
    Queue.top q |> String.make 1 |> print_endline;
    Queue.dequeue q |> print_char

let tests () =
  let q = Queue.(enqueue 1 empty |>
    enqueue 2 |>
    enqueue 3 |>
    dequeue |>
    dequeue |>
    enqueue 4) in
  q |> print_int;

  let q = Queue.(enqueue 'a' empty
    |> enqueue 'b'
    |> enqueue 'c' 
    |> dequeue) in
  q |> print_char;

  ()

(* Define the tape with some initial input *)
let tape = ref ['1'; '1'; '0'; '1']  (* Initial input on the tape *)
let head = ref 0                     (* Initial head position *)
let state = ref "q0"                 (* Initial state *)

(* Function to determine the action based on the current state and symbol *)
let determine_action state symbol =
  match (state, symbol) with
  | ("q0", '1') -> ("q1", '1', 1)
  | ("q0", '0') -> ("q2", '1', 1)
  | ("q1", '1') -> ("q0", '1', 1)
  | ("q1", '0') -> ("q1", '1', 1)
  | ("q2", '1') -> ("q2", '1', 1)
  | ("q2", '0') -> ("HALT", '0', 0)
  | _ -> ("HALT", '0', 0)

(* Function to run the Turing machine *)
let rec run_machine () =
  if !state <> "HALT" then (
    let symbol = if !head < List.length !tape then List.nth !tape !head else '0' in
    let (new_state, write_symbol, direction) = determine_action !state symbol in
    state := new_state;
    if !head < List.length !tape then
      tape := List.mapi (fun i x -> if i = !head then write_symbol else x) !tape
    else
      tape := !tape @ [write_symbol];
    head := !head + direction;
    Printf.printf "State: %s, Tape: %s, Head: %d\n" !state (String.concat "" (List.map (String.make 1) !tape)) !head;
    run_machine ()
  ) else
    Printf.printf "Final Tape: %s\n" (String.concat "" (List.map (String.make 1) !tape))

(* Start the Turing machine *)
let () = run_machine ()

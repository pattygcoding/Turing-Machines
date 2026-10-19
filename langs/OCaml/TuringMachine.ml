(* The canonical machine: a binary incrementer.  Reading the tape from the
   right, every 1 becomes a 0 (carry); the first 0 becomes a 1 and the carry
   stops; if the carry runs off the tape a new leading 1 is added. *)
let increment bits =
  let rec go = function
    | [] -> [ '1' ]
    | '1' :: rest -> '0' :: go rest
    | _ :: rest -> '1' :: rest
  in
  List.rev (go (List.rev bits))

let read_line () =
  try input_line stdin with End_of_file -> ""

let () =
  let line = read_line () in
  if String.exists (fun character -> character <> '0' && character <> '1') line then
    print_endline "Invalid input: expected a binary string."
  else begin
    let bits = List.of_seq (String.to_seq line) in
    let result = String.of_seq (List.to_seq (increment bits)) in
    print_endline ("Initial tape: " ^ line);
    print_endline ("Final tape: " ^ result)
  end

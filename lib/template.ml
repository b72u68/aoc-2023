(* Read input file *)
let rec read_lines (ic : in_channel) =
  match input_line ic with
  | line -> line :: read_lines ic
  | exception End_of_file -> []

let read_input () = open_in "input/day01.txt" |> read_lines

(* Helper functions *)

(* Solver *)
let part_1 lines = 0
let part_2 lines = 0

let solve () =
  let lines = read_input () in
  Printf.printf "Part 1: %d\n" (part_1 lines);
  Printf.printf "Part 2: %d\n" (part_2 lines)

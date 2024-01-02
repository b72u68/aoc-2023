(* Read input file *)
type race = { t : int; d : int }

let is_number c = '0' <= c && c <= '9'

let get_numbers (line : string) : string list =
  let rec aux i num =
    if i >= String.length line then if num = "" then [] else [ num ]
    else
      let c = line.[i] in
      if is_number c then aux (i + 1) (num ^ String.make 1 c)
      else if num = "" then aux (i + 1) ""
      else num :: aux (i + 1) ""
  in
  aux 0 ""

let process_line (line : string) =
  get_numbers (List.nth (String.split_on_char ':' line) 1)

let rec read_lines (ic : in_channel) =
  match input_line ic with
  | line -> process_line line :: read_lines ic
  | exception End_of_file -> []

let process_lines (part_2 : bool) = function
  | [ time; distance ] ->
      let time =
        if part_2 then [ int_of_string (String.concat "" time) ]
        else List.map int_of_string time
      in
      let distance =
        if part_2 then [ int_of_string (String.concat "" distance) ]
        else List.map int_of_string distance
      in
      List.map2 (fun t d -> { t; d }) time distance
  | _ -> failwith "Invalid input file"

let read_input (filename : string) (part_2 : bool) =
  open_in filename |> read_lines |> process_lines part_2

(* Helper functions *)
let string_of_race r = Printf.sprintf "time=%d; distance=%d" r.t r.d

(* The winning holding time x can be calculated using:
    (x - t/2)^2 < t^2/4 - d
*)
let get_win_records r =
  let rhs = (float_of_int (r.t * r.t) /. 4.) -. float_of_int r.d in
  if rhs < 0. then 0
  else
    let sr = Float.sqrt rhs in
    let h = sr +. (float_of_int r.t /. 2.) in
    let l = max (0. -. sr +. (float_of_int r.t /. 2.)) 0. in
    int_of_float (Float.ceil h) - int_of_float (Float.floor l) - 1

let get_total_win_recs races =
  List.fold_left (fun acc r -> get_win_records r * acc) 1 races

(* Solver *)
let solve (run_test : bool) =
  let fn = if run_test then "input/test06.txt" else "input/day06.txt" in
  let get_races = read_input fn in
  Printf.printf "Part 1: %d\n" (get_total_win_recs (get_races false));
  Printf.printf "Part 2: %d\n" (get_total_win_recs (get_races true))
